#' Calculates the geometric mean of a vector of numbers
#'
#' @param values vector of numeric values
#' @param na.rm if TRUE then remove NA values, if FALSE then return NA if any NA values exist
#' @param add_subtract since the geometric mean takes the log of the values, a value/log of 0 will cause
#'      problems; `add_subtract` adds the number to the values and then subtracts the value after taking the
#'      mean (but before taking the exponent)
#'
#' @export
rt_geometric_mean <- function(values, na.rm=TRUE, add_subtract=0.0001){
    x <- mean(log(values + add_subtract), na.rm =na.rm) - add_subtract
    if(is.infinite(x)) {
        return (x)
    }
    return (exp(x))
}

#' Builds a (string) formula to pass to lm
#'
#' @param dependent_variable dependent_variable
#' @param independent_variables independent_variables
#' @param interaction_variables list with elements as character vectors. each element is an interaction
#'
#' @importFrom purrr map_chr
#' @export
rt_regression_build_formula <- function(dependent_variable,
                                        independent_variables=NULL,
                                        interaction_variables=NULL) {

    if(is.null(interaction_variables)) {

        interaction_variables_formula <- ''

    } else {

        # I use `*` rather than `:` because of the hierarchical principle ISL pg 89
        # `A*B` tells R's lm to include the main affects (A & B). it is shorthand for `A + B + A:B`
        interaction_variables_formula <- paste(map_chr(interaction_variables, ~ paste(., collapse ='*')),
                                               collapse = ' + ')
    }

    if(is.null(independent_variables) || length(independent_variables) == 0) {

        independent_variables_formula <- interaction_variables_formula

    } else if(is.null(interaction_variables) || length(interaction_variables) == 0) {

        independent_variables_formula <- paste(independent_variables, collapse =' + ')

    } else {

        independent_variables_formula <- paste(interaction_variables_formula,
                                               '+',
                                               paste(independent_variables, collapse =' + '))
    }

    formula <- paste(dependent_variable, '~', independent_variables_formula)

    return (formula)
}

#' Builds a (string) formula to pass to lm
#'
#' @param dataset dataset
#' @param dependent_variable dependent_variable
#' @param independent_variables independent_variables
#' @param interaction_variables list with elements as character vectors. each element is an interaction
#' 
#' @export
rt_regression <- function(dataset,
                          dependent_variable,
                          independent_variables,
                          interaction_variables=NULL) {

    formula <- rt_regression_build_formula(dependent_variable,
                                           independent_variables,
                                           interaction_variables)

    if(is.numeric(dataset[[dependent_variable]])) {

        type <- 'Linear Regression'
        result <- lm(formula, data=dataset, na.action = na.exclude)
        reference <- NULL

    } else {

        if(length(unique(dataset[[dependent_variable]])) == 2) {

            type <- 'Logistic Regression'
            result <- glm(formula, data=dataset, na.action = na.exclude, family=binomial)
            reference <- rownames(contrasts(dataset[, dependent_variable]))[2]

        } else {

            return (NULL)
        }
    }

    return (
        list(rows_excluded=which(!complete.cases(dataset[, independent_variables])),
             type=type,
             formula=formula,
             model=result,
             reference=reference)
    )
}

#' Actual vs. Predicted plot
#'
#' @param model model
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 geom_line aes geom_smooth labs
#' @export
rt_regression_plot_actual_vs_predicted <- function(model) {
    actual_variable <- as.character(model$terms)[2]

    data.frame(actual=model$model[[actual_variable]],
               pred=model$fitted.values) %>%
        rt_explore_plot_scatter(variable='actual', comparison_variable='pred') +
        geom_line(aes(x=actual, y=actual), color='red') +
        geom_smooth(method = 'auto') +
        labs(title="Actual vs Predicted",
             y=paste0("Actual (`", actual_variable,"`)"),
             x="Model Prediction",
             caption="Red line indicates perfect prediction.\nBlue line represets pattern of Prediction vs `", actual_variable,"`")
}

#' Residual vs. Predicted plot
#'
#' @param model model
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 geom_hline geom_smooth labs
#' @export
rt_regression_plot_residual_vs_predicted <- function(model) {
    actual_variable <- as.character(model$terms)[2]

    data.frame(resid=model$residuals,
               pred=model$fitted.values) %>%
        rt_explore_plot_scatter(variable='resid', comparison_variable='pred') +
        geom_hline(yintercept = 0) +
        geom_smooth(method = 'auto') +
        labs(title='Residual vs Predicted (a.k.a Fitted)',
             subtitle = 'Residual = Actual - Prediction; a positive Residual indicates the model is under-predicting.',
             y='Residual',
             x='Model Prediction',
             caption='Red line indicates perfect prediction (no residual).\nBlue line represets pattern of Residual vs Predicted (i.e. pattern that is not captured by the model).')
}

#' Residual vs. Predicted plot
#'
#' @param model model
#' @param original_dataset
#' @param dependent_variable
#'
#' @export
rt_regression_get_ind_var_options <- function(model, original_dataset, dependent_variable) {

    stopifnot(nrow(model$model) == nrow(original_dataset))

    options <- unique(colnames(model$model), colnames(original_dataset))
    dependent_variable_used <- as.character(model$terms)[2]

    return(options %>% rt_remove_val(dependent_variable) %>% rt_remove_val(dependent_variable_used))
}

#' Residual vs. Predicted plot
#'
#' @param model model
#' @param predictor the predictor i.e. variable to use
#' @param dataset the original dataset
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 geom_hline geom_smooth labs
#' @importFrom modelr add_residuals
#' @export
rt_regression_plot_residual_vs_variable <- function(model, predictor, dataset) {

    stopifnot(nrow(model$model) == nrow(dataset))

    transformed_columns <- base::setdiff(colnames(model$model), colnames(dataset))

    if(length(transformed_columns) > 0) {
        dataset <- cbind(dataset, model$model[, transformed_columns])
    }

    dataset <- dataset %>% add_residuals(model)
    if(is.numeric(dataset[[predictor]])) {

        dataset %>%
            rt_explore_plot_scatter(variable='resid', comparison_variable=predictor) +
            # ggplot(aes(x=pred, y=price)) +
            # geom_point(alpha = 0.3) +
            geom_hline(yintercept = 0, color='red') +
            geom_smooth(method = 'auto') +
            labs(title=paste0("Residual vs Predictor (`", predictor,"`)"),
                 subtitle = "Residual = Actual - Prediction; a positive Residual indicates the model is under-predicting.",
                 y="Residual",
                 x=paste0("Predictor (`", predictor,"`)"),
                 caption="Red line indicates perfect prediction (no residual).\nBlue line represets pattern of Residual vs Predicted (i.e. pattern that is not captured by the model).")
    } else {
        dataset %>%
            rt_explore_plot_boxplot(variable='resid', comparison_variable=predictor) +
            labs(title=paste0("Residual vs Predictor (`", predictor,"`)"),
                 subtitle = 'Residual = Actual - Prediction; a positive Residual indicates the model is under-predicting.',
                 y='Residual',
                 x=paste0("Predictor (`", predictor,"`)"))
    }
}
