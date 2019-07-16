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
        geom_hline(yintercept = 0, color='red') +
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
#' @param dependent_variable dependent_variable
#' @param independent_variables independent_variables
#'
#' @export
rt_regression_get_ind_var_options <- function(model, dependent_variable, independent_variables) {

    # these might not be the same if there were transformations
    options <- unique(colnames(model$model), c(independent_variables))
    # e.g. might have been logged transformed
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

    # need to do this because the regression (lm) automatically removes NAs
    dataset <- dataset[complete.cases(dataset), ]

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

#' plots the proportions with confidence intervals according to prop.test
#'
#' @param numerators numerators
#' @param denominators denominators
#' @param categories categories
#' @param groups vector of groups/categories to plot, seperated by color
#' @param confidence_level the confidence level (e.g. 0.95) passed to prop.test
#' @param show_confidence_values show the high/low confidence values
#' @param axes_flip flip axes
#' @param axis_limits custom limits for the corresponding axis (x if not axes_flip, y if axes_flip)
#' @param text_size text size (proportion value)
#' @param line_size the line size for the error bars
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#' @param x_label label for x-axis
#' @param y_label label for y-axis
#' @param group_name when using `groups`, used for the legend in the plot
#' @param title title
#' @param subtitle subtitle
#' @param caption caption
#'
#' @importFrom magrittr "%>%"
#' @importFrom purrr map2 map_dbl
#' @importFrom scales percent pretty_breaks percent_format
#' @importFrom ggplot2 ggplot aes labs geom_text theme_light theme element_text geom_errorbar geom_point scale_y_continuous scale_color_manual coord_cartesian coord_flip
#' @export
rt_plot_proportions <- function(numerators,
                                denominators,
                                categories,
                                groups=NULL,
                                confidence_level = 0.95,
                                show_confidence_values=TRUE,
                                axes_flip=FALSE,
                                axis_limits=NULL,
                                text_size=4,
                                line_size=0.35,
                                base_size=11,
                                x_label="",
                                y_label="",
                                group_name="",
                                title="",
                                subtitle=NULL,
                                caption=NULL) {

    results <- map2(numerators, denominators, ~ prop.test(x=.x, n=.y, conf.level = confidence_level))

    df <- data.frame(categories=factor(categories, levels=unique(categories), ordered = TRUE),
                              proportions=map_dbl(results, ~ .$estimate),
                              conf_low=map_dbl(results, ~ .$conf.int[1]),
                              conf_high=map_dbl(results, ~ .$conf.int[2]))

    if(is.null(caption)) {

        caption <- paste("\nConfidence Level:", confidence_level)
    }

    if(is.null(groups)) {

        plot_object <- ggplot(df, aes(x=categories, y=proportions, color=categories)) +
            geom_errorbar(aes(x=categories, min=conf_low, max=conf_high, color=categories), size=line_size) +
            geom_point(size=line_size*2) +
            geom_text(aes(label=percent(proportions)), size=text_size, vjust=-0.5, check_overlap = TRUE) +
            scale_y_continuous(breaks=pretty_breaks(10), labels = percent_format()) +
            scale_color_manual(values=rt_get_colors_from_values(df$categories), na.value = '#2A3132') +
            theme_light(base_size = base_size) +
            theme(legend.position = 'none',
                  axis.text.x = element_text(angle = 30, hjust = 1)) +
            labs(x=x_label,
                 y=y_label,
                 title=title,
                 caption=caption)

    } else {

        df$groups <- groups

        plot_object <- ggplot(df, aes(x=categories, y=proportions, color=groups)) +
            geom_errorbar(aes(x=categories, min=conf_low, max=conf_high, color=groups),
                          position=position_dodge(width=0.9),
                          size=line_size) +
            geom_point(size=line_size*2, position=position_dodge(width=0.9)) +
            geom_text(aes(label=percent(proportions)),
                      position=position_dodge(width=0.9),
                      size=text_size, vjust=-0.5, check_overlap = TRUE) +
            scale_y_continuous(breaks=pretty_breaks(10), labels = percent_format()) +
            scale_color_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
            theme_light(base_size = base_size) +
            theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
            labs(x=x_label,
                 y=y_label,
                 title=title,
                 subtitle=subtitle,
                 color=group_name,
                 caption=caption)
    }

    if(axes_flip) {

        plot_object <- plot_object +
            coord_flip(ylim = axis_limits)

    } else if (!is.null(axis_limits)) {

        plot_object <- plot_object +
            coord_cartesian(ylim = axis_limits)
    }

    if(show_confidence_values) {

        if(axes_flip) {

            # bottom / top
            vjust_values <- c(2, -2.5)

        } else {

            # bottom / top
            vjust_values <- c(1.1, -0.5)
        }

        plot_object <- plot_object +
            geom_text(aes(label=percent(conf_low), y=conf_low),
                      position=position_dodge(width=0.9),
                      size=text_size, vjust=vjust_values[1], check_overlap = TRUE) +
            geom_text(aes(label=percent(conf_high), y=conf_high),
                      position=position_dodge(width=0.9),
                      size=text_size, vjust=vjust_values[2], check_overlap = TRUE)
    }

    return (plot_object)
}

#' plots the proportions with confidence intervals according to prop.test
#'
#' @param prop_1 vector with two values c(numerator, denominator)
#' @param prop_2 vector with two values c(numerator, denominator)
#' @param categories categories
#' @param groups vector of groups/categories to plot, seperated by color
#' @param confidence_level the confidence level (e.g. 0.95) passed to prop.test
#' @param show_confidence_values show the high/low confidence values
#' @param axes_flip flip axes
#' @param axis_limits custom limits for the corresponding axis (x if not axes_flip, y if axes_flip)
#' @param text_size text size (proportion value)
#' @param line_size the line size for the error bars
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#' @param x_label label for x-axis
#' @param y_label label for y-axis
#' @param group_name when using `groups`, used for the legend in the plot
#' @param title overwrites default title of "Confidence Intervals of Two Proportions"
#' @param subtitle subtitle
#' @param caption expands on default caption giving p-value
#'
#' @export
rt_plot_2_proportions_test <- function(prop_1,
                                       prop_2,
                                       categories,
                                       groups=NULL,
                                       confidence_level = 0.95,
                                       show_confidence_values=TRUE,
                                       axes_flip=FALSE,
                                       axis_limits=NULL,
                                       text_size=4,
                                       line_size=0.35,
                                       base_size=11,
                                       x_label="",
                                       y_label="",
                                       group_name="",
                                       title="",
                                       subtitle=NULL,
                                       caption=NULL) {

    stopifnot(length(prop_1) == 2)
    stopifnot(length(prop_2) == 2)

    numerators <- c(prop_1[1], prop_2[1])
    denominators <- c(prop_1[2], prop_2[2])

    prop_test_results <- prop.test(numerators, denominators, conf.level = confidence_level)
    stat_sig_message <- ifelse(prop_test_results$p.value <= 1 - confidence_level,
                               paste('"Stat Sig" @', confidence_level,'Conf Level; p-value of', round(prop_test_results$p.value, 3)),
                               paste('NOT "Stat Sig" @', confidence_level,'Conf Level; p-value of', round(prop_test_results$p.value, 3)))

    if(is.null(title) || title == "") {
        title = "Confidence Intervals of Two Proportions"
    }

    final_caption = stat_sig_message

    if(!is.null(caption)) {

        final_caption <- paste0(final_caption, "\n", caption)
    }

    plot_object <- rt_plot_proportions(numerators=numerators,
                                       denominators=denominators,
                                       categories=categories,
                                       groups=groups,
                                       confidence_level=confidence_level,
                                       show_confidence_values=show_confidence_values,
                                       axes_flip=axes_flip,
                                       axis_limits=axis_limits,
                                       text_size=text_size,
                                       line_size=line_size,
                                       base_size=base_size,
                                       x_label=x_label,
                                       y_label=y_label,
                                       group_name=group_name,
                                       title=title,
                                       subtitle=subtitle,
                                       caption=final_caption)

    return (plot_object)
}

