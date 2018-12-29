#' determines if a time-series dataset contains a single variable (as opposed to multiple variables)
#'
#' @param x a time series dataset
#'
#' library(fpp2)
#' rt_ts_is_single_variable(a10)
#'
#' @export
rt_ts_is_single_variable <- function(x) {

    classes <- class(x)
    return (length(classes) == 1 && classes == 'ts')
}

#' determines if a time-series dataset contains a single variable (as opposed to multiple variables)
#'
#' @param x a time series dataset
#'
#' library(fpp2)
#' rt_ts_is_multi_variable(melsyd)
#'
#' @export
rt_ts_is_multi_variable <- function(x) {

    classes <- class(x)
    return (any(classes == 'mts') || any(classes == 'msts'))
}

#' returns a forumla string to pass `tslm()``
#'
#' @param dependent_variable name of the depedent variable in the dataset
#' @param independent_variables name of the indepedent variables in the dataset (including "trend" & "season" if applicable)
#' @param interaction_variables not supported yet
#' @param ex_ante_horizon is used when the regression model will be used to forecast future values, `ex_ante_horizon` specifies the number periods (i.e. horizon) that will be forecasted.
#'
#' Ex ante means the forecast will be a "true" forecast and, in this case, will only use lagged values that are lagged far enough back, that we can use them to predict into the future. (and/or trend/season variables)
#' For example, if we want to lag 4 periods (e.g. quarters) ahead, we must use values that are lagged 4 periods behind.
#'
#' Lagged variables must be in the for of `x_lag_y`, where `x` is the original variable name and `y` is the lag number.
#'
#' If `ex_ante_horizon` is set, only variables that are in the form of `x_lag_y` will be included, and additionally, only lagged variables where `y` >= `ex_ante_horizon` will be included.
#'
#' All independent variables are included of `ex_ante_horizon` is `NULL`
#' @examples
#'
#' rt_ts_lm_build_formula(dependent_variable = 'dataset',
#'                        independent_variables = c('trend', 'season'))
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_split
#' @export
rt_ts_lm_build_formula <- function(dependent_variable,
                                   independent_variables=NULL,
                                   interaction_variables=NULL,
                                   ex_ante_horizon=NULL) {
    # ex_ante_horizon specifies the number periods (i.e. horizon) we will be forecasting
    # this ex_ante (i.e. true forecast) implementation requires the lags included in the regression model
    # to be >= the ex_ante_horizon.... e.g. cannot have lag 5 included if we are predicting 10 periods ahead, or lag 9
    # all independent variables are included of ex_ante_horizon is NULL

    if(!is.null(ex_ante_horizon)) {

        #independent_variables <- c('trend', 'season', 'x_lag_1', 'x_lag_2', 'x_lag_3', 'x_lag_4', 'x_lag_5')
        splits <- str_split(independent_variables, '_lag_', simplify=TRUE)
        if(ncol(splits) == 1) { # no matches/lags found

            valid_lags <- rep(FALSE, length(independent_variables))  # all invalid

        } else {

            lag_number <- as.numeric(splits[, 2])
            lag_number[is.na(lag_number)] <- 0  # NA means not a lag
            valid_lags <- lag_number >= ex_ante_horizon
        }

        keep_vars <- independent_variables %in% c('trend', 'season') | valid_lags
        independent_variables <- independent_variables[keep_vars]
    }

    if(is.null(interaction_variables)) {

        interaction_variables_formula <- ''

    } else {

        interaction_variables_formula <- paste(map_chr(interaction_variables, ~ paste(., collapse =' * ')),
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

    return (paste(dependent_variable, '~', independent_variables_formula))
}
