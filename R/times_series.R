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

