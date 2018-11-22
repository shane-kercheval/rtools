#' returns whether or not a value is NULL, NA, or NAN
#'
#' @param x a single value to check
#'
#' @examples
#'
#' rt_is_null_na_nan(NA)
#'
#' @export
rt_is_null_na_nan <- function(x) {
    return (is.null(x) || is.na(x) || is.nan(x))
}
