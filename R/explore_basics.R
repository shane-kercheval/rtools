#' Title
#'
#' Summary
#'
#' @param dataset Explanation of 'dataset'
#'
#' @examples
#'
#' library(ggplot2)
#' explore_numeric_summary(iris)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select_if
#' @importFrom moments skewness kurtosis
#' @importFrom stats sd quantile
#' @export
explore_numeric_summary <- function(dataset) {

    return (data.frame(
        non_nulls=apply(dataset %>% select_if(is.numeric), 2, function(x) sum(!is.na(x))),
        nulls=apply(dataset %>% select_if(is.numeric), 2, function(x) sum(is.na(x))),
        perc_nulls=apply(dataset %>% select_if(is.numeric), 2, function(x) round(sum(is.na(x)) / nrow(dataset), 4)),
        num_zeros=apply(dataset %>% select_if(is.numeric), 2, function(x) sum(x == 0, na.rm=TRUE)),
        perc_zeros=apply(dataset %>% select_if(is.numeric), 2, function(x) round(sum(x == 0, na.rm=TRUE) / nrow(dataset), 4)),
        mean=apply(dataset %>% select_if(is.numeric), 2, function(x) round(mean(x, na.rm=TRUE), 4)),
        st_dev=apply(dataset %>% select_if(is.numeric), 2, function(x) round(sd(x, na.rm=TRUE), 4)),
        coef_of_var=apply(dataset %>% select_if(is.numeric), 2, function(x) round(sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE), 4)),
        skewness=apply(dataset %>% select_if(is.numeric), 2, function(x) round(skewness(x, na.rm=TRUE), 4)),
        kurtosis=apply(dataset %>% select_if(is.numeric), 2, function(x) round(kurtosis(x, na.rm=TRUE), 4)),
        min=apply(dataset %>% select_if(is.numeric), 2, function(x) min(x, na.rm=TRUE)),
        percentile_10=apply(dataset %>% select_if(is.numeric), 2, function(x) quantile(x, .10, na.rm=TRUE)),
        percentile_25=apply(dataset %>% select_if(is.numeric), 2, function(x) quantile(x, .25, na.rm=TRUE)),
        percentile_50=apply(dataset %>% select_if(is.numeric), 2, function(x) quantile(x, .50, na.rm=TRUE)),
        percentile_75=apply(dataset %>% select_if(is.numeric), 2, function(x) quantile(x, .75, na.rm=TRUE)),
        percentile_90=apply(dataset %>% select_if(is.numeric), 2, function(x) quantile(x, .90, na.rm=TRUE)),
        max=apply(dataset %>% select_if(is.numeric), 2, function(x) max(x, na.rm=TRUE)))
    )
}
