#' returns a count of the values in a vector; the object returned is a dataframe
#'
#' @param values a string vector
#'
#' @examples
#'
#' library(ggplot2)
#' value_counts(iris$Species)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename arrange desc
#' @export
value_counts <- function(values) {

    return (data.frame(table(values)) %>% rename(frequency = Freq) %>% arrange(desc(frequency)))
}

#' returns a dataframe containing summary statistics for numeric columns passsed to `dataset`
#'
#' @param dataset dataframe containing numeric columns
#'
#' @examples
#'
#' library(ggplot2)
#' rt_explore_numeric_summary(iris)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select_if
#' @importFrom moments skewness kurtosis
#' @importFrom stats sd quantile
#' @export
rt_explore_numeric_summary <- function(dataset) {

    dataset <- dataset %>% select_if(is.numeric)

    return (data.frame(
        non_nulls=apply(dataset, 2, function(x) sum(!is.na(x))),
        nulls=apply(dataset, 2, function(x) sum(is.na(x))),
        perc_nulls=apply(dataset, 2, function(x) round(sum(is.na(x)) / nrow(dataset), 4)),
        num_zeros=apply(dataset, 2, function(x) sum(x == 0, na.rm=TRUE)),
        perc_zeros=apply(dataset, 2, function(x) round(sum(x == 0, na.rm=TRUE) / nrow(dataset), 4)),
        mean=apply(dataset, 2, function(x) round(mean(x, na.rm=TRUE), 4)),
        st_dev=apply(dataset, 2, function(x) round(sd(x, na.rm=TRUE), 4)),
        coef_of_var=apply(dataset, 2, function(x) round(sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE), 4)),
        skewness=apply(dataset, 2, function(x) round(skewness(x, na.rm=TRUE), 4)),
        kurtosis=apply(dataset, 2, function(x) round(kurtosis(x, na.rm=TRUE), 4)),
        min=apply(dataset, 2, function(x) min(x, na.rm=TRUE)),
        percentile_10=apply(dataset, 2, function(x) quantile(x, .10, na.rm=TRUE)),
        percentile_25=apply(dataset, 2, function(x) quantile(x, .25, na.rm=TRUE)),
        percentile_50=apply(dataset, 2, function(x) quantile(x, .50, na.rm=TRUE)),
        percentile_75=apply(dataset, 2, function(x) quantile(x, .75, na.rm=TRUE)),
        percentile_90=apply(dataset, 2, function(x) quantile(x, .90, na.rm=TRUE)),
        max=apply(dataset, 2, function(x) max(x, na.rm=TRUE)))
    )
}

#' returns a dataframe containing summary statistics for categoric columns passsed to `dataset`
#'
#' @param dataset dataframe containing categoric columns
#'
#' @examples
#'
#' library(ggplot2)
#' rt_explore_numeric_summary(iris)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select_if
#' @export
rt_explore_categoric_summary <- function(dataset) {

    dataset <- dataset %>% select_if(function(x) !is.numeric(x))

    return (data.frame(
        non_nulls=apply(dataset, 2, function(x) sum(!is.na(x))),
        nulls=apply(dataset, 2, function(x) sum(is.na(x))),
        perc_nulls=apply(dataset, 2, function(x) round(sum(is.na(x)) / nrow(dataset), 4)),
        top=apply(dataset, 2, function(x) as.character(value_counts(x)[1, 'values'])),
        unique=apply(dataset, 2, function(x) nrow(value_counts(x))),
        perc_unique=apply(dataset, 2, function(x) nrow(value_counts(x)) / nrow(dataset))
        )
    )
}
