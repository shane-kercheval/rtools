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

    results <- data.frame(
        feature=colnames(dataset),
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

    rownames(results) <- NULL

    return (results)
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

    results <- data.frame(
        feature=colnames(dataset),
        non_nulls=apply(dataset, 2, function(x) sum(!is.na(x))),
        nulls=apply(dataset, 2, function(x) sum(is.na(x))),
        perc_nulls=apply(dataset, 2, function(x) round(sum(is.na(x)) / nrow(dataset), 4)),
        top=apply(dataset, 2, function(x) as.character(value_counts(x)[1, 'values'])),
        unique=apply(dataset, 2, function(x) nrow(value_counts(x))),
        perc_unique=apply(dataset, 2, function(x) nrow(value_counts(x)) / nrow(dataset)))


    rownames(results) <- NULL

    return (results)
}

#' returns a matrix containing of correlations
#'
#' @param dataset dataframe containing numberic columns
#' @param corr_threshold any correlations that are <= `corr_threshold` will be set to `NA`. (Default is `0`, so all correlations are shown). Helps to reduce noise.
#' @param p_value_threshold any correlations that have a p-value greater than `p_value_threshold` will be set to `NA` (Default is `1`, so all correlations are shown)
#' @param type type of correlation to perform (Default is `pearson`)
#'
#' @examples
#'
#' library(ggplot2)
#' rt_explore_correlations(iris)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select_if
#' @importFrom Hmisc rcorr
#' @importFrom stats complete.cases
#' @export
rt_explore_correlations <- function(dataset, corr_threshold=0, p_value_threshold=1, type='pearson') {

    data_numeric = dataset %>% select_if(is.numeric)

    rcorr_results = rcorr(as.matrix(data_numeric[complete.cases(data_numeric), ]), type=type)
    correlations = rcorr_results$r
    p_values = rcorr_results$P

    correlations[which(abs(correlations) <= corr_threshold, arr.ind=TRUE)] <- NA # set correlations that are 'lower' than corr_threshold to NA
    correlations[which(p_values > p_value_threshold, arr.ind=TRUE)] <- NA # set correlations that have p_value > `p_value_threshold` to NA (i.e. we only want values who have low p_value (i.e. statistically significant), lower than pvalue threshold)

    # set diagnal and above to NA (it is just duplicated information)
    correlations[upper.tri(correlations, diag = TRUE)] <- NA

    # the first row and the last column will contain ALL NAs, remove
    correlations <- correlations[-1,]
    correlations <- correlations[,-ncol(correlations)]

    return (as.matrix(round(correlations, 2)))
}

#' returns a heatmap of correlations
#'
#' @param dataset dataframe containing numberic columns
#' @param corr_threshold any correlations that are <= `corr_threshold` will be set to `NA`. (Default is `0`, so all correlations are shown). Helps to reduce noise.
#' @param p_value_threshold any correlations that have a p-value greater than `p_value_threshold` will be set to `NA` (Default is `1`, so all correlations are shown)
#' @param type type of correlation to perform (Default is `pearson`)
#'
#' @examples
#'
#' library(ggplot2)
#' rt_explore_correlations(iris)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter mutate
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradientn labs theme element_rect element_text
#' @export
rt_explore_plot_correlations <- function(dataset, corr_threshold=0, p_value_threshold=1, type='pearson') {

    correlations <- rt_explore_correlations(dataset=dataset,
                                            corr_threshold=corr_threshold,
                                            p_value_threshold=p_value_threshold,
                                            type=type)

    column_names <- colnames(correlations)
    row_names <- rownames(correlations)

    melted_correlations <- melt(correlations) %>%
        filter(!is.na(value)) %>%
        mutate(Var1 = factor(Var1, levels = rev(row_names)),
               Var2 = factor(Var2, levels = column_names))

    ggplot(melted_correlations, aes(x=Var2, y=Var1)) +
        geom_tile(aes(fill = value)) +
        geom_text(aes(label = value)) +
        scale_fill_gradientn(colours=c('blue','white','red'),
                             breaks=c(-1, 0, 1),
                             labels=c("Perfect Neg Correlation", "No Correlation", "Perfect Pos Correlation"),
                             limits=c(-1,1)) +
        labs(title = 'Correlations',
             subtitle = paste0('corr_threshold: ', corr_threshold, '; ',
                               'p_value_threshold: ', p_value_threshold, '; ',
                               'type: ', type),
             x = '',
             y = '',
             fill = 'Colors') +
        theme(panel.background = element_rect(fill = 'white', colour = 'white'),
              axis.text.x = element_text(angle = 30, hjust = 1))

}
