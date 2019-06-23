#' returns a count of the values in a vector; the object returned is a dataframe
#'
#' @param values a string vector
#'
#' @examples
#'
#' library(ggplot2)
#' rt_value_counts(iris$Species)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename arrange desc
#' @export
rt_value_counts <- function(values) {

    if(all(is.na(values))) {

        return (NA)
    }

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
        top=apply(dataset, 2, function(x) {

            value_counts <- rt_value_counts(x)
            if(is.null(nrow(value_counts)) || is.na(value_counts)) {
                "NA"
            } else {
                as.character(value_counts[1, 'values'])
            }
        }),
        unique=apply(dataset, 2, function(x) length(unique(x)) ),
        perc_unique=apply(dataset, 2, function(x) length(unique(x)) / nrow(dataset)))


    rownames(results) <- NULL

    return (results)
}

#' returns a matrix containing of correlations
#'
#' @param dataset dataframe containing numberic columns
#' @param corr_threshold any correlations that are <= `corr_threshold` will be set to `NA`. (Default is `0`, so all correlations are shown). Helps to reduce noise.
#' @param p_value_threshold any correlations that have a p-value greater than `p_value_threshold` will be set to `NA` (Default is `1`, so all correlations are shown)
#' @param max_missing_column_perc the max percent of missing values for a column for it to be included.
#'       The default is `0.25` (i.e. `25%`) meaning any column with more than 25% of it's data missing will be
#'       removed.
#'       For example, if you set the value to `0.05` (i.e. `5%` then the any column that had more than 5% of
#'       values missing would be removed.
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
rt_explore_correlations <- function(dataset,
                                    corr_threshold=0,
                                    p_value_threshold=1,
                                    max_missing_column_perc=0.25,
                                    type='pearson') {

    data_numeric <- dataset %>% select_if(is.numeric)

    # remove columns that have > max_missing_column_perc % of data missing
    data_numeric <- data_numeric[, colSums(is.na(data_numeric)) / nrow(data_numeric) <= max_missing_column_perc]

    rcorr_results <- rcorr(as.matrix(data_numeric[complete.cases(data_numeric), ]), type=type)
    correlations <- rcorr_results$r
    p_values <- rcorr_results$P

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
#' @param max_missing_column_perc the max percent of missing values for a column for it to be included.
#'       The default is `0.25` (i.e. `25%`) meaning any column with more than 25% of it's data missing will be
#'       removed.
#'       For example, if you set the value to `0.05` (i.e. `5%` then the any column that had more than 5% of
#'       values missing would be removed.
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @examples
#'
#' library(ggplot2)
#' rt_explore_correlations(iris)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter mutate
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradientn labs theme element_rect element_text theme_classic element_blank element_line
#' @export
rt_explore_plot_correlations <- function(dataset,
                                         corr_threshold=0,
                                         p_value_threshold=1,
                                         type='pearson',
                                         max_missing_column_perc=0.25,
                                         base_size=11) {

    correlations <- rt_explore_correlations(dataset=dataset,
                                            corr_threshold=corr_threshold,
                                            p_value_threshold=p_value_threshold,
                                            type=type,
                                            max_missing_column_perc=max_missing_column_perc)

    column_names <- colnames(correlations)
    row_names <- rownames(correlations)

    melted_correlations <- melt(correlations) %>%
        filter(!is.na(value)) %>%
        mutate(Var1 = factor(Var1, levels = rev(row_names)),
               Var2 = factor(Var2, levels = column_names))

    ggplot(melted_correlations, aes(x=Var2, y=Var1)) +
        geom_tile(aes(fill = value)) +
        geom_text(aes(label = value), check_overlap=TRUE) +
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
        theme_classic(base_size = base_size) +
        theme(line = element_blank(),
              axis.ticks.x = element_line(color="black"),
              axis.ticks.y = element_line(color="black"),
              panel.background = element_rect(fill = 'white', colour = 'white'),
              axis.text.x = element_text(angle = 30, hjust = 1))

}

#' returns either a *count* of the unique values of `variable` if `sum_by_variable` is NULL, otherwise it *sums* the
#' variable represented by `sum_by_variable` across (i.e. grouped by) `variable`
#'
#' If `multi_value_delimiter` is not NULL, then it counts all the values found after it splits/separates the
#'      variable by the delimiter. If `sum_by_variable` is NULL, it counts the values and the denominator for
#'      the `percent` column returned is the total number of records. If `sum_by_variable` is not NULL, then
#'      when multiple values are found, each value is weighted by the value found in `sum_by_variable`, and
#'      the denominator for the `percent` column returned is the `sum` of `sum_by_variable` (before the values
#'      are split).
#'
#' @param dataset dataframe containing numberic columns
#' @param variable the variable (e.g. factor) to get unique values from
#' @param sum_by_variable the numeric variable to sum
#' @param multi_value_delimiter if the variable contains multiple values (e.g. "A", "A, B", ...) then setting
#'      this variable to the delimiter will cause the function to count seperate values
#'
#' @examples
#'
#' library(ggplot2)
#' rt_explore_value_totals(dataset=iris, variable='Species')
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr sym count mutate group_by ungroup summarise rename arrange desc
#' @importFrom stringr str_split
#' @importFrom purrr flatten map2 map_dbl map_chr
#' @export
rt_explore_value_totals <- function(dataset, variable, sum_by_variable=NULL, multi_value_delimiter=NULL) {

    symbol_variable <- sym(variable)  # because we are using string variables

    # temp <- data.frame(values=unlist(str_split(dataset[, variable], multi_value_delimiter, simplify=FALSE)),
    #                    weight=1)
    values <- dataset[, variable]


    if(is.null(sum_by_variable)) {

        weights <- 1
        count_column_name <- 'count'
        denominator <- nrow(dataset)

    } else{

        weights <- dataset[, sum_by_variable]
        count_column_name <- 'sum'
        denominator <- sum(weights, na.rm = TRUE)
    }

    if(is.null(multi_value_delimiter)) {

        temp <- data.frame(value = values,
                           weight = weights,
                           stringsAsFactors = FALSE)
    }else {

        temp <- map2(values, weights,
                      function(value, weight) {
                          split_values <- unlist(str_split(value, multi_value_delimiter, simplify = FALSE))
                          map2(split_values, rep(weight, length(split_values)),
                               function(val, val_weight)
                                   c(val, val_weight))
                      })
        temp <- flatten(temp)
        temp <- data.frame(value = map_chr(temp, ~.[[1]]),
                           weight = map_dbl(temp, ~as.double(.[[2]])),
                           stringsAsFactors = FALSE)

    }
    # sometimes, column names given to data.frame aren't being retained
    colnames(temp) <- c('value', 'weight')

    # this will give warning if there are NA's, but we want to keep NAs, no use fct_explicit_na so that, e.g.
    # we can use `na.value = '#2A3132'` with scale_fill_manual
    totals <- suppressWarnings(temp %>%
                                   group_by(value) %>%
                                   summarise(count = sum(weight, na.rm = TRUE)) %>%
                                   ungroup() %>%
                                   mutate(percent = count / denominator) %>%
                                   arrange(desc(count), value))

    if(is.factor(values)) {

        if(is.null(multi_value_delimiter)) {

            totals <- totals %>%
                mutate(value = factor(value, levels=levels(values)))

        } else {
            # if we are delimiting, then the original factors aren't valid because they contain multi-value
            # levels
            totals <- totals %>%
                mutate(value = factor(value, levels=sort(unique(as.character(totals$value)))))
        }
    }

    colnames(totals) <- c(variable, count_column_name, 'percent')

    return (as.data.frame(totals))
}

#' returns a barchart of the unique value counts for a given dataset/variable, grouped by an additional variable
#'
#' If `multi_value_delimiter` is not NULL, then it counts all the values found after it splits/separates the
#'      variable by the delimiter. If `sum_by_variable` is NULL, it counts the values and the denominator for
#'      the `percent` column returned is the total number of records. If `sum_by_variable` is not NULL, then
#'      when multiple values are found, each value is weighted by the value found in `sum_by_variable`, and
#'      the denominator for the `percent` column returned is the `sum` of `sum_by_variable` (before the values
#'      are split).
#'
#' Currently only works when using only `variable` (not `comparison_variable`)
#'
#' @param dataset dataframe containing numberic columns
#' @param variable the variable (e.g. factor) to get unique values from
#' @param comparison_variable the additional variable to group by; must be a string/factor column
#' @param sum_by_variable the numeric variable to sum
#' @param order_by_count if TRUE (the default) it will plot the bars from most to least frequent, otherwise it will order by the original factor levels if applicable
#' @param show_variable_totals if TRUE (the default) the graph will display the totals for the variable
#' @param show_comparison_totals if TRUE (the default) the graph will display the totals for the comparison_variable
#' @param show_dual_axes show a secondary axis for the Count or Sum
#' @param stacked_comparison rather than side-by-side bars for the comparison variable, the bars are stacked within the main variable
#' @param multi_value_delimiter if the variable contains multiple values (e.g. "A", "A, B", ...) then setting
#'      this variable to the delimiter will cause the function to count seperate values

#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise mutate ungroup arrange n count desc
#' @importFrom scales percent_format percent pretty_breaks format_format
#' @importFrom ggplot2 ggplot aes aes geom_bar scale_y_continuous geom_text labs theme_light theme element_text position_fill position_dodge scale_fill_manual sec_axis
#' @export
rt_explore_plot_value_totals <- function(dataset,
                                         variable,
                                         comparison_variable=NULL,
                                         sum_by_variable=NULL,
                                         order_by_count=TRUE,
                                         show_variable_totals=TRUE,
                                         show_comparison_totals=TRUE,
                                         show_dual_axes=FALSE,
                                         stacked_comparison=FALSE,
                                         multi_value_delimiter=NULL,
                                         base_size=11) {

    symbol_variable <- sym(variable)  # because we are using string variables

    if(is.null(sum_by_variable)) {

        plot_title <- paste0('Value Counts - `', variable, '`')
        plot_y_axis_label <- 'Percent of Dataset Containing Value'
        plot_y_second_axis_label <- "Count"
        groups_by_variable <- rt_explore_value_totals(dataset=dataset,
                                                      variable=variable,
                                                      multi_value_delimiter=multi_value_delimiter) %>%
            rename(total=count)

    } else {

        plot_title <- paste0('Sum of `', sum_by_variable, '` by `', variable, '`')
        plot_y_axis_label <- 'Percent of Total Amount'
        plot_y_second_axis_label <- paste0('Sum of `', sum_by_variable, '`')
        groups_by_variable <- rt_explore_value_totals(dataset=dataset,
                                                      variable=variable,
                                                      sum_by_variable=sum_by_variable,
                                                      multi_value_delimiter=multi_value_delimiter) %>%
            rename(total=sum)
    }

    if(rt_is_null_na_nan(comparison_variable)) {

        if(order_by_count) {

            groups_by_variable[, variable] <- factor(groups_by_variable[, variable],
                                                     levels = groups_by_variable[, variable])
        }

        unique_values_plot <- groups_by_variable %>%
            ggplot(aes(x=!!symbol_variable, y=percent, fill=!!symbol_variable)) +
                geom_bar(stat = 'identity', alpha=0.75)

        if(show_dual_axes) {

            unique_values_plot <- unique_values_plot +
                scale_y_continuous(breaks=pretty_breaks(), labels = percent_format(),
                                   sec.axis = sec_axis(~.*sum(groups_by_variable$total),
                                                       breaks=pretty_breaks(),
                                                       labels = format_format(big.mark=",",
                                                                              preserve.width="none",
                                                                              digits=4,
                                                                              scientific=FALSE),
                                                       name=plot_y_second_axis_label))
        } else {

            unique_values_plot <- unique_values_plot +
                scale_y_continuous(breaks=pretty_breaks(), labels = percent_format())
        }

        if(show_variable_totals) {

            unique_values_plot <- unique_values_plot +
                geom_text(aes(label = percent(percent), y = percent), vjust=-0.25, check_overlap=TRUE) +
                geom_text(aes(label = prettyNum(total, big.mark=",", preserve.width="none", digits=4,
                                                scientific=FALSE), y = percent),
                          vjust=1.25, check_overlap=TRUE)
        }

        return (
            unique_values_plot +
                labs(title=plot_title,
                     y=plot_y_axis_label,
                     x=variable) +
                scale_fill_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
                theme_light(base_size = base_size) +
                theme(legend.position = 'none',
                      axis.text.x = element_text(angle = 30, hjust = 1))
            )

    } else {

        symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables
        plot_title <- paste0(plot_title, ' against `', comparison_variable, '`')

        if(is.null(sum_by_variable)) {
            # this will give warning if there are NA's, but we want to keep NAs, no use fct_explicit_na so that, e.g.
            # we can use `na.value = '#2A3132'` with scale_fill_manual
            groups_by_both <- suppressWarnings(dataset %>%
                    group_by(!!symbol_variable, !!symbol_comparison_variable) %>%
                    summarise(total = n(), actual_percent=total / nrow(dataset)))

        } else {

            symbol_sum_by <- sym(sum_by_variable)  # because we are using string variables
            # this will give warning if there are NA's, but we want to keep NAs, no use fct_explicit_na so that, e.g.
            # we can use `na.value = '#2A3132'` with scale_fill_manual
            groups_by_both <- suppressWarnings(dataset %>%
                    group_by(!!symbol_variable, !!symbol_comparison_variable) %>%
                    summarise(total = sum(!!symbol_sum_by, na.rm=TRUE)) %>%
                    ungroup() %>%
                    mutate(actual_percent=total / sum(total, na.rm = TRUE)))
        }
        # this will give warning if there are NA's, but we want to keep NAs, no use fct_explicit_na so that, e.g.
        # we can use `na.value = '#2A3132'` with scale_fill_manual
        groups_by_both <- suppressWarnings(as.data.frame(groups_by_both %>%
                group_by(!!symbol_variable) %>%
                mutate(group_percent = total / sum(total, na.rm=TRUE)) %>%
                ungroup() %>%
                arrange(!!symbol_variable, !!symbol_comparison_variable)))

        if(order_by_count) {

            groups_by_variable[, variable] <- factor(groups_by_variable[, variable],
                                                     levels = as.character(groups_by_variable[, variable]))
            groups_by_both[, variable] <- factor(groups_by_both[, variable],
                                                     levels = as.character(groups_by_variable[, variable]))

            comparison_order <- as.character((dataset %>% count(!!symbol_comparison_variable) %>% arrange(desc(n)))[[comparison_variable]])
            groups_by_both[, comparison_variable] <- factor(groups_by_both[, comparison_variable],
                                                     levels = comparison_order)
        }

        if(stacked_comparison) {

            comparison_position <- position_fill(reverse = TRUE)

        } else {

            comparison_position <- position_dodge(width = 0.9)
        }

        # create the plot
        unique_values_plot <- ggplot() +
            geom_bar(data = groups_by_variable,
                     aes(x = !!symbol_variable, y = percent),
                     stat = 'identity',
                     position = 'dodge',
                     alpha = 0.3) +
            geom_bar(data = groups_by_both,
                     aes(x = !!symbol_variable,
                         y = actual_percent,
                         fill = !!symbol_comparison_variable),
                     stat = 'identity',
                     position = comparison_position)

        if(show_dual_axes && !stacked_comparison) {

            unique_values_plot <- unique_values_plot +
                scale_y_continuous(breaks=pretty_breaks(), labels = percent_format(),
                                   sec.axis = sec_axis(~.*sum(groups_by_variable$total),
                                                       breaks=pretty_breaks(),
                                                       labels = format_format(big.mark=",",
                                                                              preserve.width="none",
                                                                              digits=4,
                                                                              scientific=FALSE),
                                                       name=plot_y_second_axis_label))
        } else {

            unique_values_plot <- unique_values_plot +
                scale_y_continuous(breaks=pretty_breaks(), labels = percent_format())
        }

        # we will only show variable totals if show_variable_totals and the variable values aren't filled
        #(i.e. all 100%)
        if(show_variable_totals && !stacked_comparison) {

            unique_values_plot <- unique_values_plot +
                geom_text(data = groups_by_variable,
                          aes(x=!!symbol_variable, label = percent(percent), y = percent),
                          vjust=-1.5, check_overlap=TRUE) +
                geom_text(data = groups_by_variable,
                          aes(x=!!symbol_variable, label = prettyNum(total, big.mark=",", preserve.width="none", digits=4, scientific=FALSE), y = percent),
                          vjust=-0.25, check_overlap=TRUE)
        }

        if(show_comparison_totals && !stacked_comparison) {

            unique_values_plot <- unique_values_plot +
                geom_text(data = groups_by_both,
                          aes(x = !!symbol_variable,
                              y = actual_percent,
                              label = prettyNum(total, big.mark=",", preserve.width="none", digits=4, scientific=FALSE),
                              group = !!symbol_comparison_variable),
                          position = comparison_position,
                          vjust=-0.25, check_overlap=TRUE) +
                geom_text(data = groups_by_both,
                          aes(x = !!symbol_variable,
                              y = actual_percent,
                              label = percent(group_percent),
                              group = !!symbol_comparison_variable),
                          position = comparison_position,
                          vjust=1.25, check_overlap=TRUE)

        } else if (show_comparison_totals && stacked_comparison) {
            #in this case, we don't want to show the totals of the main variable (they are all at 100%)
            unique_values_plot <- unique_values_plot +
                geom_text(data = groups_by_both,
                          aes(x = !!symbol_variable,
                              y = group_percent,
                              label = percent(group_percent),
                              group = !!symbol_comparison_variable),
                          position = position_fill(reverse = TRUE, vjust = .5),
                          check_overlap=TRUE)
        }

        return (unique_values_plot +
            labs(title = plot_title,
                 y=plot_y_axis_label,
                 fill = comparison_variable,
                 x = variable) +
            scale_fill_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
            theme_light(base_size = base_size) +
            theme(#legend.position = 'none',
                  axis.text.x = element_text(angle = 30, hjust = 1)))
    }
}

#' returns a boxplot of `variable`, potentally grouped by `comparison_variable`
#'
#' @param dataset dataframe containing numberic columns
#' @param variable the variable from which to create a boxplot
#' @param comparison_variable the additional variable to group by; must be a string/factor column
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes geom_boxplot scale_x_discrete xlab ylab theme_light theme element_text coord_cartesian scale_color_manual geom_text
#' @importFrom dplyr group_by summarise n
#' @importFrom scales pretty_breaks format_format
#' @export
rt_explore_plot_boxplot <- function(dataset,
                                    variable,
                                    comparison_variable=NULL,
                                    y_zoom_min=NULL,
                                    y_zoom_max=NULL,
                                    base_size=11) {

    symbol_variable <- sym(variable)  # because we are using string variables

    if(rt_is_null_na_nan(comparison_variable)) {

        boxplot_plot <- ggplot(dataset, aes(y=!!symbol_variable, group=1)) +
            geom_boxplot() +
            scale_y_continuous(breaks=pretty_breaks(), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
            scale_x_discrete(breaks = NULL) +
            labs(caption = paste("\n", prettyNum(sum(!is.na(dataset[[variable]])),
                                                 big.mark=",", preserve.width="none", digits=4, scientific=FALSE),
                                 'Non-NA Values'),
                 y=variable,
                 x='') +
            theme_light(base_size = base_size)

    } else {

        symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables

            aggregations <- dataset %>%
                group_by(!!symbol_comparison_variable) %>%
                summarise(median = round(median(!!symbol_variable, na.rm = TRUE), 4),
                          count = n())

        boxplot_plot <- ggplot(dataset,
                               aes(y=!!symbol_variable,
                                   x=!!symbol_comparison_variable,
                                   color=!!symbol_comparison_variable)) +
            scale_y_continuous(breaks=pretty_breaks(), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
            geom_boxplot() +
            geom_text(data = aggregations,
                      mapping = aes(y=median,
                                    x=!!symbol_comparison_variable,
                                    label = prettyNum(median, big.mark=",", preserve.width="none", digits=4, scientific=FALSE)),
                      vjust=-0.5,
                      check_overlap = TRUE) +
            geom_text(data = aggregations,
                      mapping = aes(y=median,
                                    x=!!symbol_comparison_variable,
                                    label = prettyNum(count, big.mark=",", preserve.width="none", digits=4, scientific=FALSE)),
                      vjust=1.3,
                      check_overlap = TRUE) +
            scale_color_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
            labs(caption="\n# above median line is the median value, # below median line is the size of the group.",
                 x=comparison_variable,
                 y=variable) +
            theme_light(base_size = base_size) +
            theme(legend.position = 'none',
                  axis.text.x = element_text(angle = 30, hjust = 1))
    }

    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[, variable], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[, variable], na.rm = TRUE)
        }

        boxplot_plot <- boxplot_plot +
            coord_cartesian(ylim = c(y_zoom_min, y_zoom_max))
    }

    return (boxplot_plot)
}

#' returns a histogram of `variable`
#'
#' @param dataset dataframe containing numberic columns
#' @param variable the variable from which to create a histogram
#' @param comparison_variable (optional) the additional variable to group by; must be a string/factor column
#' @param density use geom_density rather than geom_freqpoly for comparison_variable
#' @param num_bins the number of bins that the histogram will use
#' @param x_zoom_min adjust (i.e. zoom in) to the x-axis; sets the minimum x-value for the adjustment
#' @param x_zoom_max adjust (i.e. zoom in) to the x-axis; sets the maximum x-value for the adjustment
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes aes geom_histogram geom_freqpoly geom_density labs theme_light coord_cartesian scale_color_manual
#' @export
rt_explore_plot_histogram <- function(dataset,
                                      variable,
                                      comparison_variable=NULL,
                                      density=FALSE,
                                      num_bins=30,
                                      x_zoom_min=NULL,
                                      x_zoom_max=NULL,
                                      base_size=11) {

    symbol_variable <- sym(variable)  # because we are using string variables

    # if no comparison_variable, then do histogram with density; otherwise do histogram with group
    if(is.null(comparison_variable)) {

        histogram_plot <- ggplot(dataset, aes(x=!!symbol_variable)) +
            geom_histogram(bins = num_bins) +
            geom_density(aes(y = ..count..), col='red') +
            labs(title=paste0('Histogram & Density Plot of `', variable, '`'),
                 x=variable) +
            theme_light(base_size = base_size)
    } else {

        symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables
        histogram_plot <- ggplot(dataset, aes(x=!!symbol_variable,
                                              color=!!symbol_comparison_variable))

        if(density) {

            histogram_plot <- histogram_plot +
                geom_density()

        } else {

            histogram_plot <- histogram_plot +
                geom_freqpoly(binwidth= 100 / num_bins)
        }

        histogram_plot <- histogram_plot +
            scale_color_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
            labs(title=paste0('Distribution of `', variable, '` by `', comparison_variable, '`'),
                 x=variable,
                 color=comparison_variable) +
            theme_light(base_size = base_size)
    }

    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(x_zoom_min) || !rt_is_null_na_nan(x_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(x_zoom_min)) {

            x_zoom_min <- min(dataset[, variable], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(x_zoom_max)) {

            x_zoom_max <- max(dataset[, variable], na.rm = TRUE)
        }

        histogram_plot <- histogram_plot +
            coord_cartesian(xlim = c(x_zoom_min, x_zoom_max))
    }

    return (histogram_plot)
}

#' returns a scatterplot of `variable` (numeric, x-axis) compared against `comparison_variable` (y-axis)
#'
#' @param dataset dataframe
#' @param variable a numeric variable (y-axis)
#' @param comparison_variable the additional numeric variable (x-axis)
#' @param color_variable an optional variable (categoric or numeric) that allows points to be colored based on the values of the variable
#' @param size_variable an optional variable (numeric) that allows points to be sized based on the values of the variable
#' @param alpha controls transparency
#' @param jitter enables/disables jittering
#' @param x_zoom_min adjust (i.e. zoom in) to the x-axis; sets the minimum x-value for the adjustment
#' @param x_zoom_max adjust (i.e. zoom in) to the x-axis; sets the maximum x-value for the adjustment
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes geom_point theme_light coord_cartesian geom_jitter position_jitter scale_y_continuous scale_color_manual
#' @importFrom scales pretty_breaks format_format
#' @export
rt_explore_plot_scatter <- function(dataset,
                                    variable,
                                    comparison_variable,
                                    color_variable=NULL,
                                    size_variable=NULL,
                                    alpha=0.3,
                                    jitter=FALSE,
                                    x_zoom_min=NULL,
                                    x_zoom_max=NULL,
                                    y_zoom_min=NULL,
                                    y_zoom_max=NULL,
                                    base_size=11) {

    symbol_variable <- sym(variable)  # because we are using string variables
    symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables

    symbol_if_not_null <- function(x) {
        if (is.null(x)) {

            return (NULL)

        } else {

            return (sym(x))
        }
    }

    symbol_color_variable <- symbol_if_not_null(color_variable)
    symbol_size_variable <- symbol_if_not_null(size_variable)

    scatter_plot <- ggplot(dataset, aes(x=!!symbol_comparison_variable,
                                        y=!!symbol_variable,
                                        color=!!symbol_color_variable,
                                        size=!!symbol_size_variable))

    if(jitter) {

        scatter_plot <- scatter_plot + geom_jitter(alpha=alpha, position=position_jitter(seed=42))

    } else {

        scatter_plot <- scatter_plot + geom_point(alpha=alpha)
    }

    scatter_plot <- scatter_plot +
        scale_y_continuous(breaks=pretty_breaks(), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
        theme_light(base_size = base_size) +
        labs(x=comparison_variable,
             y=variable)

    if(!is.null(color_variable) &&
            (is.character(dataset[, color_variable]) || is.factor(dataset[, color_variable]))) {

        scatter_plot <- scatter_plot + scale_color_manual(values=rt_colors())
    }

    x_zooms <- NULL
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(x_zoom_min) || !rt_is_null_na_nan(x_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(x_zoom_min)) {

            x_zoom_min <- min(dataset[, comparison_variable], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(x_zoom_max)) {

            x_zoom_max <- max(dataset[, comparison_variable], na.rm = TRUE)
        }

        x_zooms <- c(x_zoom_min, x_zoom_max)
    }

    y_zooms <- NULL
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[, variable], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[, variable], na.rm = TRUE)
        }

        y_zooms <- c(y_zoom_min, y_zoom_max)
    }
    scatter_plot <- scatter_plot +
        coord_cartesian(xlim=x_zooms, ylim = y_zooms)

    return (scatter_plot)
}

#' returns a eih
#'
#' @param dataset dataframe
#' @param variable a numeric variable (y-axis)
#' @param comparison_variable the additional numeric variable (x-axis) that will be grouped
#' @param aggregation_function the aggregation function that gives, for example,
#'      the mean of the `variable` grouped by the `comparison_variable`
#'      if no aggregation function is supplied the boxplots are showed for each grouped `comparison_variable`
#'      value.
#' @param aggregation_function_name the friendly name of the `aggregation_function`
#' @param aggregation_count_minimum the minimum number of samples to included for each `comparison_variable`
#'      value. The default is 30, so any value of `comparison_variable` that didn't occur at least 30 times
#'      would be removed. We need at least 30, otherwise when we bootstrap resample e.g. with a group that has
#'      1 sample we'd pull e.g. 100 random samples of the same value
#' @param show_resampled_confidence_interval when `aggregation_function` is not null, then we have the option
#'      to show a confidence interval based on resampling
#' @param show_points the show points
#' @param show_labels show the labels
#' @param x_zoom_min adjust (i.e. zoom in) to the x-axis; sets the minimum x-value for the adjustment
#' @param x_zoom_max adjust (i.e. zoom in) to the x-axis; sets the maximum x-value for the adjustment
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by filter n ungroup summarise rename
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_point theme_light coord_cartesian geom_jitter position_jitter scale_y_continuous scale_color_manual geom_text labs theme element_text geom_line scale_x_continuous expand_limits geom_ribbon
#' @importFrom scales pretty_breaks format_format
#' @importFrom rsample bootstraps
#' @importFrom tidyr unnest
#' @export
rt_explore_plot_aggregate_2_numerics <- function(dataset,
                                                 variable,
                                                 comparison_variable,
                                                 aggregation_function=NULL,
                                                 aggregation_function_name=NULL,
                                                 aggregation_count_minimum=30,
                                                 show_resampled_confidence_interval=FALSE,
                                                 show_points=FALSE,
                                                 show_labels=FALSE,
                                                 x_zoom_min=NULL,
                                                 x_zoom_max=NULL,
                                                 y_zoom_min=NULL,
                                                 y_zoom_max=NULL,
                                                 base_size=11) {

    symbol_variable <- sym(variable)  # because we are using string variables
    symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables

    t <- dataset %>%
        group_by(!!symbol_comparison_variable) %>%
        filter(n() >= aggregation_count_minimum) %>%
        ungroup()

    if(is.null(aggregation_function)) {
        # if no aggregation function then show boxplots
        aggregations <- t %>%
            group_by(!!symbol_comparison_variable) %>%
            summarise(median = round(median(!!symbol_variable, na.rm = TRUE), 4),
                      count = n())

        aggregate_plot <- ggplot(t, aes(x=!!symbol_comparison_variable, y=!!symbol_variable, group=!!symbol_comparison_variable)) +
            geom_boxplot() +
            geom_text(data = aggregations,
                      mapping = aes(y=median,
                                    x=!!symbol_comparison_variable,
                                    label = prettyNum(median, big.mark=",", preserve.width="none", digits=4, scientific=FALSE)),
                      vjust=-0.5,
                      check_overlap = TRUE) +
            geom_text(data = aggregations,
                      mapping = aes(y=median,
                                    x=!!symbol_comparison_variable,
                                    label = prettyNum(count, big.mark=",", preserve.width="none", digits=4, scientific=FALSE)),
                      vjust=1.3,
                      check_overlap = TRUE) +
            scale_x_continuous(breaks=pretty_breaks(), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
            labs(title=paste0("`", variable, "` grouped by `", comparison_variable, "`"),
                 caption="\n# above median line is the median value, # below median line is the size of the group.",
                 x=comparison_variable,
                 y=variable) +
            theme(legend.position = 'none',
                  axis.text.x = element_text(angle = 30, hjust = 1))

    } else {

        t <- t %>%
            group_by(!!symbol_comparison_variable) %>%
            summarise(agg_variable = aggregation_function(!!symbol_variable))
        aggregate_plot <- ggplot(t, aes(x=!!symbol_comparison_variable)) +
            geom_line(aes(y=agg_variable)) +
            scale_x_continuous(breaks=pretty_breaks(), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
            expand_limits(y=0) +
            labs(title = paste0(aggregation_function_name, " of `", variable, "` by `", comparison_variable, "`"),
                 y = paste0(aggregation_function_name, " of `", variable, "`"),
                 x = comparison_variable)

        if(show_points) {
            aggregate_plot <- aggregate_plot +
                geom_point(aes(y=agg_variable))
        }

        if(show_labels) {
            aggregate_plot <- aggregate_plot +
                geom_text(aes(y=agg_variable, label = prettyNum(agg_variable, big.mark=",", preserve.width="none", digits=4, scientific=FALSE)), check_overlap = TRUE, vjust=-0.5)
        }

        if(show_resampled_confidence_interval) {
            # https://www.youtube.com/watch?v=zjWm__nFLXI
            #install.packages('rsample')
            set.seed(42)
            bs <- dataset[, c(comparison_variable, variable)] %>%
                rename(value = !!symbol_variable) %>%
                #count(!!symbol_comparison_variable)
                group_by(!!symbol_comparison_variable) %>%
                filter(n() >= aggregation_count_minimum) %>%
                ungroup() %>%
                # filter(!!symbol_comparison_variable == 5) %>%
                bootstraps(times=1000)
            #pull(splits) %>% pluck(1)
            #bs %>% mutate(statistic = map_dbl(splits, ~ rt_geometric_mean(as.data.frame(.)$amount)))
            bs <- bs %>%
                unnest(map(splits, as.data.frame)) %>%
                group_by(!!symbol_comparison_variable, id) %>%
                summarise(average_value = aggregation_function(value)) %>%
                summarise(bootstrap_low = quantile(average_value, 0.025),
                          bootstrap_high = quantile(average_value, 0.975))
            aggregate_plot <- aggregate_plot +
                geom_ribbon(data=bs, aes(ymin=bootstrap_low, ymax=bootstrap_high), alpha=0.25)
        }
    }

    aggregate_plot <- aggregate_plot +
        scale_y_continuous(breaks=pretty_breaks(), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
        theme_light(base_size = base_size)

    x_zooms <- NULL
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(x_zoom_min) || !rt_is_null_na_nan(x_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(x_zoom_min)) {

            x_zoom_min <- min(dataset[, comparison_variable], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(x_zoom_max)) {

            x_zoom_max <- max(dataset[, comparison_variable], na.rm = TRUE)
        }

        x_zooms <- c(x_zoom_min, x_zoom_max)
    }

    y_zooms <- NULL
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[, variable], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[, variable], na.rm = TRUE)
        }

        y_zooms <- c(y_zoom_min, y_zoom_max)
    }
    aggregate_plot <- aggregate_plot +
        coord_cartesian(xlim=x_zooms, ylim = y_zooms)

    return (aggregate_plot)
}

#' returns a time-series plot
#'
#' @param dataset dataframe
#' @param variable a variable (x-axis) that is a date type
#' @param comparison_variable the additional numeric variable (y-axis)
#' @param comparison_function if a comparison variable is supplied, a function must be given so the plot knows how to graph it (e.g. sum, mean, median)
#' @param comparison_function_name name of the function so that it can be plotted on the Y-axis label
#' @param color_variable an optional variable (categoric) that seperates the time series
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param show_points if TRUE adds points to the graph
#' @param show_labels if TRUE adds labels to each point
#' @param date_floor options are e.g. "week", "month", "quarter"
#' @param date_break_format format of date breaks
#' @param date_breaks_width the date breaks for x axis, values correspond to ggplot scale_x_date e.g. "1 month", "1 week"
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr count group_by summarise rename
#' @importFrom ggplot2 ggplot aes labs geom_line expand_limits theme_light theme element_text coord_cartesian scale_color_manual geom_text geom_point scale_x_date
#' @importFrom lubridate floor_date
#' @importFrom stringr str_to_title str_trim
#' @importFrom scales date_format pretty_breaks format_format
#' @export
rt_explore_plot_time_series <- function(dataset,
                                        variable,
                                        comparison_variable=NULL,
                                        comparison_function=NULL,
                                        comparison_function_name=NULL,
                                        color_variable=NULL,
                                        y_zoom_min=NULL,
                                        y_zoom_max=NULL,
                                        show_points=FALSE,
                                        show_labels=FALSE,
                                        date_floor=NULL,
                                        date_break_format=NULL,
                                        date_breaks_width=NULL,
                                        base_size=11) {

    # if using a comparison variable, we must also have a function and function name
    stopifnot(!(!is.null(comparison_variable) &&
        (is.null(comparison_function) || is.null(comparison_function_name))))

    symbol_variable <- sym(variable)  # because we are using string variables

    dataset[, variable] <- as.Date(dataset[, variable])

    symbol_if_not_null <- function(x) {
        if (is.null(x)) {

            return (NULL)

        } else {

            return (sym(x))
        }
    }

    title_context <- NULL
    x_label_context <- NULL
    if(!is.null(date_floor)) {
        title_context <- paste0(str_to_title(date_floor), "ly")
        if(title_context == "Dayly") {
            title_context <- "Daily"
        }
        x_label_context <- paste0("(", date_floor, ")")

        dataset[, variable] <- floor_date(dataset[, variable], unit=date_floor, week_start = 1)

        if(is.null(date_breaks_width)) {

            if (date_floor == 'quarter') {

                date_breaks_width <- NULL
            } else {
                date_breaks_width <- paste('1', date_floor)
            }

        }

        if(is.null(date_break_format)) {
            if(date_floor == 'week') {

                date_break_format <- '%Y-%W'

            } else if (date_floor == 'month') {

                date_break_format <- '%Y-%m'

            } else if (date_floor == 'quarter') {

                date_break_format <- '%Y-%m'

            } else if (date_floor == 'year') {

                date_break_format <- '%Y'

            } else {

                date_break_format <- '%Y-%m-%d'
            }
        }
    }

    # need to do this after date_floor because if date_floor is not NULL and date_breaks_width is NULL date_breaks_width
    # will get set
    if(is.null(date_breaks_width)) {

        date_breaks_width <- '1 month'
    }
    if(is.null(date_break_format)) {

        date_break_format <- '%Y-%m-%d'
    }

    sym_comparison_variable <- symbol_if_not_null(comparison_variable)
    sym_color_variable <- symbol_if_not_null(color_variable)

    if(is.null(sym_comparison_variable)) {

        if(is.null(sym_color_variable)) {

            dataset <- dataset %>% count(!!symbol_variable) %>% rename(total=n)

        } else {

            dataset <- dataset %>% count(!!symbol_variable, !!sym_color_variable) %>% rename(total=n)
        }
        ggplot_object <- dataset %>%
            ggplot(aes(x=!!symbol_variable, y=total, color=!!sym_color_variable)) +
            scale_color_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
            labs(title=str_trim(paste(title_context, 'Count of Records')),
                 x=str_trim(paste(variable, x_label_context)),
                 y='Count')

    } else {
        if(is.null(sym_color_variable)) {
            dataset <- dataset %>%
                group_by(!!symbol_variable) %>%
                summarise(total=comparison_function(!!sym_comparison_variable))

        } else {

            dataset <- dataset %>%
                group_by(!!symbol_variable, !!sym_color_variable) %>%
                summarise(total=comparison_function(!!sym_comparison_variable))
        }
        ggplot_object <- dataset %>%
            ggplot(aes(x=!!symbol_variable, y=total, color=!!sym_color_variable)) +
            scale_color_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
            labs(title=str_trim(paste(title_context,
                                      paste(comparison_function_name,
                                            'of', comparison_variable,
                                            'by', variable))),
                 x=str_trim(paste(variable, x_label_context)),
                 y=paste(comparison_function_name, comparison_variable))
    }
    ggplot_object <- ggplot_object +
        geom_line() +
        scale_y_continuous(breaks=pretty_breaks(), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
        scale_x_date(labels = date_format(date_break_format), breaks=date_breaks_width) +
        expand_limits(y=0) +
        theme_light(base_size = base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

    if(show_points) {
        ggplot_object <- ggplot_object +
            geom_point()
    }

    if(show_labels) {
        ggplot_object <- ggplot_object +
            geom_text(aes(label = prettyNum(total, big.mark=",", preserve.width="none", digits=4, scientific=FALSE)), check_overlap = TRUE, vjust=-0.5)
    }

    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[, 'total'], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[, 'total'], na.rm = TRUE)
        }

        ggplot_object <- ggplot_object +
            coord_cartesian(ylim = c(y_zoom_min, y_zoom_max))
    }

    return (ggplot_object)
}

#' returns a funnel plot
#'
#' @param step_names the names of the steps, top down
#' @param step_values the values of the steps, top down; the first/top value is assumed to be 100 percent
#' @param title the title
#' @param subtitle the subtitle
#' @param caption the caption
#' @param proportionate controls the shape of the funnel
#'      when FALSE, the width consistently decreases with each step
#'      when TRUE, the width decreases proportionate to the value of each step
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr arrange desc mutate group_by ungroup
#' @importFrom scales percent pretty_breaks format_format
#' @importFrom ggplot2 ggplot aes labs geom_polygon geom_text scale_fill_manual theme_classic theme element_blank element_text
#' @export
rt_funnel_plot <- function(step_names, step_values, title="", subtitle="", caption="", proportionate=FALSE) {

    step_names <- factor(step_names, levels = step_names)
    df_steps <- data.frame(Step=rep(step_names, 4),
                           value=rep(step_values, 4))
    df_steps <- df_steps %>% arrange(desc(Step))

    num_steps <- length(step_names)
    if(proportionate) {
        width_perc <- rev(step_values) / step_values[1] * 100
        width_perc <- c(width_perc[1], width_perc)

    } else {

        width_perc <- seq(5, 100, length.out = num_steps + 1)
    }


    df <- data.frame(x=NULL, y=NULL)
    for(step in 1:num_steps){
        df <- rbind(df,
                    data.frame(x=c(-width_perc[step],
                                   width_perc[step],
                                   width_perc[step + 1],
                                   -width_perc[step + 1]),
                               y=c(rep(step - 1, 2), rep(step, 2))))
    }

    df <- cbind(df, df_steps)
    df <- df %>%
        mutate(conversion_rate = percent(value / step_values[1]),
               value = prettyNum(value, big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) %>%
        group_by(Step) %>%
        mutate(label_y = mean(y)) %>%
        # # need to make sure the label is only associated with one point so it's not overlapping
        # mutate(value = ifelse(x == max(x) & y == max(y), as.character(value), NA)) %>%
        # mutate(conversion_rate = ifelse(x == max(x) & y == max(y), conversion_rate, NA)) %>%
        ungroup() %>%
        mutate(label = ifelse(is.na(conversion_rate),
                              value,
                              paste0(value, " (", conversion_rate, ")")))

    ggplot(df, aes(x = x, y = y)) +
        geom_polygon(aes(fill = Step, group = Step), alpha=0.3) +
        #geom_text(aes(x = 0, y=label_y, label=ifelse(is.na(label), '', label)), check_overlap = TRUE) +
        geom_text(aes(x = 0, y=label_y, label=label), vjust=1, check_overlap = TRUE) +
        geom_text(aes(x = 0, y=label_y, label=Step), vjust=-1, check_overlap = TRUE) +
        scale_fill_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
        theme_classic() +
        theme(
              axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.line=element_blank(),
              legend.position = "none",
              legend.title=element_text(size=15),
              legend.text=element_text(size=12),
              #legend.justification = c(0, 1),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             fill="")
}

#' plots the proportions with confidence intervals according to prop.test
#'
#' @param numerators numerators
#' @param denominators denominators
#' @param categories categories
#' @param confidence_level the confidence level (e.g. 0.95) passed to prop.test
#' @param show_confidence_values show the high/low confidence values
#' @param text_size text size (proportion value)
#' @param line_size the line size for the error bars
#' @param x_label label for x-axis
#' @param y_label label for y-axis
#' @param title title
#'
#' @importFrom magrittr "%>%"
#' @imoprtFrom purrr map2 map_dbl
#' @importFrom scales percent pretty_breaks percent_format
#' @importFrom ggplot2 ggplot aes labs geom_text theme_light theme element_text geom_errorbar geom_point expand_limits scale_y_continuous scale_color_manual
#' @export
rt_plot_proportions <- function(numerators,
                                denominators,
                                categories,
                                confidence_level = 0.95,
                                show_confidence_values=TRUE,
                                text_size=4,
                                line_size=0.35,
                                x_label="",
                                y_label="",
                                title="") {

    results <- map2(numerators, denominators, ~ prop.test(x=.x, n=.y, conf.level = confidence_level))

    plot_object <- data.frame(categories=factor(categories, levels=categories, ordered = TRUE),
                              proportions=map_dbl(results, ~ .$estimate),
                              conf_low=map_dbl(results, ~ .$conf.int[1]),
                              conf_high=map_dbl(results, ~ .$conf.int[2])) %>%
    ggplot(aes(x=categories, y=proportions, color=categories)) +
        geom_errorbar(aes(x=categories, min=conf_low, max=conf_high, color=categories), size=line_size) +
        geom_point(size=line_size*2) +
        geom_text(aes(label=percent(proportions)), size=text_size, vjust=-0.5, check_overlap = TRUE) +
        expand_limits(y=0) +
        scale_y_continuous(breaks=pretty_breaks(), labels = percent_format()) +
        scale_color_manual(values=c(rt_colors(), rt_colors())) +
        theme_light() +
        theme(legend.position = 'none',
              axis.text.x = element_text(angle = 30, hjust = 1)) +
        labs(x=x_label,
             y=y_label,
             title=title,
             caption = paste("\nConfidence Level:", confidence_level))

    if(show_confidence_values) {

        plot_object <- plot_object +
            geom_text(aes(label=percent(conf_low), y=conf_low), size=text_size, vjust=1.1, check_overlap = TRUE) +
            geom_text(aes(label=percent(conf_high), y=conf_high), size=text_size, vjust=-0.5, check_overlap = TRUE)
    }

    return (plot_object)
}
