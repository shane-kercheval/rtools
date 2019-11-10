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
            if(is.null(nrow(value_counts)) || all(is.na(value_counts))) {
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

    original_num_rows <- nrow(dataset)
    data_numeric <- dataset %>% select_if(is.numeric)

    # remove columns that have > max_missing_column_perc % of data missing
    data_numeric <- data_numeric[, colSums(is.na(data_numeric)) / nrow(data_numeric) <= max_missing_column_perc]

    data_numeric <- data_numeric[complete.cases(data_numeric), ]
    remaining_num_rows <- nrow(data_numeric)

    rcorr_results <- rcorr(as.matrix(data_numeric), type=type)
    correlations <- rcorr_results$r
    p_values <- rcorr_results$P

    correlations[which(abs(correlations) <= corr_threshold, arr.ind=TRUE)] <- NA # set correlations that are 'lower' than corr_threshold to NA
    correlations[which(p_values > p_value_threshold, arr.ind=TRUE)] <- NA # set correlations that have p_value > `p_value_threshold` to NA (i.e. we only want values who have low p_value (i.e. statistically significant), lower than pvalue threshold)

    # set diagnal and above to NA (it is just duplicated information)
    correlations[upper.tri(correlations, diag = TRUE)] <- NA

    # the first row and the last column will contain ALL NAs, remove
    correlations <- correlations[-1,]
    correlations <- correlations[,-ncol(correlations)]

    return (list(correlations=as.matrix(round(correlations, 2)),
                 num_rows_dataset=original_num_rows,
                 num_rows_used=remaining_num_rows))
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

    results <- rt_explore_correlations(dataset=dataset,
                                       corr_threshold=corr_threshold,
                                       p_value_threshold=p_value_threshold,
                                       type=type,
                                       max_missing_column_perc=max_missing_column_perc)

    correlations <- results$correlations

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
             fill = 'Colors',
             caption=paste("\nBased on",
                           results$num_rows_used,
                           "rows (removed",
                           results$num_rows_dataset - results$num_rows_used,
                           "rows with missing values)")) +
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
#' @param second_variable group by a second variable
#' @param count_distinct count the distinct number of values in this column
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
#' @importFrom dplyr sym count mutate group_by ungroup summarise rename arrange select filter n_distinct
#' @importFrom stringr str_count
#' @importFrom tidyr gather separate
#' @export
rt_explore_value_totals <- function(dataset,
                                    variable,
                                    second_variable=NULL,
                                    count_distinct=NULL,
                                    sum_by_variable=NULL,
                                    multi_value_delimiter=NULL) {

    # if either is NULL make sure both aren't not null
    if(!is.null(sum_by_variable) || !is.null(count_distinct)) {

        rt_stopif(!is.null(sum_by_variable) && !is.null(count_distinct))
    }

    dataset <- dataset %>% select(c(variable, second_variable, count_distinct, sum_by_variable))
    # capture original values (might be changed by multi-value-delimiter)
    values <- dataset[[variable]]
    symbol_variable <- sym(variable)  # because we are using string variables
    symbol_sum_by <- NULL
    if(!is.null(sum_by_variable)) {
        symbol_sum_by <- sym(sum_by_variable)
    }

    # NOTE some of the following code will give warnings if there are NA's,
    # but we want to keep NAs, no use fct_explicit_na so that,
    # e.g. we can use `na.value = '#2A3132'` with scale_fill_manual
    # so we use suppressWarnings

    ##########################################################################################################
    # Transform primary column from multi-value to single value (duplicates records)
    ##########################################################################################################
    if(!is.null(multi_value_delimiter)) {

        dataset <- rt_transform_multi_value_df(dataset=dataset,
                                               variable=variable,
                                               multi_value_delimiter=multi_value_delimiter)
    }

    if(is.null(count_distinct)) {

        if(is.null(second_variable)) {

            totals <- suppressWarnings(dataset %>% count(!!symbol_variable, wt=!!symbol_sum_by)) %>%
                arrange(!!symbol_variable)
            totals$percent <- totals$n / sum(totals$n)


            if(sum(totals$n) == 0) {
                # e.g. all sum_by values are NA
                totals$percent <- 0

            } else {

                stopifnot(rt_are_numerics_equal(sum(totals$percent), 1, num_decimals = 8))
            }

        } else {

            symbol_second <- sym(second_variable)
            totals <- suppressWarnings(dataset %>% count(!!symbol_variable, !!symbol_second, wt=!!symbol_sum_by))
            totals$percent <- totals$n / sum(totals$n)
            totals <- suppressWarnings(totals %>%
                group_by(!!symbol_variable) %>%
                mutate(group_percent= n / sum(n)) %>%
                ungroup()) %>%
                arrange(!!symbol_variable, !!symbol_second)

            if(sum(totals$n) == 0) {
                # e.g. all sum_by values are NA
                totals$percent <- 0
                totals$group_percent <- 0

            } else {

                stopifnot(rt_are_numerics_equal(suppressWarnings(totals %>%
                                                                     filter(!is.nan(group_percent)) %>%
                                                                     group_by(!!symbol_variable) %>%
                                                                     summarise(check=sum(group_percent))) %>%
                                                    rt_get_vector('check'),
                                                1, num_decimals = 8))
                stopifnot(rt_are_numerics_equal(sum(totals$percent), 1, num_decimals = 8))
            }
        }

    } else {

        symbol_count_distinct <- sym(count_distinct)
        if(is.null(second_variable)) {

            totals <- suppressWarnings(dataset %>%
                group_by(!!symbol_variable) %>%
                summarise(n=n_distinct(!!symbol_count_distinct))) %>%
                arrange(!!symbol_variable)
            totals$percent <- totals$n / sum(totals$n)

            stopifnot(rt_are_numerics_equal(sum(totals$percent), 1, num_decimals = 8))

        } else {

            symbol_second <- sym(second_variable)
            totals <- suppressWarnings(dataset %>%
                group_by(!!symbol_variable, !!symbol_second) %>%
                summarise(n=n_distinct(!!symbol_count_distinct)))
            totals$percent <- totals$n / sum(totals$n)
            totals <- suppressWarnings(totals %>%
                group_by(!!symbol_variable) %>%
                mutate(group_percent= n / sum(n)) %>%
                ungroup()) %>%
                arrange(!!symbol_variable, !!symbol_second)

            stopifnot(rt_are_numerics_equal(suppressWarnings(totals %>%
                                                group_by(!!symbol_variable) %>%
                                                summarise(check=sum(group_percent))) %>%
                                                rt_get_vector('check'),
                                            1, num_decimals = 8))
            stopifnot(rt_are_numerics_equal(sum(totals$percent), 1, num_decimals = 8))
        }
    }

    if(is.null(symbol_sum_by)) {

        totals <- totals %>% rename(count = n)
    } else {

        totals <- totals %>% rename(sum = n)
    }

    if(is.factor(values)) {
        if(is.null(multi_value_delimiter)) {

            variable_levels <- levels(values)

        } else {
            # if we are delimiting, then the original factors aren't valid because they contain multi-value
            # levels
            variable_levels <- sort(unique(as.character(totals[[variable]])))
        }

        totals[, variable] <- factor(totals[[variable]], levels=variable_levels)
    }

    # if(!is.null(second_variable) && is.factor(dataset[[second_variable]])) {
    #
    #         totals <- totals %>% mutate(value = factor(value, levels=levels(values)))
    # }

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
#' @param facet_variable additional variable to facet by
#' @param count_distinct_variable when aggregating, rather than counting the total number of records, count distinct occurances of this variabled (cannot be used with `sum_by_variable`)
#' @param sum_by_variable the numeric variable to sum
#' @param order_by_count if TRUE (the default) it will plot the bars from most to least frequent, otherwise it will order by the original factor levels if applicable
#' @param show_variable_totals if TRUE (the default) the graph will display the totals for the variable
#' @param show_comparison_totals if TRUE (the default) the graph will display the totals for the comparison_variable
#' @param show_dual_axes show a secondary axis for the Count or Sum
#' @param view_type this setting describes the type/view of the graph
#'      Options are:
#'          "Bar" - Default option, for either single variable or with comparison_variable, bar-chart
#'          "Confidence Interval" - for either single variable or with comparison variable; when comparison_variable is not NULL, the denominator/count used is the same as faceting
#'          "Confidence Interval - within Variable" - valid when comparison_variable is not null, this provides confidence intervals similar to the "Stack" view
#'          "Stack" - valid when comparison_variable is not null, stack comparison variable within variable
#'          "Stack Percent" - valid when comparison_variable is not null, stack comparison variable within variable (i.e. for each variable value, the comparison_variable percentages are shown)
#' @param multi_value_delimiter if the variable contains multiple values (e.g. "A", "A, B", ...) then setting
#'      this variable to the delimiter will cause the function to count seperate values
#' @param reverse_stack reverse stack from the default stacking order (defaulted to `TRUE`)
#' @param simple_mode changes to single color for single bar charts and removes percentages
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise mutate ungroup arrange n count desc select bind_rows
#' @importFrom scales percent_format percent pretty_breaks format_format
#' @importFrom ggplot2 ggplot aes aes geom_bar scale_y_continuous geom_text labs theme_light theme element_text position_fill position_dodge scale_fill_manual sec_axis facet_wrap
#' @importFrom stats as.formula
#' @export
rt_explore_plot_value_totals <- function(dataset,
                                         variable,
                                         comparison_variable=NULL,
                                         facet_variable=NULL,
                                         sum_by_variable=NULL,
                                         count_distinct_variable=NULL,
                                         order_by_count=TRUE,
                                         show_variable_totals=TRUE,
                                         show_comparison_totals=TRUE,
                                         show_dual_axes=FALSE,
                                         view_type="Bar",
                                         multi_value_delimiter=NULL,
                                         reverse_stack=TRUE,
                                         simple_mode=FALSE,
                                         base_size=11) {

    stopifnot(view_type %in% c("Bar", "Confidence Interval", "Confidence Interval - within Variable", "Stack", "Stack Percent"))

    # we can't do confidence intervals if we are suming by a variable.
    # Confidence intervals are based on sample size, which come from the denominator of the proportion.
    # But if e.g. we are summing across money, and we are adding 2 records that have a value of $1M, those very large numbers
    # will result in a very small proportion confidence interval, even though it is based on only 2 records;
    # so confidence intervals for weighted counts probably doesn't make sense in general.
    rt_stopif(!is.null(sum_by_variable) && view_type %in% c("Confidence Interval", "Confidence Interval - within Variable"))
    # same with counting by distinct variable, it could be a count of 4 distinct values, but a million observations;
    # i doubt the same rules of confidence intervals apply
    # additionally, what would it mean to stack unique ids of a comparison within the primary variable?;
    # they don't necessarily add up, could be counting the same id in each sub-category
    rt_stopif(!is.null(count_distinct_variable) && view_type %in% c("Confidence Interval",
                                                                    "Confidence Interval - within Variable",
                                                                    "Stack",
                                                                    "Stack Percent"))
    symbol_variable <- sym(variable)  # because we are using string variables

    if(is.null(facet_variable)) {
        groups_by_variable <- rt_explore_value_totals(dataset=dataset,
                                                      variable=variable,
                                                      sum_by_variable=sum_by_variable,
                                                      count_distinct=count_distinct_variable,
                                                      multi_value_delimiter=multi_value_delimiter)
    } else {
        # FYI should include NA
        symbol_facet_variable <- sym(facet_variable)
        unique_facet_values <- unique(dataset[[facet_variable]])
        groups_by_variable <- NULL
        for(facet_value in unique_facet_values) {
            #facet_value <- unique_facet_values[1]

            temp <- rt_explore_value_totals(dataset=dataset %>%
                                                filter(rt_equal_include_na(!!symbol_facet_variable,
                                                                           facet_value)),
                                            variable=variable,
                                            sum_by_variable=sum_by_variable,
                                            count_distinct=count_distinct_variable,
                                            multi_value_delimiter=multi_value_delimiter)

            temp[[facet_variable]] <- paste(facet_variable, '-', facet_value)
            groups_by_variable <- bind_rows(groups_by_variable, temp)
        }

        if(is.factor(unique_facet_values)) {
            groups_by_variable[[facet_variable]] <- factor(groups_by_variable[[facet_variable]],
                                                           levels= paste(facet_variable, '-', levels(unique_facet_values)),
                                                           ordered=TRUE)
        }
    }

    if(is.null(sum_by_variable)) {

        groups_by_variable <- groups_by_variable %>% rename(total=count)

    } else {

        groups_by_variable <- groups_by_variable %>% rename(total=sum)
    }

    if(rt_is_null_na_nan(comparison_variable)) {

        if(order_by_count) {

            ordered_levels <- groups_by_variable %>% arrange(desc(total)) %>% rt_get_vector(variable)
            groups_by_variable[[variable]] <- factor(groups_by_variable[[variable]],
                                                     levels = unique(ordered_levels))

        } else {

            groups_by_variable <- groups_by_variable %>% arrange(!!symbol_variable)
        }

        stopifnot(view_type %in% c("Bar", "Confidence Interval"))

        if(view_type == "Confidence Interval") {

            if(is.null(facet_variable)) {

                facets <- NULL

            } else {

                facets <- dataset[[facet_variable]]
            }
            rt_plot_multinom_cis(values=dataset[[variable]],
                                 groups=NULL,
                                 facets=facets,
                                 facet_variable_name=facet_variable,
                                 ci_within_variable=FALSE,
                                 confidence_level = 0.95,
                                 show_confidence_values=show_variable_totals,
                                 axes_flip=FALSE,
                                 simple_mode=simple_mode,
                                 axis_limits=NULL,
                                 text_size=4,
                                 line_size=0.35,
                                 base_size=base_size,
                                 x_label=variable,
                                 y_label="Percent of Dataset",
                                 subtitle = "",
                                 title=paste0("Percent of dataset containing `", variable, "` categories."))
        } else {

            if(is.null(sum_by_variable)) {
                if(is.null(count_distinct_variable)) {

                    plot_title <- paste0("Count of records for each `", variable, "` category.")
                    plot_y_axis_label <- "Count"
                    plot_y_second_axis_label <- "Percent of Dataset"

                } else {

                    plot_title <- paste0("Count of Unique `", count_distinct_variable, "` by `", variable, "`")
                    plot_y_axis_label <- paste0("Count of Unique `", count_distinct_variable, "`")
                    plot_y_second_axis_label <- NULL
                }
            } else {



                plot_title <- paste0('Sum of `', sum_by_variable,'`, by `', variable, '`')

                if(!is.null(facet_variable)) {

                    plot_title <- paste0(plot_title, ' and `', facet_variable,'`')
                }

                plot_subtitle <- NULL
                #plot_title <- paste0('Percent of `', variable,'` after summing across `' , sum_by_variable, '`')
                plot_y_axis_label <- paste0('`' , sum_by_variable, '`')
                plot_y_second_axis_label <- paste0('Percent of Total `', sum_by_variable, '`')
            }

            private__create_bar_chart_single_var(groups_by_variable,
                                                 variable,
                                                 facet_variable,
                                                 simple_mode,
                                                 show_dual_axes,
                                                 plot_y_second_axis_label,
                                                 show_variable_totals,
                                                 plot_title,
                                                 plot_y_axis_label,
                                                 base_size)
        }
    } else {

        symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables

        if(is.null(facet_variable)) {

            groups_by_both <- rt_explore_value_totals(dataset=dataset,
                                                      variable=variable,
                                                      second_variable = comparison_variable,
                                                      sum_by_variable=sum_by_variable,
                                                      count_distinct=count_distinct_variable,
                                                      multi_value_delimiter=multi_value_delimiter) %>%
                rename(actual_percent=percent)

        } else {
            # FYI should include NA
            symbol_facet_variable <- sym(facet_variable)
            unique_facet_values <- unique(dataset[[facet_variable]])
            groups_by_both <- NULL
            for(facet_value in unique_facet_values) {
                #facet_value <- unique_facet_values[1]

                temp <- rt_explore_value_totals(dataset=dataset %>%
                                                    filter(rt_equal_include_na(!!symbol_facet_variable,
                                                                               facet_value)),
                                                variable=variable,
                                                second_variable = comparison_variable,
                                                sum_by_variable=sum_by_variable,
                                                count_distinct=count_distinct_variable,
                                                multi_value_delimiter=multi_value_delimiter) %>%
                    rename(actual_percent=percent)

                temp[[facet_variable]] <- paste(facet_variable, '-', facet_value)
                groups_by_both <- bind_rows(groups_by_both, temp)
            }

            if(is.factor(unique_facet_values)) {
                groups_by_both[[facet_variable]] <- factor(groups_by_both[[facet_variable]],
                                                               levels= paste(facet_variable, '-', levels(unique_facet_values)),
                                                               ordered=TRUE)
            }
        }

        if(is.null(sum_by_variable)) {

            groups_by_both <- groups_by_both %>% rename(total=count)

        } else {

            groups_by_both <- groups_by_both %>% rename(total=sum)
        }

        if(order_by_count) {

            if(is.null(facet_variable)) {

                ordered_levels <- groups_by_variable %>% arrange(desc(total)) %>% rt_get_vector(variable)

            } else {

                ordered_levels <- suppressWarnings(groups_by_variable %>%
                    group_by(!!symbol_variable) %>%
                    summarise(total = sum(total, na.rm = TRUE)) %>%
                    arrange(desc(total)) %>% rt_get_vector(variable))
            }

            groups_by_variable[[variable]] <- factor(groups_by_variable[[variable]], levels=ordered_levels)
            groups_by_both[[variable]] <- factor(groups_by_both[[variable]], levels=ordered_levels)

            comparison_order <- as.character(suppressWarnings((dataset %>%
                                                  count(!!symbol_comparison_variable) %>%
                                                  arrange(desc(n)))[[comparison_variable]]))
            groups_by_both[[comparison_variable]] <- factor(groups_by_both[[comparison_variable]],
                                                     levels = comparison_order)
        }

        if(view_type %in% c("Confidence Interval", "Confidence Interval - within Variable")) {
            if(view_type == "Confidence Interval") {

                ci_within_variable <- FALSE

                y_axis_label <- paste0("Percent of `", comparison_variable,"` sub-population")
                subtitle_label <- paste0("(each `", comparison_variable,"` category defines the sub-populations)")
                title_label <- paste0("Distribution of `", variable,"` for each category of `", comparison_variable,"`")

            } else {

                ci_within_variable <- TRUE

                y_axis_label <- paste0("Percent of `", comparison_variable,"` sub-population")
                subtitle_label <- paste0("(each `", variable,"` category defines the sub-populations)")
                title_label <- paste0("Percent of `", comparison_variable,"` represented in each `", variable,"` category.")
            }

            if(is.null(facet_variable)) {

                facets <- NULL

            } else {

                facets <- dataset[[facet_variable]]
            }
            rt_plot_multinom_cis(values=dataset[[variable]],
                                 groups=dataset[[comparison_variable]],
                                 facets=facets,
                                 facet_variable_name=facet_variable,
                                 ci_within_variable=ci_within_variable,
                                 confidence_level=0.95,
                                 show_confidence_values=FALSE,
                                 #axes_flip=FALSE,
                                 #axis_limits=NULL,
                                 text_size=4,
                                 line_size=0.35,
                                 base_size=base_size,
                                 x_label=variable,
                                 y_label=y_axis_label,
                                 group_name=comparison_variable,
                                 subtitle=subtitle_label,
                                 title=title_label)

        } else {

            if(!is.null(sum_by_variable)) {

                plot_title <- paste0('Sum of `', sum_by_variable,'`, by `', variable,'` and `', comparison_variable,'`')
                plot_subtitle <- ""
                #plot_title <- paste0('Percent of `', variable,'` after summing across `' , sum_by_variable, '`')
                if(view_type == "Stack Percent") {

                    plot_y_axis_label <- paste0('Percent of Total `' , sum_by_variable, '`')
                    plot_y_second_axis_label <- paste0('`', sum_by_variable, '`')

                } else {

                    plot_y_axis_label <- paste0('`' , sum_by_variable, '`')
                    plot_y_second_axis_label <- paste0('Percent of Total `', sum_by_variable, '`')
                }
            } else {

                if(is.null(count_distinct_variable)) {

                    # if(view_type == "Facet by Comparison") {

                    #     plot_title <- paste0("Distribution of `", variable, "` for each `", comparison_variable, "` category.")
                    #     plot_subtitle <- ""

                    #     plot_y_axis_label <- "Count"
                    #     plot_y_second_axis_label <- "Percent of Sub-population"

                    # } else
                    if(view_type == "Stack") {

                        plot_title <- paste0("Count of `", variable, "` by `", comparison_variable, "`")
                        plot_subtitle <- ""
                        plot_y_axis_label <- "Count"
                        plot_y_second_axis_label <- NULL

                    } else if(view_type == "Stack Percent") {

                        plot_title <- paste0("Percent `", comparison_variable, "` within each `", variable, "` category.")
                        plot_subtitle <- ""
                        plot_y_axis_label <- paste0("Perent of `", variable, "` category")
                        plot_y_second_axis_label <- NULL

                    } else {

                        plot_title <- paste0("Count of `", comparison_variable, "` records within each `", variable, "` category.")
                        plot_subtitle <- ""
                        plot_y_axis_label <- "Count"
                        plot_y_second_axis_label <- paste0("Percent of Dataset")
                    }
                } else {

                    # if(view_type == "Facet by Comparison" ) {
                    #
                    #     plot_title <- paste0("Distribution of unique counts of `", count_distinct_variable, "`")
                    #     plot_subtitle <- paste0("by `", variable, "` for each `", comparison_variable, "` category.")
                    #     plot_y_axis_label <- "Percent of Unique Values in Sub-population"
                    #     plot_y_second_axis_label <- "Count of Unique Values"
                    #
                    # # } else if(view_type == "Stack") {
                    # #
                    # #     plot_title <- paste0("Count of unique `", count_distinct_variable, "`")
                    # #     plot_subtitle <- paste0("by `", variable, "` for each `", comparison_variable, "` category.")
                    # #     plot_y_axis_label <- paste0("Count of Unique `", count_distinct_variable, "`")
                    # #     plot_y_second_axis_label <- NULL
                    #
                    # } else {
                        plot_title <- paste0("Count of Unique `", count_distinct_variable, "`")
                        plot_subtitle <- paste0("by `", variable, "` for each `", comparison_variable, "` category.")
                        plot_y_axis_label <- paste0("Count of Unique `", count_distinct_variable, "`")
                        plot_y_second_axis_label <- NULL
                    # }
                }
            }

            private__create_bar_chart_comparison_var(groups_by_variable,
                                                     groups_by_both,
                                                     variable,
                                                     comparison_variable,
                                                     facet_variable,
                                                     count_distinct_variable,
                                                     view_type,
                                                     show_dual_axes,
                                                     reverse_stack,
                                                     show_variable_totals,
                                                     show_comparison_totals,
                                                     plot_y_second_axis_label,
                                                     plot_title,
                                                     plot_subtitle,
                                                     plot_y_axis_label,
                                                     base_size)
        }
    }
}

#' returns a graph for categoric/numeric variables based on the aggregation_type
#'
#' @param dataset dataframe containing numberic columns
#' @param categoric_variable the variable (e.g. factor) to get unique values from
#' @param numeric_variable the additional variable to group by; must be a string/factor column
#' @param aggregation_type must be one of 'Total', 'Mean', 'Average Value Per Record', 'Median', 'Boxplot'
#' @param color_variable additional categoric variable to color by
#' @param facet_variable additional categoric variable to facet by
#' @param show_variable_totals if TRUE (the default) the graph will display the totals for the variable
#' @param show_comparison_totals if TRUE (the default) the graph will display the totals for the comparison_variable
#' @param simple_mode simplifies the coloring scheme and text
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise n
#' @importFrom scales pretty_breaks format_format
#' @importFrom ggplot2 ggplot aes geom_bar geom_point geom_errorbar scale_y_continuous geom_text labs theme_light theme element_text position_dodge scale_fill_manual scale_color_manual facet_wrap coord_cartesian
#' @importFrom stats as.formula
#' @export
rt_explore_plot_categoric_numeric_aggregation <- function(dataset,
                                                          categoric_variable,
                                                          numeric_variable,
                                                          aggregation_type,
                                                          color_variable=NULL,
                                                          facet_variable=NULL,
                                                          show_variable_totals=TRUE,
                                                          show_comparison_totals=TRUE,
                                                          simple_mode=FALSE,
                                                          base_size=11) {

    stopifnot(aggregation_type %in% c('Total', 'Mean', 'Average Value Per Record', 'Median', 'Boxplot'))
    symbol_numeric <- sym(numeric_variable)
    symbol_categoric <- sym(categoric_variable)

    convert_facet_variable <- function(aggregated_data, facet_variable) {
        if(!is.null(facet_variable)) {

            unique_facet_values <- unique(aggregated_data[[facet_variable]])

            aggregated_data[[facet_variable]] <- paste(facet_variable, '-', aggregated_data[[facet_variable]])

            if(is.factor(unique_facet_values)) {
                aggregated_data[[facet_variable]] <- factor(aggregated_data[[facet_variable]],
                                                            levels= paste(facet_variable, '-', levels(unique_facet_values)),
                                                            ordered=TRUE)
            }
        }

        return (aggregated_data)
    }
    helper_get_colors <- function(dataset, categoric_variable, color_variable, simple_mode) {
        if(is.null(color_variable)) {

            if(simple_mode) {

                custom_colors <- rep(rt_colors()[1], length(unique(dataset[[categoric_variable]])))

            } else {

                custom_colors <- rt_get_colors_from_values(dataset[[categoric_variable]])
            }
        } else {

            custom_colors <- rt_get_colors_from_values(dataset[[color_variable]])
        }

        return (custom_colors)
    }
    plot_bar_mean_value <- function(aggregated_data,
                                    categoric_variable,
                                    numeric_variable,
                                    color_variable,
                                    facet_variable,
                                    base_size,
                                    plot_title,
                                    plot_y,
                                    plot_caption) {


        aggregated_data <- convert_facet_variable(aggregated_data, facet_variable)

        symbol_categoric <- sym(categoric_variable)
        symbol_numeric <- sym(numeric_variable)

        if(is.null(facet_variable)){
            symbol_facet <- NULL
        } else {
            symbol_facet <- sym(facet_variable)
        }

        custom_colors <- helper_get_colors(dataset=aggregated_data,
                                           categoric_variable=categoric_variable,
                                           color_variable=color_variable,
                                           simple_mode=simple_mode)

        if(is.null(color_variable)) {

            symbol_color <- symbol_categoric

        } else {

            symbol_color <- sym(color_variable)
        }

        unique_values_plot <- aggregated_data %>%
            ggplot(aes(x=!!symbol_categoric, y=mean_value, fill=!!symbol_color)) +
            geom_bar(stat = 'identity', alpha=0.75, position = position_dodge(0.9)) +
            scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",",
                                                                                preserve.width="none",
                                                                                digits=4,
                                                                                scientific=FALSE)) +
            labs(title=plot_title,
                 y=plot_y,
                 x=categoric_variable,
                 caption=plot_caption
            ) +
            scale_fill_manual(values=custom_colors, na.value = '#2A3132') +
            theme_light(base_size = base_size)

        if(show_variable_totals) {

            unique_values_plot <- unique_values_plot +
                geom_text(aes(label = private__standard_prettyNum(mean_value), y = mean_value),
                          vjust=-0.25, check_overlap=TRUE, position=position_dodge(0.9)) +
                geom_text(aes(label = private__standard_prettyNum(num_records), y = mean_value),
                          vjust=1.25, check_overlap=TRUE, position=position_dodge(0.9))
        }

        if(is.null(color_variable)) {
            unique_values_plot <- unique_values_plot +
                theme(legend.position = 'none',
                      axis.text.x = element_text(angle = 30, hjust = 1))
        }

        if(!is.null(facet_variable)) {

            unique_values_plot <- unique_values_plot +
                facet_wrap(as.formula(paste("~", facet_variable)), ncol = 1, scales = 'free_y') +
                coord_cartesian(clip = "off")
        }

        return (unique_values_plot)
    }

    if(aggregation_type == 'Total') {

        rt_explore_plot_value_totals(dataset=dataset,
                                     variable=categoric_variable,
                                     comparison_variable=color_variable,
                                     facet_variable=facet_variable,
                                     sum_by_variable=numeric_variable,
                                     #count_distinct_variable=NULL,
                                     order_by_count=FALSE,
                                     show_variable_totals=show_variable_totals,
                                     show_comparison_totals=show_comparison_totals,
                                     #show_dual_axes=FALSE,
                                     view_type="Bar",
                                     multi_value_delimiter=NULL,
                                     simple_mode=simple_mode,
                                     #reverse_stack=TRUE,
                                     base_size=base_size)

    } else if(aggregation_type == 'Mean') {

        # confidence interval
        # bootstrap??

        # average value on top of bar
        # sample size below top of bar
        group_by_variables <- c(categoric_variable, color_variable, facet_variable)
        aggregated_data <- suppressWarnings(dataset %>%
            group_by(.dots=group_by_variables) %>%
            summarise(num_records=sum(!is.na(!!symbol_numeric)),
                      mean_value=mean(!!symbol_numeric, na.rm = TRUE)))

        plot_title=paste0("Mean `", numeric_variable, "`, by ", paste0(paste0("`", c(categoric_variable, color_variable, facet_variable), "`"), collapse = ", "))
        plot_y=paste0("Mean `", numeric_variable, "`")
        plot_caption="The number above the top of the bars is the mean;\nthe number below the top of the bars is the total number of non-missing values for the group."

        return (plot_bar_mean_value(aggregated_data=aggregated_data,
                                    categoric_variable=categoric_variable,
                                    numeric_variable=numeric_variable,
                                    color_variable=color_variable,
                                    facet_variable=facet_variable,
                                    base_size=base_size,
                                    plot_title=plot_title,
                                    plot_y=plot_y,
                                    plot_caption=plot_caption))

    } else if(aggregation_type == 'Average Value Per Record') {

        # no confidence interval possible
        # average value on top of bar
        # sample size below top of bar

        group_by_variables <- c(categoric_variable, color_variable, facet_variable)
        aggregated_data <- suppressWarnings(dataset %>%
                             group_by(.dots=group_by_variables) %>%
                             summarise(num_records=n(),
                                       mean_value=sum(!!symbol_numeric, na.rm = TRUE) / num_records))

        plot_title=paste0("Average Value (`", numeric_variable, "`) Per Record, by ", paste0(paste0("`", c(categoric_variable, color_variable, facet_variable), "`"), collapse = ", "))
        plot_y=paste0("Average Value (`", numeric_variable, "`) Per Record")
        plot_caption="The number above the top of the bars is the average value per record;\nthe number below the top of the bars is the total number of records for the group."

        return (plot_bar_mean_value(aggregated_data=aggregated_data,
                                    categoric_variable=categoric_variable,
                                    numeric_variable=numeric_variable,
                                    color_variable=color_variable,
                                    facet_variable=facet_variable,
                                    base_size=base_size,
                                    plot_title=plot_title,
                                    plot_y=plot_y,
                                    plot_caption=plot_caption))

    } else if(aggregation_type == 'Median') {

        # could do like median + 90% conf interval e.g. 5th percentile 95% percentile
        # show the sample size somewhere??
        group_by_variables <- c(categoric_variable, color_variable, facet_variable)
        aggregated_data <- suppressWarnings(dataset %>%
                             group_by(.dots=group_by_variables) %>%
                             summarise(num_records=sum(!is.na(!!symbol_numeric)),
                                       percentile_5th=quantile(x=!!symbol_numeric, probs=0.05, na.rm=TRUE),
                                       median_value=median(!!symbol_numeric, na.rm = TRUE),
                                       percentile_95th=quantile(x=!!symbol_numeric, probs=0.95, na.rm=TRUE)))

        aggregated_data <- convert_facet_variable(aggregated_data, facet_variable)

        if(is.null(facet_variable)){
            symbol_facet <- NULL
        } else {
            symbol_facet <- sym(facet_variable)
        }

        custom_colors <- helper_get_colors(dataset=aggregated_data,
                                           categoric_variable=categoric_variable,
                                           color_variable=color_variable,
                                           simple_mode=simple_mode)

        if(is.null(color_variable)) {

            symbol_color <- symbol_categoric

        } else {

            symbol_color <- sym(color_variable)
        }

        plot_title=paste0("Median `", numeric_variable, "`, by ", paste0(paste0("`", c(categoric_variable, color_variable, facet_variable), "`"), collapse = ", "))
        plot_y=paste0("Median `", numeric_variable, "`")
        plot_caption="The number above the point is the median value;\nthe number below the point is the total number of non-missing values for the group;\nThe bottom and top of the error-bar gives the 5th and 95th percentile."

        unique_values_plot <- aggregated_data %>%
            ggplot(aes(x=!!symbol_categoric, y=median_value, color=!!symbol_color)) +
            geom_point(position=position_dodge(0.9),) +
            geom_errorbar(aes(ymin = percentile_5th, ymax = percentile_95th),
                          position=position_dodge(0.9),) +
            scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",",
                                                                                preserve.width="none",
                                                                                digits=4,
                                                                                scientific=FALSE)) +
            labs(title=plot_title,
                 y=plot_y,
                 x=categoric_variable,
                 caption=plot_caption
            ) +
            scale_color_manual(values=custom_colors, na.value = '#2A3132') +
            theme_light(base_size = base_size)

        if(show_variable_totals) {
            unique_values_plot <- unique_values_plot +
                geom_text(aes(label = private__standard_prettyNum(median_value), y = median_value),
                          vjust=-0.4, check_overlap=TRUE, position=position_dodge(0.9)) +
                geom_text(aes(label = private__standard_prettyNum(num_records), y = median_value),
                          vjust=1.4, check_overlap=TRUE, position=position_dodge(0.9))
        }
        if(is.null(color_variable)) {
            unique_values_plot <- unique_values_plot +
                theme(legend.position = 'none',
                      axis.text.x = element_text(angle = 30, hjust = 1))
        }

        if(!is.null(facet_variable)) {

            unique_values_plot <- unique_values_plot +
                facet_wrap(as.formula(paste("~", facet_variable)), ncol = 1, scales = 'free_y') +
                coord_cartesian(clip = "off")
        }

        return (unique_values_plot)

    } else if(aggregation_type == 'Boxplot') {

        rt_explore_plot_boxplot(dataset=dataset,
                                variable=numeric_variable,
                                comparison_variable=categoric_variable,
                                color_variable=color_variable,
                                facet_variable=facet_variable,
                                simple_mode=simple_mode,
                                # y_zoom_min=y_zoom_min,
                                # y_zoom_max=y_zoom_max,
                                base_size=base_size)

    } else {
        stopifnot(FALSE)
    }
}


#' returns a boxplot of `variable`, potentally grouped by `comparison_variable`
#'
#' @param dataset dataframe containing numberic columns
#' @param variable the variable from which to create a boxplot
#' @param comparison_variable the additional variable to group by; must be a string/factor column
#' @param color_variable if comparison_variable is used, then if color_variable is used the boxplots will be colored by this variable rather than each of the comparison_variable categories. `color_variable` must be categoric
#' @param facet_variable  if comparison_variable is used, then if facet_variable is used the boxplots will be faceted by this variable `faceted` must be categoric
#' @param simple_mode if a comparison variable is used (and no color variable), use a single color
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes geom_boxplot scale_x_discrete xlab ylab theme_light theme element_text coord_cartesian scale_color_manual geom_text position_dodge geom_hline
#' @importFrom dplyr group_by summarise n filter bind_rows
#' @importFrom scales pretty_breaks format_format
#' @export
rt_explore_plot_boxplot <- function(dataset,
                                    variable,
                                    comparison_variable=NULL,
                                    color_variable=NULL,
                                    facet_variable=NULL,
                                    simple_mode=FALSE,
                                    y_zoom_min=NULL,
                                    y_zoom_max=NULL,
                                    base_size=11) {

    symbol_variable <- sym(variable)  # because we are using string variables

    if(!is.null(color_variable)) {
        rt_stopif(is.null(comparison_variable))
    }
    if(!is.null(facet_variable)) {
        rt_stopif(is.null(comparison_variable))
    }

    if(rt_is_null_na_nan(comparison_variable)) {

        plot_labels <- dataset %>%
            summarise(y_position = median(!!symbol_variable, na.rm = TRUE),
                      med = y_position,
                      avg = mean(!!symbol_variable, na.rm = TRUE),
                      cnt = sum(!is.na(!!symbol_variable)))

        boxplot_plot <- ggplot(dataset, aes(y=!!symbol_variable, group=1)) +
            geom_boxplot() +
            geom_hline(yintercept = plot_labels[['avg']], show.legend = FALSE, color=rt_colors()[1], size=.7) +
            geom_text(data = plot_labels,
                      mapping = aes(y=y_position, x=0, group=1,
                                    label = private__standard_prettyNum(med)),
                      vjust=-0.5, check_overlap = TRUE) +
            scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
            scale_x_discrete(breaks = NULL) +
            labs(caption = paste("\n", private__standard_prettyNum(plot_labels[['cnt']]),
                                 'Non-NA Values',
                                 "\nAverage (blue line): ", private__standard_prettyNum(plot_labels[['avg']])),
                 y=variable,
                 x='') +
            theme_light(base_size = base_size)

    } else {

        symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables
        if(is.null(color_variable)) {

            symbol_color_variable <- symbol_comparison_variable

        } else {

            symbol_color_variable <- sym(color_variable)
        }

        if(!is.null(facet_variable)) {
            facet_values <- dataset[[facet_variable]]

            dataset[[facet_variable]] <- paste(facet_variable, '-', as.character(facet_values))

            if(is.factor(facet_values)) {
                dataset[[facet_variable]] <- factor(dataset[[facet_variable]],
                                                    levels= paste(facet_variable, '-', levels(facet_values)),
                                                    ordered=TRUE)
            }
        }

        helper_get_aggregations <- function(local_dataset,
                                            color_variable,
                                            symbol_variable,
                                            symbol_comparison_variable) {

            if(is.null(color_variable)) {

                local_aggregations <- suppressWarnings(
                    local_dataset %>%
                        group_by(!!symbol_comparison_variable) %>%
                        summarise(median = round(median(!!symbol_variable, na.rm = TRUE), 4),
                                  count = sum(!is.na(!!symbol_variable))) %>% as.data.frame())


            } else {

                symbol_color_variable <- sym(color_variable)
                local_aggregations <- suppressWarnings(
                    local_dataset %>%
                        group_by(!!symbol_comparison_variable, !!symbol_color_variable) %>%
                        summarise(median = round(median(!!symbol_variable, na.rm = TRUE), 4),
                                  count = sum(!is.na(!!symbol_variable))) %>% as.data.frame())
            }

            return (local_aggregations)
        }

        aggregations <- NULL
        if(is.null(facet_variable)) {

            aggregations <- helper_get_aggregations(local_dataset=dataset,
                                                    color_variable=color_variable,
                                                    symbol_variable=symbol_variable,
                                                    symbol_comparison_variable=symbol_comparison_variable)

        } else {

            unique_facet_values <- unique(dataset[[facet_variable]])
            aggregations <- NULL
            for(facet_value in unique_facet_values) {
                #facet_value <- unique_facet_values[1]
                symbol_facet_variable <- sym(facet_variable)
                local_dataset <- dataset %>% filter(rt_equal_include_na(!!symbol_facet_variable, facet_value))

                temp <- helper_get_aggregations(local_dataset=local_dataset,
                                                color_variable=color_variable,
                                                symbol_variable=symbol_variable,
                                                symbol_comparison_variable=symbol_comparison_variable)

                temp[[facet_variable]] <- facet_value
                aggregations <- bind_rows(aggregations, temp)
            }

            if(is.factor(unique_facet_values)) {
                aggregations[[facet_variable]] <- factor(aggregations[[facet_variable]],
                                                         levels=levels(unique_facet_values),
                                                         ordered=TRUE)
            }
        }

        if(is.null(color_variable)) {

            if(simple_mode) {

                custom_colors <- rep(rt_colors()[1], length(unique(dataset[[comparison_variable]])))

            } else {

                custom_colors <- rt_get_colors_from_values(dataset[[comparison_variable]])
            }
        } else {

            custom_colors <- rt_get_colors_from_values(dataset[[color_variable]])
        }

        boxplot_plot <- ggplot(dataset,
                               aes(y=!!symbol_variable,
                                   x=!!symbol_comparison_variable,
                                   color=!!symbol_color_variable)) +
            scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
            geom_boxplot(position=position_dodge(0.9)) +
            geom_text(data = aggregations,
                      mapping = aes(y=median,
                                    x=!!symbol_comparison_variable,
                                    color=!!symbol_color_variable,
                                    label = private__standard_prettyNum(median)),
                      position=position_dodge(0.9),
                      vjust=-0.5,
                      check_overlap = TRUE) +
            geom_text(data = aggregations,
                      mapping = aes(y=median,
                                    x=!!symbol_comparison_variable,
                                    label = private__standard_prettyNum(count)),
                      position=position_dodge(0.9),
                      vjust=1.3,
                      check_overlap = TRUE) +
            scale_color_manual(values=custom_colors, na.value = '#2A3132') +
            labs(caption="\n# above median line is the median value, # below median line is the size of the group.",
                 x=comparison_variable,
                 y=variable) +
            theme_light(base_size = base_size)

        if(!is.null(facet_variable)) {

            boxplot_plot <- boxplot_plot +
                facet_wrap(as.formula(paste("~", facet_variable)), ncol = 1, scales = 'free_y') +
                coord_cartesian(clip = "off")
        }

        if(is.null(color_variable)) {

            boxplot_plot <- boxplot_plot +
                theme(legend.position = 'none',
                      axis.text.x = element_text(angle = 30, hjust = 1))

        } else {

            boxplot_plot <- boxplot_plot +
                theme(axis.text.x = element_text(angle = 30, hjust = 1))
        }
    }

    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[[variable]], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[[variable]], na.rm = TRUE)
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

            x_zoom_min <- min(dataset[[variable]], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(x_zoom_max)) {

            x_zoom_max <- max(dataset[[variable]], na.rm = TRUE)
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
#' @param label_variables variable to show above each point; if multiple variables, then the values are shown as e.g. `(x, y, z)`
#' @param label_size text size of the label corresponding to label_variables
#' @param alpha controls transparency
#' @param jitter enables/disables jittering
#' @param x_zoom_min adjust (i.e. zoom in) to the x-axis; sets the minimum x-value for the adjustment
#' @param x_zoom_max adjust (i.e. zoom in) to the x-axis; sets the maximum x-value for the adjustment
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes geom_point theme_light coord_cartesian geom_jitter position_jitter scale_y_continuous scale_color_manual geom_text
#' @importFrom scales pretty_breaks format_format
#' @importFrom dplyr arrange desc
#' @importFrom tidyr unite
#' @export
rt_explore_plot_scatter <- function(dataset,
                                    variable,
                                    comparison_variable,
                                    color_variable=NULL,
                                    size_variable=NULL,
                                    label_variables=NULL,
                                    label_size=4,
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

    if(!is.null(size_variable)) {

        # ggplot gives error if any size values are NA
        rt_stopif(any(is.na(dataset[[size_variable]])))
    }

    if(!is.null(label_variables)) {

        #ensure the new column i'm adding doesn't already exist; which would be a miracle
        rt_stopif('custom_label_column_dtyqpdhjdemn' %in% colnames(dataset))
        if(length(label_variables) > 1) {

            label_values <- dataset[, label_variables] %>% unite('col', sep = ', ', remove = TRUE)
            label_values <- paste0("(", label_values[[1]], ")")

            dataset$custom_label_column_dtyqpdhjdemn <- label_values

        } else {

            dataset$custom_label_column_dtyqpdhjdemn <- dataset[[label_variables]]
        }

        # check_overlap shows the first labels that appear in the dataset;
        # so, if we are going to label the points, then we want to sort the dataset so that the most
        # important/interesting values show up; for now, we are going to guess
        # first assumption: larger values are "more interesting"
        # if size_variable is not null, then sort by that first; then sort by variable and
        # comparision_variable
        if(!is.null(size_variable)) {

            dataset <- dataset %>%
                arrange(desc(!!symbol_size_variable),
                        desc(!!symbol_variable),
                        desc(!!symbol_comparison_variable))

        } else {

            dataset <- dataset %>% arrange(desc(!!symbol_variable), desc(!!symbol_comparison_variable))
        }
    }

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
        scale_x_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
        scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
        theme_light(base_size = base_size) +
        labs(x=comparison_variable,
             y=variable)

    if(!is.null(color_variable) &&
            (is.character(dataset[[color_variable]]) ||
                is.factor(dataset[[color_variable]]) ||
                is.logical(dataset[[color_variable]]))) {

        custom_colors <- rt_get_colors_from_values(dataset[[color_variable]])
        scatter_plot <- scatter_plot + scale_color_manual(values=custom_colors)
    }

    if(!is.null(size_variable) && is.numeric(dataset[[size_variable]])) {

        scatter_plot <- scatter_plot +
            scale_size_continuous(breaks=pretty_breaks(10),
                                  labels = format_format(big.mark=",",
                                                         preserve.width="none",
                                                         digits=4,
                                                         scientific=FALSE))
    }

    x_zooms <- NULL
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(x_zoom_min) || !rt_is_null_na_nan(x_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(x_zoom_min)) {

            x_zoom_min <- min(dataset[[comparison_variable]], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(x_zoom_max)) {

            x_zoom_max <- max(dataset[[comparison_variable]], na.rm = TRUE)
        }

        x_zooms <- c(x_zoom_min, x_zoom_max)
    }

    y_zooms <- NULL
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[[variable]], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[[variable]], na.rm = TRUE)
        }

        y_zooms <- c(y_zoom_min, y_zoom_max)
    }
    scatter_plot <- scatter_plot +
        coord_cartesian(xlim=x_zooms, ylim = y_zooms)

    if(!is.null(label_variables)) {

        if(length(label_variables) == 1 && is.numeric(dataset[[label_variables]])) {

            scatter_plot <- scatter_plot +
                geom_text(aes(label = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)(custom_label_column_dtyqpdhjdemn)),
                          vjust=-0.5, check_overlap=TRUE, size=label_size)
        } else {

            scatter_plot <- scatter_plot +
                geom_text(aes(label = custom_label_column_dtyqpdhjdemn),
                          vjust=-0.5, check_overlap=TRUE, size=label_size)
        }
    }

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
                                    label = private__standard_prettyNum(median)),
                      vjust=-0.5,
                      check_overlap = TRUE) +
            geom_text(data = aggregations,
                      mapping = aes(y=median,
                                    x=!!symbol_comparison_variable,
                                    label = private__standard_prettyNum(count)),
                      vjust=1.3,
                      check_overlap = TRUE) +
            scale_x_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
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
            scale_x_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
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
                geom_text(aes(y=agg_variable, label = private__standard_prettyNum(agg_variable)), check_overlap = TRUE, vjust=-0.5)
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
                mutate(r=map(splits, as.data.frame)) %>%
                unnest(r) %>%
                group_by(!!symbol_comparison_variable, id) %>%
                summarise(average_value = aggregation_function(value)) %>%
                summarise(bootstrap_low = quantile(average_value, 0.025),
                          bootstrap_high = quantile(average_value, 0.975))
            aggregate_plot <- aggregate_plot +
                geom_ribbon(data=bs, aes(ymin=bootstrap_low, ymax=bootstrap_high), alpha=0.25)
        }
    }

    aggregate_plot <- aggregate_plot +
        scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
        theme_light(base_size = base_size)

    x_zooms <- NULL
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(x_zoom_min) || !rt_is_null_na_nan(x_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(x_zoom_min)) {

            x_zoom_min <- min(dataset[[comparison_variable]], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(x_zoom_max)) {

            x_zoom_max <- max(dataset[[comparison_variable]], na.rm = TRUE)
        }

        x_zooms <- c(x_zoom_min, x_zoom_max)
    }

    y_zooms <- NULL
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[[variable]], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[[variable]], na.rm = TRUE)
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
#' @param facet_variable an optional variable (categoric) that seperates the time series by facet
#' @param year_over_year if true it displays the graph year-over-year; color_variable should be NULL (color will be year)
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param include_zero_y_axis expand the lower bound of the y-axis to 0 (TRUE is best practice.)
#' @param show_points if TRUE adds points to the graph
#' @param show_labels if TRUE adds labels to each point
#' @param date_floor converts dates to date_floor value and aggregates; options are e.g. "week", "month", "quarter"
#' @param date_break_format format of date breaks e.g. `'\%Y-\%m-\%d'`
#' @param date_breaks_width the date breaks for x axis, values correspond to ggplot scale_x_date e.g. "1 month", "1 week"
#' @param date_limits "zoom" for date x-axis, 2 values representing min/max in the format of YYYY-MM-DD. If `date_floor` is used, the date_limits are converted to the corresponding date floor
#'       e.g. if date_floor is 'month' and date_limits are `c('2013-01-15', '2013-12-15')` they will be converted to `c('2013-01-01', '2013-12-01')`
#' @param format_as_percent if `TRUE` display the y-axis as a percent, as well as any text labels
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr count group_by summarise rename filter
#' @importFrom ggplot2 ggplot aes labs geom_line expand_limits theme_light theme element_text coord_cartesian scale_color_manual geom_text geom_point scale_x_date facet_wrap
#' @importFrom lubridate floor_date year
#' @importFrom stringr str_to_title str_trim str_replace str_detect
#' @importFrom scales date_format pretty_breaks format_format percent_format
#' @export
rt_explore_plot_time_series <- function(dataset,
                                        variable,
                                        comparison_variable=NULL,
                                        comparison_function=NULL,
                                        comparison_function_name=NULL,
                                        color_variable=NULL,
                                        facet_variable=NULL,
                                        year_over_year=FALSE,
                                        y_zoom_min=NULL,
                                        y_zoom_max=NULL,
                                        include_zero_y_axis=TRUE,
                                        show_points=FALSE,
                                        show_labels=FALSE,
                                        date_floor=NULL,
                                        date_break_format=NULL,
                                        date_breaks_width=NULL,
                                        date_limits=NULL,
                                        format_as_percent=FALSE,
                                        base_size=11) {

    # if using a comparison variable, we must also have a function and function name
    stopifnot(!(!is.null(comparison_variable) &&
        (is.null(comparison_function) || is.null(comparison_function_name))))

    if(year_over_year) {
        stopifnot(is.null(color_variable))
    }

    symbol_variable <- sym(variable)  # because we are using string variables

    # if there are many NAs, it will mess up the count scale, and we can't plot them anyway
    dataset <- dataset %>% filter(!is.na(!!symbol_variable))

    symbol_if_not_null <- function(x) {
        if (is.null(x)) {

            return (NULL)

        } else {

            return (sym(x))
        }
    }

    title_context <- NULL
    x_label_context <- NULL

    if(is.null(date_floor)) {

        dataset[[variable]] <- as.Date(dataset[[variable]])

    } else {

        title_context <- paste0(str_to_title(date_floor), "ly")
        if(title_context == "Dayly") {
            title_context <- "Daily"
        }
        x_label_context <- paste0("(", date_floor, ")")

        dataset[[variable]] <- as.Date(floor_date(dataset[[variable]], unit=date_floor, week_start = 1))

        # if we have a date floor, we need to adjust the date_limits
        # e.g. if date floor is 'month' but our date limit starts half way through the month (e.g. because
        # perhaps the min date in the dataset starts half way through the month, then we need to make sure
        # the date limit is also the floor of the month; or whatever date aggregation we are doing.
        if(!is.null(date_limits)) {

            date_limits <- floor_date(as.Date(date_limits), unit=date_floor, week_start = 1)
        }


        if(str_detect(date_floor, 'quarter')) {

            date_breaks_width <- NULL

        } else if(is.null(date_breaks_width)) {

            date_breaks_width <- paste('1', date_floor)
        }

        if(is.null(date_break_format)) {
            if(str_detect(date_floor, 'week')) {

                date_break_format <- '%Y-%W'

            } else if (str_detect(date_floor, 'month')) {

                date_break_format <- '%Y-%m'

            } else if (str_detect(date_floor, 'quarter')) {

                date_break_format <- '%Y-%m'

            } else if (str_detect(date_floor, 'year')) {

                date_break_format <- '%Y'

            } else {

                date_break_format <- '%Y-%m-%d'
            }
        }

        if(year_over_year && str_detect(date_floor, 'week')) {
            # if it is year over year and the date floor is week then we need to force '%Y-%W'
            # because '%Y-%m-%d' will not be the same for each year

            date_break_format <- '%Y-%W'
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
    sym_facet_variable <- symbol_if_not_null(facet_variable)

    if(is.null(color_variable)) {

        custom_colors <- NULL

    } else {

        custom_colors <- rt_get_colors_from_values(dataset[[color_variable]])
    }

    if(!is.null(date_limits)) {
        date_limits <- as.Date(date_limits)
    }

    if(is.null(sym_comparison_variable)) {
        # if no comparison_variable, we are just counting records
        # either have to aggregate by variable, or variable and/or color/facet

        # COLOR | FACET
        # NULL  | NULL
        # NULL  | NOT
        # NOT   | NULL
        # NOT   | NOT
        if(is.null(sym_color_variable) && is.null(sym_facet_variable)) {

            dataset <- dataset %>% count(!!symbol_variable) %>% rename(total=n)

        } else if (is.null(sym_color_variable) && !is.null(sym_facet_variable)) {


            dataset <- dataset %>% count(!!symbol_variable, !!sym_facet_variable) %>% rename(total=n)


        } else if (!is.null(sym_color_variable) && is.null(sym_facet_variable)) {

            dataset <- dataset %>% count(!!symbol_variable, !!sym_color_variable) %>% rename(total=n)

        } else {

            dataset <- dataset %>%
                count(!!symbol_variable, !!sym_color_variable, !!sym_facet_variable) %>%
                rename(total=n)
        }

        if(year_over_year) {

            year_factor_levels <- as.character(sort(unique(year(dataset[[variable]]))))
            temp_string_date <- private__custom_date_format(date_floor, date_break_format)(dataset[[variable]])
            temp_string_date <- str_replace(temp_string_date, paste0(as.character(year(dataset[[variable]])), "-"), "")
            dataset$cohort___ <- factor(temp_string_date, levels = sort(unique(temp_string_date)), ordered = TRUE)
            dataset$year_factor___ <- factor(year(dataset[[variable]]), levels = year_factor_levels, ordered = TRUE)

            custom_colors <- rt_get_colors_from_values(dataset$year_factor___)

            ggplot_object <- dataset %>%
                ggplot(aes(x=cohort___, y=total, group=year_factor___, color=year_factor___)) +
                scale_color_manual(values=custom_colors, na.value = '#2A3132') +
                labs(title=str_trim(paste(title_context, 'Count of Records')),
                     x=str_trim(paste(variable, x_label_context)),
                     y='Count',
                     color='Year')

        } else {

            ggplot_object <- dataset %>%
                ggplot(aes(x=!!symbol_variable, y=total, color=!!sym_color_variable)) +
                scale_color_manual(values=custom_colors, na.value = '#2A3132') +
                scale_x_date(labels = private__custom_date_format(date_floor, date_break_format), breaks=date_breaks_width, limits=date_limits) +
                labs(title=str_trim(paste(title_context, 'Count of Records')),
                     x=str_trim(paste(variable, x_label_context)),
                     y='Count')

        }

    } else {
        # if we have a comparison_variable, we are have to aggregate by a given aggregation function
        # either have to aggregate by variable, or variable and/or color/facet

        # COLOR | FACET
        # NULL  | NULL
        # NULL  | NOT
        # NOT   | NULL
        # NOT   | NOT
        if(is.null(sym_color_variable) && is.null(sym_facet_variable)) {

            dataset <- dataset %>%
                group_by(!!symbol_variable) %>%
                summarise(total=comparison_function(!!sym_comparison_variable))

        } else if (is.null(sym_color_variable) && !is.null(sym_facet_variable)) {

            dataset <- dataset %>%
                group_by(!!symbol_variable, !!sym_facet_variable) %>%
                summarise(total=comparison_function(!!sym_comparison_variable))


        } else if (!is.null(sym_color_variable) && is.null(sym_facet_variable)) {

            dataset <- dataset %>%
                group_by(!!symbol_variable, !!sym_color_variable) %>%
                summarise(total=comparison_function(!!sym_comparison_variable))

        } else {

            dataset <- dataset %>%
                group_by(!!symbol_variable, !!sym_color_variable, !!sym_facet_variable,) %>%
                summarise(total=comparison_function(!!sym_comparison_variable))
        }

        if(year_over_year) {

            year_factor_levels <- as.character(sort(unique(year(dataset[[variable]]))))
            temp_string_date <- private__custom_date_format(date_floor, date_break_format)(dataset[[variable]])
            temp_string_date <- str_replace(temp_string_date, paste0(as.character(year(dataset[[variable]])), "-"), "")
            dataset$cohort___ <- factor(temp_string_date, levels = sort(unique(temp_string_date)), ordered = TRUE)
            dataset$year_factor___ <- factor(year(dataset[[variable]]), levels = year_factor_levels, ordered = TRUE)

            custom_colors <- rt_get_colors_from_values(dataset$year_factor___)

            ggplot_object <- dataset %>%
                ggplot(aes(x=cohort___, y=total, group=year_factor___, color=year_factor___)) +
                scale_color_manual(values=custom_colors, na.value = '#2A3132') +
                labs(title=str_trim(paste(title_context,
                                          paste(comparison_function_name,
                                                comparison_variable,
                                                'by', variable))),
                     x=str_trim(paste(variable, x_label_context)),
                     y=paste(comparison_function_name, comparison_variable),
                     color='Year')

        } else {

            ggplot_object <- dataset %>%
                ggplot(aes(x=!!symbol_variable, y=total, color=!!sym_color_variable)) +
                scale_color_manual(values=custom_colors, na.value = '#2A3132') +
                scale_x_date(labels = private__custom_date_format(date_floor, date_break_format), breaks=date_breaks_width, limits=date_limits) +
                labs(title=str_trim(paste(title_context,
                                          paste(comparison_function_name,
                                                comparison_variable,
                                                'by', variable))),
                     x=str_trim(paste(variable, x_label_context)),
                     y=paste(comparison_function_name, comparison_variable))
        }
    }
    ggplot_object <- ggplot_object +
        geom_line() +
        theme_light(base_size = base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

    if(format_as_percent) {

        ggplot_object <- ggplot_object +
            scale_y_continuous(breaks=pretty_breaks(10), labels = percent_format())

    } else {

        ggplot_object <- ggplot_object +
            scale_y_continuous(breaks=pretty_breaks(10),
                               labels = format_format(big.mark=",",
                                                      preserve.width="none",
                                                      digits=4,
                                                      scientific=FALSE))
    }

    if(include_zero_y_axis) {

        ggplot_object <- ggplot_object + expand_limits(y=0)
    }

    if(show_points) {

        ggplot_object <- ggplot_object + geom_point()
    }

    if(show_labels) {

        if(format_as_percent) {

            ggplot_object <- ggplot_object +
                geom_text(aes(label = percent_format()(total)),
                              check_overlap = TRUE,
                              vjust=-0.5)
        } else {

            ggplot_object <- ggplot_object +
                geom_text(aes(label = private__standard_prettyNum(total)),
                              check_overlap = TRUE,
                              vjust=-0.5)
        }
    }

    if(!is.null(facet_variable)) {

        ggplot_object <- ggplot_object +
            facet_wrap(facets = facet_variable , ncol = 1, scales = 'free_y', strip.position = "right") +
            theme(strip.text.y = element_text(size = base_size))
    }

    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[['total']], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[['total']], na.rm = TRUE)
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
               value = private__standard_prettyNum(value)) %>%
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
        theme(axis.title=element_blank(),
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

#' shows conversion rates (y-axis) of a particular cohort based on various 'snapshots' in time (a line is a snapshot in time).
#'
#' Each record in the cohort must have had enough time to convert, in order to be shown in the graph.
#' For example, say the snapshots we are interested in are at day 1 and day 7. If the cohort refers to
#'  February, and it is March 3rd, every record in the February cohort has had at least 1 day of "activity" or
#'  potential to convert, so we can include Feb for that snapshot. However, not everyone in Feb has had 7 days
#'  of activity, so we cannot yet include Feb for the 7 day snapshot, because people who joined the cohort on
#'  Feb 28, for example, have not had a full 7 days of potential activity.
#'
#' @param dataset dataframe
#' @param first_date the reference date (e.g. first-touch point)
#' @param second_date the date of conversion, NA if not converted
#' @param group_variable a variable to group/facet by. If `color_or_facet` will be ignored
#' @param reference_date we need to know how old the cohort is so we can determine if the
#' @param snapshots the numeric snapshots
#' @param snapshot_units the units of the snapshots e.g. `hours`, `days`, `weeks`
#' @param date_floor how we should define the cohort group e.g. by `day`, `by week`, by `months`
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate group_by ungroup arrange desc rename filter summarise n
#' @importFrom lubridate floor_date ymd
#' @importFrom tidyr crossing
#' @export
rt_get_cohorted_conversion_rates <- function(dataset,
                                             first_date,
                                             second_date,
                                             reference_date,
                                             group_variable=NULL,
                                             snapshots=c(1, 7, 14),
                                             snapshot_units='days',
                                             date_floor='month') {

    symbol_first_date <- sym(first_date)
    symbol_second_date <- sym(second_date)

    # this fixes a bug; I didn't realize crossing removes duplicates; so if, for example, the first-date
    # is `day` units, and there are multiple records that have the same date, they would be removed.
    # i'm simply add a temporary id (named so it doesn't interfer with other columns) so that there are no
    # duplicated values/rows
    dataset$ced54308f6c411e9802a5aa538984bd8 <- 1:nrow(dataset)

    if(is.null(group_variable)) {

        snapshotted_conversions <- dataset %>%
            # e.g. "# days from x to y" where x and y are first and second date
            mutate(time_units_from_x_to_y = rt_difftime_numeric(!!symbol_second_date,
                                                                !!symbol_first_date,
                                                                units = snapshot_units)) %>%
            mutate(cohort = as.character(floor_date(x=!!symbol_first_date, unit=date_floor, week_start=1))) %>%
            group_by(cohort) %>%
            mutate(youngest_age = rt_difftime_numeric(reference_date,
                                                      max(!!symbol_first_date),
                                                      units = snapshot_units)) %>%
            ungroup() %>%
            arrange(desc(!!symbol_first_date)) %>%
            #head() %>%
            crossing(snapshots) %>%
            rename(Snapshot=snapshots) %>%
            filter(youngest_age >= Snapshot) %>%
            group_by(cohort, Snapshot) %>%
            # make sure that y happened after x (i.e. time_units_from_x_to_y is positie)
            summarise(converted_within_threshold=sum(time_units_from_x_to_y > 0 & time_units_from_x_to_y <= Snapshot, na.rm = TRUE) / n()) %>%
            ungroup() %>%
            mutate(cohort = ymd(cohort),
                   Snapshot = factor(paste(Snapshot, snapshot_units),
                                     levels = paste((sort(snapshots)), snapshot_units),
                                     ordered = TRUE))

    } else {

        symbol_group <- sym(group_variable)
        snapshotted_conversions <- dataset %>%
            # e.g. "# days from x to y" where x and y are first and second date
            mutate(time_units_from_x_to_y = rt_difftime_numeric(!!symbol_second_date,
                                                                !!symbol_first_date,
                                                                units = snapshot_units)) %>%
            mutate(cohort = as.character(floor_date(x=!!symbol_first_date, unit=date_floor, week_start=1))) %>%
            group_by(cohort, !!symbol_group) %>%
            mutate(youngest_age = rt_difftime_numeric(reference_date,
                                                      max(!!symbol_first_date),
                                                      units = snapshot_units)) %>%
            ungroup() %>%
            arrange(desc(!!symbol_first_date)) %>%
            #head() %>%
            crossing(snapshots) %>%
            rename(Snapshot=snapshots) %>%
            filter(youngest_age >= Snapshot) %>%
            group_by(cohort, !!symbol_group, Snapshot) %>%
            # make sure that y happened after x (i.e. time_units_from_x_to_y is positie)
            summarise(converted_within_threshold=sum(time_units_from_x_to_y > 0 & time_units_from_x_to_y <= Snapshot, na.rm = TRUE) / n()) %>%
            ungroup() %>%
            mutate(cohort = ymd(cohort),
                   Snapshot = factor(paste(Snapshot, snapshot_units),
                                     levels = paste((sort(snapshots)), snapshot_units),
                                     ordered = TRUE))
    }

    return (snapshotted_conversions)
}

#' shows conversion rates (y-axis) of a particular cohort based on various 'snapshots' in time (a line is a snapshot in time).
#'
#' Each record in the cohort must have had enough time to convert, in order to be shown in the graph.
#' For example, say the snapshots we are interested in are at day 1 and day 7. If the cohort refers to
#'  February, and it is March 3rd, every record in the February cohort has had at least 1 day of "activity" or
#'  potential to convert, so we can include Feb for that snapshot. However, not everyone in Feb has had 7 days
#'  of activity, so we cannot yet include Feb for the 7 day snapshot, because people who joined the cohort on
#'  Feb 28, for example, have not had a full 7 days of potential activity.
#'
#' @param dataset dataframe
#' @param first_date the reference date (e.g. first-touch point)
#' @param second_date the date of conversion, NA if not converted
#' @param group_variable a variable to group/facet by. If `color_or_facet` will be ignored
#' @param reference_date we need to know how old the cohort is so we can determine if the
#' @param snapshots the numeric snapshots
#' @param snapshot_units the units of the snapshots e.g. `hours`, `days`, `weeks`
#' @param date_floor how we should define the cohort group e.g. by `day`, `by week`, by `months`
#' @param color_or_facet display snaphosts by `color` or `facet`
#' @param year_over_year if true it displays the graph year-over-year; `color_or_facet` will be ignored
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param include_zero_y_axis expand the lower bound of the y-axis to 0 (TRUE is best practice.)
#' @param show_points if TRUE adds points to the graph
#' @param show_labels if TRUE adds labels to each point
#' @param date_break_format format of date breaks e.g. `'\%Y-\%m-\%d'`
#' @param date_breaks_width the date breaks for x axis, values correspond to ggplot scale_x_date e.g. "1 month", "1 week"
#' @param date_limits "zoom" for date x-axis, 2 values representing min/max in the format of YYYY-MM-DD. If `date_floor` is used, the date_limits are converted to the corresponding date floor
#'       e.g. if date_floor is 'month' and date_limits are `c('2013-01-15', '2013-12-15')` they will be converted to `c('2013-01-01', '2013-12-01')`
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 labs
#' @export
rt_explore_plot_conversion_rates <- function(dataset,
                                             first_date,
                                             second_date,
                                             reference_date,
                                             group_variable=NULL,
                                             snapshots=c(1, 7, 14),
                                             snapshot_units='days',
                                             date_floor='month',
                                             color_or_facet='color',
                                             year_over_year=FALSE,
                                             y_zoom_min=NULL,
                                             y_zoom_max=NULL,
                                             include_zero_y_axis=TRUE,
                                             show_points=FALSE,
                                             show_labels=FALSE,
                                             date_break_format=NULL,
                                             date_breaks_width=NULL,
                                             date_limits=NULL,
                                             base_size=11
                                             ) {

    rt_stopif(!is.null(group_variable) && year_over_year)
    stopifnot(color_or_facet %in% c('color', 'facet'))

    snapshotted_conversions <- rt_get_cohorted_conversion_rates(dataset=dataset,
                                                                first_date=first_date,
                                                                second_date=second_date,
                                                                reference_date=reference_date,
                                                                group_variable=group_variable,
                                                                snapshots=snapshots,
                                                                snapshot_units=snapshot_units,
                                                                date_floor=date_floor)
    if(is.null(group_variable)) {

        if(color_or_facet == 'color' && !year_over_year) {

            color_variable <- 'Snapshot'
            facet_variable <- NULL

        } else {
            color_variable <- NULL
            facet_variable <- 'Snapshot'
        }
        subtitle <- NULL

    } else {

        subtitle <- paste0("by `", group_variable,"`")

        if(color_or_facet == 'color') {

            color_variable <- 'Snapshot'
            facet_variable <- group_variable

        } else if (color_or_facet == 'facet') {

            color_variable <- group_variable
            facet_variable <- 'Snapshot'

        } else {

            stopifnot(FALSE)
        }
    }

    rt_explore_plot_time_series(dataset=snapshotted_conversions,
                                variable='cohort',
                                comparison_variable='converted_within_threshold',
                                comparison_function=function(x) {x},
                                comparison_function_name='xxx',
                                color_variable=color_variable,
                                facet_variable=facet_variable,
                                year_over_year=year_over_year,
                                y_zoom_min=y_zoom_min,
                                y_zoom_max=y_zoom_max,
                                include_zero_y_axis=include_zero_y_axis,
                                show_points=show_points,
                                show_labels=show_labels,
                                date_floor=date_floor,
                                date_break_format=date_break_format,
                                date_breaks_width=date_breaks_width,
                                format_as_percent=TRUE,
                                date_limits=date_limits,
                                base_size=base_size) +
        labs(title=paste0("Conversion Rate from `", first_date, "` to `", second_date, "`"),
             subtitle = subtitle,
             y="Conversion Rate",
             x=paste0(first_date, " (", date_floor, ")"),
             caption=paste0("\nShows the conversion rates (y-axis) of a particular cohort (x-axis) based on various 'snapshots' in time\nrelative to X ",
                            snapshot_units, " after the corresponding ", first_date, "."))
}

#' creates a cohorted option graph that e.g. shows the adoption/conversion rate over time (e.g. days)
#'
#' @param dataset dataframe
#' @param first_date the reference date (e.g. first-touch point)
#' @param second_date the date of conversion, NA if not converted
#' @param reference_date we need to know how old the cohort is so we can determine if the
#' @param last_n_cohorts the number of cohorts to include
#' @param n_units_after_first_date `n` refers to the number of, for example, *days* of adoption from the first-touch, where `days` is represented in the units parameter
#' @param units see description for `n_units_after_first_date`
#' @param date_floor how we should define the cohort group e.g. by `day`, `by week`, by `months`
#' @param separated_colors if TRUE, returns the default color scheme that arranges sequential colors such that they are not close in color i.e. they are "separated". `FALSE` gives a smoother gradient of colors.
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param include_zero_y_axis expand the lower bound of the y-axis to 0 (TRUE is best practice.)
#' @param show_points if TRUE adds points to the graph
#' @param show_labels if TRUE adds labels to each point
#' @param date_break_format format of date breaks e.g. `'\%Y-\%m-\%d'`
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate group_by ungroup summarise filter bind_rows
#' @importFrom stringr str_detect
#' @importFrom scales date_format pretty_breaks format_format percent_format
#' @importFrom ggplot2 ggplot aes geom_line geom_text geom_point labs expand_limits theme_light coord_cartesian scale_color_manual scale_x_continuous scale_y_continuous
#' @importFrom lubridate floor_date
#' @export
rt_explore_plot_cohorted_adoption <- function(dataset,
                                              first_date,
                                              second_date,
                                              reference_date,
                                              last_n_cohorts=10,
                                              n_units_after_first_date=30,
                                              units='days',
                                              date_floor='month',
                                              separated_colors=TRUE,
                                              y_zoom_min=NULL,
                                              y_zoom_max=NULL,
                                              include_zero_y_axis=TRUE,
                                              show_points=FALSE,
                                              show_labels=FALSE,
                                              date_break_format=NULL,
                                              base_size=11) {
    symbol_first_date <- sym(first_date)
    symbol_second_date <- sym(second_date)

    if(is.null(date_break_format)) {
        if(str_detect(date_floor, 'week')) {

            date_break_format <- '%Y-%m-%d'

        } else if (str_detect(date_floor, 'month')) {

            date_break_format <- '%Y-%m'

        } else if (str_detect(date_floor, 'quarter')) {

            date_break_format <- '%Y-%m'

        } else if (str_detect(date_floor, 'year')) {

            date_break_format <- '%Y'

        } else {

            date_break_format <- '%Y-%m-%d'
        }
    }

    dataset <- dataset %>%
        mutate(time_units_from_x_to_y = rt_difftime_numeric(!!symbol_second_date,
                                                            !!symbol_first_date,
                                                            units=units))
    adoption_df <- data.frame()
    # unit_index is e.g. the nth day after someone signups where
    # `nth` is n_units_after_first_date
    # `day` is units
    # `signgup` is first_date
    for(unit_index in 1:n_units_after_first_date) {
        # unit_index <- 30
        adoption_df <- bind_rows(adoption_df,
            dataset %>%
            mutate(cohort=private__custom_date_format(date_floor, date_break_format)(floor_date(x=!!symbol_first_date,
                                                                    unit=date_floor,
                                                                    week_start=1))) %>%
            group_by(cohort) %>%
            mutate(youngest_age=rt_difftime_numeric(reference_date,
                                                    max(!!symbol_first_date),
                                                    units=units)) %>%
            ungroup() %>%
            filter(youngest_age >= unit_index) %>%
            group_by(cohort) %>%
            summarise(converted_within_threshold=sum(time_units_from_x_to_y > 0 & time_units_from_x_to_y <= unit_index,
                                                     na.rm = TRUE) / n()) %>%
            ungroup() %>%
            mutate(day=unit_index))
    }

    last_n_cohorts <- min(last_n_cohorts, 20)  # max out at 20

    ggplot_object <- adoption_df %>%
        filter(cohort %in% tail(sort(unique(adoption_df$cohort)), n = last_n_cohorts)) %>%
        ggplot(aes(x=day, y=converted_within_threshold, color=cohort)) +
        geom_line() +
        scale_x_continuous(breaks=pretty_breaks(10),
                           labels = format_format(big.mark=",",
                                                  preserve.width="none",
                                                  digits=4,
                                                  scientific=FALSE)) +
        scale_y_continuous(breaks=pretty_breaks(10), labels=percent_format()) +
        theme_light(base_size=base_size) +
        labs(title=paste0("Adoption from `", first_date, "` to `", second_date, "`"),
             y="Conversion Rate",
             x=paste0("# of ", units, " from `", first_date, "`"),
             color=paste0("Cohort (", date_floor,")"),
             caption=paste0("\nEach cohort represents the ", date_floor, " of `", first_date, "`."))

    if(separated_colors) {

        ggplot_object <- ggplot_object + scale_color_manual(values=c(rt_colors(),rt_colors()))
    }

    if(include_zero_y_axis) {

        ggplot_object <- ggplot_object + expand_limits(y=0)
    }

    if(show_points) {

        ggplot_object <- ggplot_object + geom_point()
    }

    if(show_labels) {

        ggplot_object <- ggplot_object +
            geom_text(aes(label = percent_format()(converted_within_threshold)),
                      check_overlap = TRUE,
                      vjust=-0.5)
    }
    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(adoption_df$converted_within_threshold, na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(adoption_df$converted_within_threshold, na.rm = TRUE)
        }

        ggplot_object <- ggplot_object +
            coord_cartesian(ylim = c(y_zoom_min, y_zoom_max))
    }

    return (ggplot_object)
}

#' only returns a value for dates matching quarter so that when used with scale_x_date there are no axis
#' ticks for anything other than quarters
#'
#' @param x a vector of dates
#'
#' @importFrom zoo as.yearqtr
#' @importFrom stringr str_replace
#' @export
rt_as_year_qtr_format <- function(x) {

    quarter_floor_values <- floor_date(x, unit='quarters')
    quarters_converted <- as.character(as.yearqtr(x))
    # formats for as.yearqtr (e.g. "%Y-%q") don't seem to insert `-`, so need to do it manually
    quarters_converted <- str_replace(string=quarters_converted, pattern = " ", replacement = "-")
    return (ifelse(quarter_floor_values == x, quarters_converted, ""))
}

##############################################################################################################
# private helpers
##############################################################################################################
private__create_bar_chart_comparison_var <- function(groups_by_variable,
                                                     groups_by_both,
                                                     variable,
                                                     comparison_variable,
                                                     facet_variable,
                                                     count_distinct_variable,
                                                     view_type,
                                                     show_dual_axes,
                                                     reverse_stack,
                                                     show_variable_totals,
                                                     show_comparison_totals,
                                                     plot_y_second_axis_label,
                                                     plot_title,
                                                     plot_subtitle,
                                                     plot_y_axis_label,
                                                     base_size) {

    stopifnot(view_type %in% c("Bar", "Stack", "Stack Percent"))

    symbol_variable <- sym(variable)
    symbol_comparison_variable <- sym(comparison_variable)


    # if(view_type == "Facet by Comparison") {

    #     # if we Facet, the colors will be based on the primary variable
    #     custom_colors <- rt_get_colors_from_values(groups_by_variable[[variable]])

    #     facet_groups <- suppressWarnings(groups_by_both %>%
    #         select(-actual_percent, -group_percent) %>%
    #         group_by(!!symbol_comparison_variable) %>%
    #         mutate(facet_percent = total / sum(total)) %>%
    #         mutate(total_facet_percent = sum(facet_percent),
    #                facet_group_total = sum(total)) %>%
    #         ungroup())
    #     # make sure total_facet_percent values are all 1 (out to 7 digits)
    #     stopifnot(rt_are_numerics_equal(n1=facet_groups$total_facet_percent, n2=1, num_decimals=7))

    #     unique_values_plot <- ggplot(data=facet_groups,
    #                                  aes(x=!!symbol_variable, y=total, fill=!!symbol_variable)) +
    #         geom_bar(stat = 'identity', alpha=0.75) +
    #         facet_wrap(as.formula(paste("~", comparison_variable)), ncol = 1, scales = 'free_y') +
    #         scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",",
    #                                                                             preserve.width="none",
    #                                                                             digits=4,
    #                                                                             scientific=FALSE))

    #     if(show_variable_totals) {

    #         unique_values_plot <- unique_values_plot +
    #             geom_text(aes(x=!!symbol_variable, label = private__standard_prettyNum(total),
    #                           y = total),
    #                       vjust=1.25, check_overlap=TRUE)

    #         if(is.null(count_distinct_variable)) {

    #             unique_values_plot <- unique_values_plot +
    #                 geom_text(aes(x=!!symbol_variable, label = percent(facet_percent), y = total),
    #                           vjust=-0.25, check_overlap=TRUE)
    #         }
    #     }

    #     return (unique_values_plot +
    #                 labs(title = plot_title,
    #                      y=plot_y_axis_label,
    #                      fill=comparison_variable,
    #                      x=variable) +
    #                 scale_fill_manual(values=custom_colors, na.value = '#2A3132') +
    #                 theme_light(base_size = base_size) +
    #                 theme(legend.position = 'none',
    #                       axis.text.x = element_text(angle = 30, hjust = 1)))
    # } else {

        # if we Stack/Bar, the colors will be based on the comparison variable
        custom_colors <- rt_get_colors_from_values(groups_by_both[[comparison_variable]])

        if(view_type == "Stack") {

            unique_values_plot <- ggplot() +
                geom_bar(data = groups_by_both,
                         aes(x = !!symbol_variable,
                             y = total,
                             fill = !!symbol_comparison_variable),
                         stat = 'identity',
                         position = position_stack(reverse=reverse_stack),
                         alpha=0.75)

        } else {

            if(view_type == "Stack Percent") {

                comparison_position <- position_fill(reverse=reverse_stack)
                # create the plot
                unique_values_plot <- ggplot() +
                    geom_bar(data = groups_by_both,
                             aes(x = !!symbol_variable,
                                 y = actual_percent,
                                 fill = !!symbol_comparison_variable),
                             stat = 'identity',
                             alpha=0.75,
                             position = comparison_position)
            } else {

                comparison_position <- position_dodge(width = 0.9)
                # create the plot
                unique_values_plot <- ggplot()

                if(is.null(count_distinct_variable)) {

                    unique_values_plot <- unique_values_plot +
                        geom_bar(data = groups_by_variable,
                                 aes(x = !!symbol_variable,
                                     y = total),
                                 stat = 'identity',
                                 position = 'dodge',
                                 alpha = 0.3)
                }
                unique_values_plot <- unique_values_plot +
                    geom_bar(data = groups_by_both,
                             aes(x = !!symbol_variable,
                                 y = total,
                                 fill = !!symbol_comparison_variable),
                             stat = 'identity',
                             position = comparison_position)
            }
        }

        if(show_dual_axes && view_type != "Stack" && view_type != "Stack Percent" && is.null(count_distinct_variable)) {

            unique_values_plot <- unique_values_plot +
                scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",",
                                                                                    preserve.width="none",
                                                                                    digits=4,
                                                                                    scientific=FALSE),
                                   sec.axis = sec_axis(~./sum(groups_by_variable$total),
                                                       breaks=pretty_breaks(10),
                                                       labels = percent_format(),
                                                       name=plot_y_second_axis_label))

        } else if(view_type == "Stack Percent") {

            unique_values_plot <- unique_values_plot +
                scale_y_continuous(breaks=pretty_breaks(10), labels = percent_format())

        } else {

            unique_values_plot <- unique_values_plot +
                scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",",
                                                                                    preserve.width="none",
                                                                                    digits=4,
                                                                                    scientific=FALSE))
        }

        # we will only show variable totals if show_variable_totals and the variable values aren't filled
        #(i.e. all 100%); and we aren't showing the primary variable totals when counting distinct values
        if(show_variable_totals && view_type != "Stack Percent" && is.null(count_distinct_variable)) {

            if(view_type == "Stack") {

                unique_values_plot <- unique_values_plot +
                    geom_text(data = groups_by_variable,
                              aes(x=!!symbol_variable,
                                  y = total,
                                  label = private__standard_prettyNum(total)),
                              vjust=-0.25, check_overlap=TRUE)

            } else {

                unique_values_plot <- unique_values_plot +
                    geom_text(data = groups_by_variable,
                              aes(x=!!symbol_variable,
                                  label = percent(percent),
                                  y = total),
                              vjust=-1.5, check_overlap=TRUE) +
                    geom_text(data = groups_by_variable,
                              aes(x=!!symbol_variable,
                                  label = private__standard_prettyNum(total),
                                  y = total),
                              vjust=-0.25, check_overlap=TRUE)

            }
        }

        if(show_comparison_totals && view_type != "Stack Percent") {

            if(view_type == "Stack") {

                unique_values_plot <- unique_values_plot +
                    geom_text(data = groups_by_both,
                              aes(x = !!symbol_variable,
                                  y = total,
                                  label = private__standard_prettyNum(total),
                                  group = !!symbol_comparison_variable),
                              position = position_stack(reverse=reverse_stack, vjust = .5),
                              check_overlap=TRUE)

            } else {

                unique_values_plot <- unique_values_plot +
                    geom_text(data = groups_by_both,
                              aes(x = !!symbol_variable,
                                  y = 0.5 * total,
                                  label = private__standard_prettyNum(total),
                                  group = !!symbol_comparison_variable),
                              position = comparison_position,
                              vjust=-0.25, check_overlap=TRUE)

                if(is.null(count_distinct_variable)) {
                    unique_values_plot <- unique_values_plot +
                        geom_text(data = groups_by_both %>% filter(!is.nan(group_percent)),
                                  aes(x = !!symbol_variable,
                                      y = 0.5 * total,
                                      label = percent(group_percent),
                                      group = !!symbol_comparison_variable),
                                  position = comparison_position,
                                  vjust=1.25, check_overlap=TRUE)
                }
            }
        } else if (show_comparison_totals && view_type == "Stack Percent") {
            #in this case, we don't want to show the totals of the main variable (they are all at 100%)
            unique_values_plot <- unique_values_plot +
                geom_text(data = groups_by_both %>% filter(!is.nan(group_percent)),
                          aes(x = !!symbol_variable,
                              y = group_percent,
                              label = percent(group_percent),
                              group = !!symbol_comparison_variable),
                          position = position_fill(reverse=reverse_stack, vjust = .5),
                          check_overlap=TRUE)
        }

        if(!is.null(facet_variable)) {

            unique_values_plot <- unique_values_plot +
                facet_wrap(as.formula(paste("~", facet_variable)), ncol = 1, scales = 'free_y') +
                coord_cartesian(clip = "off")
        }

        return (unique_values_plot +
                    labs(title = plot_title,
                         y=plot_y_axis_label,
                         fill = comparison_variable,
                         x = variable) +
                    scale_fill_manual(values=custom_colors, na.value = '#2A3132') +
                    theme_light(base_size = base_size) +
                    theme(axis.text.x = element_text(angle = 30, hjust = 1)))
    # }
}

private__create_bar_chart_single_var <- function(groups_by_variable,
                                                 variable,
                                                 facet_variable,
                                                 simple_mode,
                                                 show_dual_axes,
                                                 plot_y_second_axis_label,
                                                 show_variable_totals,
                                                 plot_title,
                                                 plot_y_axis_label,
                                                 base_size) {

    symbol_variable <- sym(variable)

    if(simple_mode) {

        custom_colors <- rep(rt_colors()[1], length(unique(groups_by_variable[[variable]])))

    } else {

        custom_colors <- rt_get_colors_from_values(groups_by_variable[[variable]])
    }

    unique_values_plot <- groups_by_variable %>%
        ggplot(aes(x=!!symbol_variable, y=total, fill=!!symbol_variable)) +
        geom_bar(stat = 'identity', alpha=0.75)

    if(show_dual_axes) {

        unique_values_plot <- unique_values_plot +
            scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",",
                                                                                preserve.width="none",
                                                                                digits=4,
                                                                                scientific=FALSE),
                               sec.axis = sec_axis(~./sum(groups_by_variable$total),
                                                   breaks=pretty_breaks(10),
                                                   labels = percent_format(),
                                                   name=plot_y_second_axis_label))
    } else {

        unique_values_plot <- unique_values_plot +
            scale_y_continuous(breaks=pretty_breaks(10), labels = format_format(big.mark=",",
                                                                                preserve.width="none",
                                                                                digits=4,
                                                                                scientific=FALSE))
    }

    if(show_variable_totals) {

        unique_values_plot <- unique_values_plot +
            geom_text(aes(label = private__standard_prettyNum(total), y = total),
                      vjust=1.25, check_overlap=TRUE)

        if(!simple_mode) {
            unique_values_plot <- unique_values_plot +
                geom_text(aes(label = percent(percent), y = total), vjust=-0.25, check_overlap=TRUE)
        }
    }

    if(!is.null(facet_variable)) {

        unique_values_plot <- unique_values_plot +
            facet_wrap(as.formula(paste("~", facet_variable)), ncol = 1, scales = 'free_y') +
            coord_cartesian(clip = "off")
    }

    return (
        unique_values_plot +
            labs(title=plot_title,
                 y=plot_y_axis_label,
                 x=variable) +
            scale_fill_manual(values=custom_colors, na.value = '#2A3132') +
            theme_light(base_size = base_size) +
            theme(legend.position = 'none',
                  axis.text.x = element_text(angle = 30, hjust = 1))
    )
}

private__standard_prettyNum <- function(x) {
    return (prettyNum(x,
                      big.mark=",",
                      preserve.width="none",
                      digits=4,
                      scientific=FALSE))
}

private__custom_date_format <- function(date_floor, date_break_format) {

    if(is.null(date_floor) || date_floor != 'quarter') {

        return (date_format(date_break_format))

    } else {  # not NULL && quarter

        return (rt_as_year_qtr_format)
    }
}
