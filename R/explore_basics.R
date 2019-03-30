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
                                         base_size=11) {

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

    totals <- temp %>%
        group_by(value) %>%
        summarise(count = sum(weight, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(percent = count / denominator) %>%
        arrange(desc(count), value)

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
#' @param stacked_comparison rather than side-by-side bars for the comparison variable, the bars are stacked within the main variable
#' @param multi_value_delimiter if the variable contains multiple values (e.g. "A", "A, B", ...) then setting
#'      this variable to the delimiter will cause the function to count seperate values

#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise mutate ungroup arrange n
#' @importFrom scales percent_format comma_format percent
#' @importFrom ggplot2 ggplot aes aes geom_bar scale_y_continuous geom_text labs theme_gray theme element_text position_dodge
#' @export
rt_explore_plot_value_totals <- function(dataset,
                                         variable,
                                         comparison_variable=NULL,
                                         sum_by_variable=NULL,
                                         order_by_count=TRUE,
                                         show_variable_totals=TRUE,
                                         show_comparison_totals=TRUE,
                                         stacked_comparison=FALSE,
                                         multi_value_delimiter=NULL,
                                         base_size=11) {

    symbol_variable <- sym(variable)  # because we are using string variables

    if(is.null(sum_by_variable)) {

        plot_title <- paste0('Value Counts - `', variable, '`')
        plot_y_axis_label <- 'Percent of Dataset Containing Value'
        groups_by_variable <- rt_explore_value_totals(dataset=dataset,
                                                      variable=variable,
                                                      multi_value_delimiter=multi_value_delimiter) %>%
            rename(total=count)

    } else {

        plot_title <- paste0('Sum of `', sum_by_variable, '` by `', variable, '`')
        plot_y_axis_label <- 'Percent of Total Amount'
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
            ggplot(aes(x=!!symbol_variable, y = percent, fill=!!symbol_variable)) +
                geom_bar(stat = 'identity') +
                scale_y_continuous(labels = percent_format())

        if(show_variable_totals) {

            unique_values_plot <- unique_values_plot +
                geom_text(aes(label = percent(percent), y = percent), vjust=-1.5) +
                geom_text(aes(label = comma_format()(total), y = percent), vjust=-0.25)
        }

        return (
            unique_values_plot +
                labs(title=plot_title,
                     y=plot_y_axis_label,
                     x=variable) +
                theme_gray(base_size = base_size) +
                theme(legend.position = 'none',
                      axis.text.x = element_text(angle = 30, hjust = 1)))

    } else {

        symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables
        plot_title <- paste0(plot_title, ' against `', comparison_variable, '`')

        if(is.null(sum_by_variable)) {

            groups_by_both <- dataset %>%
                    group_by(!!symbol_variable, !!symbol_comparison_variable) %>%
                    summarise(total = n(), actual_percent=total / nrow(dataset))

        } else {

            symbol_sum_by <- sym(sum_by_variable)  # because we are using string variables
            groups_by_both <- dataset %>%
                    group_by(!!symbol_variable, !!symbol_comparison_variable) %>%
                    summarise(total = sum(!!symbol_sum_by, na.rm=TRUE)) %>%
                    ungroup() %>%
                    mutate(actual_percent=total / sum(total, na.rm = TRUE))
        }

        groups_by_both <- as.data.frame(groups_by_both %>%
                group_by(!!symbol_variable) %>%
                mutate(group_percent = total / sum(total, na.rm=TRUE)) %>%
                ungroup() %>%
                arrange(!!symbol_variable, !!symbol_comparison_variable))

        if(order_by_count) {

            groups_by_variable[, variable] <- factor(groups_by_variable[, variable],
                                                     levels = as.character(groups_by_variable[, variable]))
            groups_by_both[, variable] <- factor(groups_by_both[, variable],
                                                     levels = as.character(groups_by_variable[, variable]))
        }

        if(stacked_comparison) {

            comparison_position <- 'fill'

        } else {

            comparison_position <- 'dodge'
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


        # we will only show variable totals if show_variable_totals and the variable values aren't filled
        #(i.e. all 100%)
        if(show_variable_totals && !stacked_comparison) {

            unique_values_plot <- unique_values_plot +
                geom_text(data = groups_by_variable,
                          aes(x=!!symbol_variable, label = percent(percent), y = percent + 0.01),
                          vjust=-1) +
                geom_text(data = groups_by_variable,
                          aes(x=!!symbol_variable, label = total, y = percent + 0.01),
                          vjust=0.5)
        }

        if(show_comparison_totals && !stacked_comparison) {

            unique_values_plot <- unique_values_plot +
                geom_text(data = groups_by_both,
                          aes(x = !!symbol_variable,
                              y = actual_percent,
                              label = total,
                              group = !!symbol_comparison_variable),
                          position = position_dodge(width = 1),
                          vjust = -0.2) +
                geom_text(data = groups_by_both,
                          aes(x = !!symbol_variable,
                              y = actual_percent,
                              label = percent(group_percent),
                              group = !!symbol_comparison_variable),
                          position = position_dodge(width = 1),
                          vjust = -1.5)

        } else if (show_comparison_totals && stacked_comparison) {
            #in this case, we don't want to show the totals of the main variable (they are all at 100%)
            unique_values_plot <- unique_values_plot +
                geom_text(data = groups_by_both,
                          aes(x = !!symbol_variable,
                              y = group_percent,
                              label = percent(group_percent),
                              group = !!symbol_comparison_variable),
                          position = position_stack(vjust = .5))
        }

        return (unique_values_plot +
            labs(title = plot_title,
                 fill = comparison_variable,
                 x = variable) +
            theme_gray(base_size = base_size) +
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
#' @importFrom ggplot2 ggplot aes geom_boxplot scale_x_discrete xlab ylab theme_gray theme element_text coord_cartesian
#' @importFrom scales comma_format
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
            scale_y_continuous(labels = comma_format()) +
            scale_x_discrete(breaks = NULL) +
            xlab(NULL) +
            ylab(variable) +
            theme_gray(base_size = base_size)

    } else {

        symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables
        boxplot_plot <- ggplot(dataset,
                               aes(y=!!symbol_variable,
                                   x=!!symbol_comparison_variable,
                                   color=!!symbol_comparison_variable)) +
            scale_y_continuous(labels = comma_format()) +
            geom_boxplot() +
            xlab(comparison_variable) +
            ylab(variable) +
            theme_gray(base_size = base_size) +
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
#' @param num_bins the number of bins that the histogram will use
#' @param x_zoom_min adjust (i.e. zoom in) to the x-axis; sets the minimum x-value for the adjustment
#' @param x_zoom_max adjust (i.e. zoom in) to the x-axis; sets the maximum x-value for the adjustment
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes aes geom_histogram geom_freqpoly geom_density labs theme_gray coord_cartesian
#' @export
rt_explore_plot_histogram <- function(dataset,
                                      variable,
                                      comparison_variable=NULL,
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
            theme_gray(base_size = base_size)
    } else {

        symbol_comparison_variable <- sym(comparison_variable)  # because we are using string variables
        histogram_plot <- ggplot(dataset, aes(x=!!symbol_variable,
                                              color=!!symbol_comparison_variable)) +
            geom_freqpoly(binwidth= 100 / num_bins) +
            labs(title=paste0('Distribution of `', variable, '` by `', comparison_variable, '`'),
                 x=variable,
                 color=comparison_variable) +
            theme_gray(base_size = base_size)
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
#' @param variable a numeric variable (x-axis)
#' @param comparison_variable the additional numeric variable (y-axis)
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
#' @importFrom ggplot2 ggplot aes geom_point theme_gray coord_cartesian geom_jitter position_jitter scale_y_continuous
#' @importFrom scales comma_format
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

    scatter_plot <- ggplot(dataset, aes(x=!!symbol_variable,
                                        y=!!symbol_comparison_variable,
                                        color=!!symbol_color_variable,
                                        size=!!symbol_size_variable))

    if(jitter) {

        scatter_plot <- scatter_plot + geom_jitter(alpha=alpha, position=position_jitter(seed=42))

    } else {

        scatter_plot <- scatter_plot + geom_point(alpha=alpha)
    }

    scatter_plot <- scatter_plot +
        scale_y_continuous(labels = comma_format()) +
        theme_gray(base_size = base_size) +
        labs(x=variable,
             y=comparison_variable)


    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(x_zoom_min) || !rt_is_null_na_nan(x_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(x_zoom_min)) {

            x_zoom_min <- min(dataset[, variable], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(x_zoom_max)) {

            x_zoom_max <- max(dataset[, variable], na.rm = TRUE)
        }

        scatter_plot <- scatter_plot +
            coord_cartesian(xlim = c(x_zoom_min, x_zoom_max))
    }

    # zoom in on graph is parameters are set
    if(!rt_is_null_na_nan(y_zoom_min) || !rt_is_null_na_nan(y_zoom_max)) {
        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max

        if(rt_is_null_na_nan(y_zoom_min)) {

            y_zoom_min <- min(dataset[, comparison_variable], na.rm = TRUE)
        }

        if(rt_is_null_na_nan(y_zoom_max)) {

            y_zoom_max <- max(dataset[, comparison_variable], na.rm = TRUE)
        }

        scatter_plot <- scatter_plot +
            coord_cartesian(ylim = c(y_zoom_min, y_zoom_max))
    }

    return (scatter_plot)
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
#' @param base_size uses ggplot's base_size parameter for controling the size of the text
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr count group_by summarise rename
#' @importFrom ggplot2 ggplot aes labs geom_line expand_limits theme_gray theme element_text coord_cartesian
#' @export
rt_explore_plot_time_series <- function(dataset,
                                        variable,
                                        comparison_variable=NULL,
                                        comparison_function=NULL,
                                        comparison_function_name=NULL,
                                        color_variable=NULL,
                                        y_zoom_min=NULL,
                                        y_zoom_max=NULL,
                                        base_size=11) {

    # if using a comparison variable, we must also have a function and function name
    stopifnot(!(!is.null(comparison_variable) &&
        (is.null(comparison_function) || is.null(comparison_function_name))))

    sym_variable <- sym(variable)  # because we are using string variables

    symbol_if_not_null <- function(x) {
        if (is.null(x)) {

            return (NULL)

        } else {

            return (sym(x))
        }
    }
    sym_comparison_variable <- symbol_if_not_null(comparison_variable)
    sym_color_variable <- symbol_if_not_null(color_variable)

    if(is.null(sym_comparison_variable)) {

        if(is.null(sym_color_variable)) {

            dataset <- dataset %>% count(!!sym_variable) %>% rename(total=n)

        } else {

            dataset <- dataset %>% count(!!sym_variable, !!sym_color_variable) %>% rename(total=n)
        }
        ggplot_object <- dataset %>%
            ggplot(aes(x=!!sym_variable, y=total, color=!!sym_color_variable)) +
            labs(title='Count of Records',
                 x=variable,
                 y='Count')

    } else {
        if(is.null(sym_color_variable)) {
            dataset <- dataset %>%
                group_by(!!sym_variable) %>%
                summarise(total=comparison_function(!!sym_comparison_variable))

        } else {

            dataset <- dataset %>%
                group_by(!!sym_variable, !!sym_color_variable) %>%
                summarise(total=comparison_function(!!sym_comparison_variable))
        }
        ggplot_object <- dataset %>%
            ggplot(aes(x=!!sym_variable, y=total, color=!!sym_color_variable)) +
            labs(title=paste(comparison_function_name, 'of', comparison_variable, 'by', variable),
                 x=variable,
                 y=paste(comparison_function_name, comparison_variable))
    }
    ggplot_object <- ggplot_object +
        geom_line() +
        expand_limits(y=0) +
        theme_gray(base_size = base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

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
