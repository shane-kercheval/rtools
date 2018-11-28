context('Exploratory Analysis')
library(testthat)
library(ggplot2)

test_that("rt_explore_categoric_summary_NAs", {

    temp_iris <- iris
    # make sure it works with NA
    temp_iris$Petal.Width <- rep(NA, nrow(temp_iris))

    results <- rt_explore_categoric_summary(dataset=temp_iris)

    expect_true(rt_are_dataframes_equal_from_file(dataframe1=results,
                                                  rds_file='data/rt_explore_numeric_summary_iris_missing.RDS'))
})

test_that("rt_explore_numeric_summary", {

    temp_iris <- iris
    temp_iris[1, 'Sepal.Width'] <- NA
    temp_iris[2, 'Sepal.Width'] <- NA
    temp_iris[3, 'Sepal.Width'] <- NA
    temp_iris[1, 'Petal.Length'] <- NA
    temp_iris[1, 'Sepal.Length'] <- 0
    temp_iris[2, 'Sepal.Length'] <- 0
    temp_iris[3, 'Sepal.Length'] <- 0
    temp_iris[1, 'Petal.Width'] <- 0

    result <- rt_explore_numeric_summary(dataset=temp_iris)

    rds_file <- 'data/rt_explore_numeric_summary_iris.RDS'
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=result, rds_file=rds_file))
})

test_that("rt_explore_categoric_summary", {

    temp_iris <- iris
    temp_iris[1, 'Species'] <- NA
    temp_iris[2, 'Species'] <- NA
    temp_iris[3, 'Species'] <- NA

    result <- rt_explore_categoric_summary(dataset=temp_iris)

    rds_file <- 'data/rt_explore_categoric_summary_iris.RDS'
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=result, rds_file=rds_file))
})

test_that("rt_explore_correlations_credit", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    # make sure it handles NAs
    credit_data[1, 'months_loan_duration'] <- NA

    # default parameters
    correlations <- rt_explore_correlations(dataset=credit_data)

    rds_file <- 'data/rt_correlations_credit.RDS'
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=data.frame(correlations), rds_file=rds_file))

    # use correlation parameters from above
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data))

     # pretty
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit_pretty.png',
                   plot=rt_explore_plot_correlations(dataset=rt_pretty_dataset(credit_data)))

    # change base_size
    test_save_plot(file_name='data/rt_explore_plot_correlations_base_size.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data, base_size=16))

    # lower p_value_threshold
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit_pvalue.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data,
                   p_value_threshold=0.3))

    # lower p_value_threshold
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit_corr_treshold.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data,
                                                     corr_threshold=0.115))

    # lower p_value_threshold
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit_both_parameters.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data,
                                                     corr_threshold=0.115,
                                                     p_value_threshold=0.3))
})


test_that("rt_explore_value_totals_counts", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)

    ##########################################################################################################
    # test with factor
    # change the levels to verify that the original levels are retained if order_by_count==FALSE
    ##########################################################################################################
    custom_levels <- c('< 0 DM', '1 - 200 DM', '> 200 DM', 'unknown')
    credit_data$checking_balance <- factor(credit_data$checking_balance, levels=custom_levels)
    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA

    variable <- 'checking_balance'

    unique_values <- rt_explore_value_totals(dataset=credit_data, variable=variable)

    expect_true(all(colnames(unique_values) == c('checking_balance', 'count', 'percent')))
    expect_true(all(levels(unique_values$checking_balance) == custom_levels))

    expect_true(all(unique_values$checking_balance[1:4] == c('unknown', '< 0 DM', '1 - 200 DM', '> 200 DM')))
    expect_true(is.na(unique_values$checking_balance[5]))
    expect_true(all(unique_values$count == c(394, 273, 269, 63, 1)))
    expect_true(all(unique_values$percent == c(0.394, 0.273, 0.269, 0.063, 0.001)))

    ##########################################################################################################
    # test without factor
    ##########################################################################################################
    credit_data$checking_balance <- as.character(credit_data$checking_balance)
    unique_values <- rt_explore_value_totals(dataset=credit_data, variable=variable)

    expect_true(all(colnames(unique_values) == c('checking_balance', 'count', 'percent')))

    # this is the only thing that should change
    expect_true(is.null(levels(unique_values$checking_balance)))

    # all of these should remain the same
    expect_true(all(unique_values$checking_balance[1:4] == c('unknown', '< 0 DM', '1 - 200 DM', '> 200 DM')))
    expect_true(is.na(unique_values$checking_balance[5]))
    expect_true(all(unique_values$count == c(394, 273, 269, 63, 1)))
    expect_true(all(unique_values$percent == c(0.394, 0.273, 0.269, 0.063, 0.001)))
})

test_that("rt_explore_value_totals_sums", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    variable <- 'checking_balance'
    sum_by <- 'amount'

    custom_levels <- c('< 0 DM', '1 - 200 DM', '> 200 DM', 'unknown')
    credit_data$checking_balance <- factor(credit_data$checking_balance, levels=custom_levels)
    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA
    credit_data[2, 'amount'] <- NA
    credit_data[3, 'checking_balance'] <- NA
    credit_data[3, 'amount'] <- NA

    value_sums <- rt_explore_value_totals(dataset=credit_data, variable=variable, sum_by_variable=sum_by)

    expect_true(all(colnames(value_sums) == c('checking_balance', 'sum', 'percent')))
    expect_true(all(levels(value_sums$checking_balance) == custom_levels))

    expect_true(all(value_sums$checking_balance[1:4] == c('unknown', '1 - 200 DM', '< 0 DM', '> 200 DM')))
    expect_true(is.na(value_sums$checking_balance[5]))

    expected_sums <- c(1232346, 1023663, 868841, 137192, 1169)
    expect_true(all(value_sums$sum == expected_sums))
    expect_true(all(value_sums$percent == expected_sums / sum(expected_sums)))
    expect_true(sum(value_sums$percent) == 1)

    # change to character
    credit_data$checking_balance <- as.character(credit_data$checking_balance)
    value_sums <- rt_explore_value_totals(dataset=credit_data, variable=variable, sum_by_variable=sum_by)

    expect_true(all(colnames(value_sums) == c('checking_balance', 'sum', 'percent')))
    expect_true(all(levels(value_sums$checking_balance) == custom_levels))

    expect_true(all(value_sums$checking_balance[1:4] == c('unknown', '1 - 200 DM', '< 0 DM', '> 200 DM')))
    expect_true(is.na(value_sums$checking_balance[5]))
    expect_true(all(value_sums$sum == expected_sums))
    expect_true(all(value_sums$percent == expected_sums / sum(expected_sums)))
    expect_true(sum(value_sums$percent) == 1)
})

test_that("rt_explore_plot_value_counts", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    variable <- 'checking_balance'

    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA

    # plot without order
    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      order_by_count=FALSE,
                                                      base_size=11))

    # plot without order
    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_group_totals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      show_group_totals=FALSE,
                                                      base_size=11))

    # plot pretty
    test_save_plot(file_name='data/rt_explore_plot_value_counts_pretty.png',
                   plot=rt_explore_plot_value_totals(dataset=rt_pretty_dataset(credit_data),
                                                      variable=rt_pretty_text(variable),
                                                      order_by_count=FALSE,
                                                      base_size=11))

    # plot with order
    test_save_plot(file_name='data/rt_explore_plot_value_counts_with_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      order_by_count=TRUE,
                                                      base_size=11))

    ##########################################################################################################
    # test without factor
    ##########################################################################################################
    credit_data$checking_balance <- as.character(credit_data$checking_balance)

    # plot without order
    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_factor_no_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      order_by_count=FALSE,
                                                      base_size=11))

    # plot with order
    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_factor_with_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      order_by_count=TRUE,
                                                      base_size=11))
})

test_that("rt_explore_plot_value_counts_against_categorical", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)

    ##########################################################################################################
    # test with factor
    # change the levels to verify that the original levels are retained if order_by_count==FALSE
    ##########################################################################################################
    custom_levels <- c('< 0 DM', '1 - 200 DM', '> 200 DM', 'unknown')
    credit_data$checking_balance <- factor(credit_data$checking_balance, levels=custom_levels)

    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA
    variable <- 'checking_balance'

    # plot with labels
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comparison_variable_defaults.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      comparison_variable='default',
                                                      order_by_count=TRUE,
                                                      show_group_totals=TRUE,
                                                      show_comparison_totals=TRUE))

    # plot pretty
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comparison_variable_pretty.png',
                   plot=rt_explore_plot_value_totals(dataset=rt_pretty_dataset(credit_data),
                                                      variable=rt_pretty_text(variable),
                                                      comparison_variable=rt_pretty_text('default'),
                                                      order_by_count=TRUE,
                                                      show_group_totals=TRUE,
                                                      show_comparison_totals=TRUE))


    # plot with labels
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comparison_variable_not_order_by_count.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      comparison_variable='default',
                                                      order_by_count=FALSE,
                                                      show_group_totals=TRUE,
                                                      show_comparison_totals=TRUE))

    # plot with labels
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comp_var_not_show_group_totals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      comparison_variable='default',
                                                      order_by_count=FALSE,
                                                      show_group_totals=FALSE,
                                                      show_comparison_totals=TRUE))

    # plot with labels
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comp_var_not_show_comparison_totals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      comparison_variable='default',
                                                      order_by_count=FALSE,
                                                      show_group_totals=FALSE,
                                                      show_comparison_totals=FALSE))
})

test_that("rt_explore_plot_value_totals_sums", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA
    variable <- 'checking_balance'
    sum_by_variable <- 'amount'
    comparison_variable <- 'default'

    test_save_plot(file_name='data/rt_explore_plot_value_totals_sums_defaults.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     sum_by_variable=sum_by_variable))

    test_save_plot(file_name='data/rt_explore_plot_value_totals_sums_non_defaults.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=NULL,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=FALSE,
                                                     show_group_totals=FALSE,
                                                     show_comparison_totals=FALSE,
                                                     base_size=16))

    test_save_plot(file_name='data/rt_explore_plot_value_totals_sums_comparison_defaults.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=TRUE,
                                                     show_group_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     base_size=14))

    test_save_plot(file_name='data/rt_explore_plot_value_totals_sums_comparison_defaults_no_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=FALSE,
                                                     show_group_totals=TRUE,
                                                     show_comparison_totals=FALSE,
                                                     base_size=14))
})

test_that("rt_explore_plot_boxplot", {
    dataset <- read.csv("data/credit.csv", header=TRUE)
    variable <- 'months_loan_duration'
    comparison_variable <- 'default'

    test_save_plot(file_name='data/rt_explore_plot_boxplot_standard.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=NULL,
                                                  y_zoom_min=NULL,
                                                  y_zoom_max=NULL,
                                                  base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_standard_zoom_min.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=NULL,
                                                  y_zoom_min=20,
                                                  y_zoom_max=NULL,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_standard_zoom_max.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=NULL,
                                                  y_zoom_min=NULL,
                                                  y_zoom_max=40,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_standard_zoom_both.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=NULL,
                                                  y_zoom_min=20,
                                                  y_zoom_max=40,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_comparison.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  y_zoom_min=NULL,
                                                  y_zoom_max=NULL,
                                                  base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_comparison_pretty.png',
                   plot=rt_explore_plot_boxplot(dataset=rt_pretty_dataset(dataset),
                                                variable=rt_pretty_text(variable),
                                                comparison_variable=rt_pretty_text(comparison_variable),
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_comparison_zoom_min.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  y_zoom_min=20,
                                                  y_zoom_max=NA,  # Check NA
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_comparison_zoom_max.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  y_zoom_min=NA,  # Check NA
                                                  y_zoom_max=40,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_comparison_zoom_both.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  y_zoom_min=20,
                                                  y_zoom_max=40,
                                                  base_size=15))
})

test_that("rt_explore_plot_histogram", {
    dataset <- read.csv("data/credit.csv", header=TRUE)
    variable <- 'months_loan_duration'

    test_save_plot(file_name='data/rt_explore_plot_histogram_standard.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  num_bins=30,
                                                  x_zoom_min=NULL,
                                                  x_zoom_max=NULL,
                                                  base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_histogram_pretty.png',
                   plot=rt_explore_plot_histogram(dataset=rt_pretty_dataset(dataset),
                                                  variable=rt_pretty_text(variable),
                                                  num_bins=30,
                                                  x_zoom_min=NULL,
                                                  x_zoom_max=NULL,
                                                  base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_histogram_num_bins.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  num_bins=15,
                                                  x_zoom_min=NULL,
                                                  x_zoom_max=NULL,
                                                  base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_histogram_standard_zoom_min.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  num_bins=30,
                                                  x_zoom_min=20,
                                                  x_zoom_max=NULL,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_histogram_standard_zoom_max.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  num_bins=30,
                                                  x_zoom_min=NULL,
                                                  x_zoom_max=40,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_histogram_standard_zoom_both.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  num_bins=30,
                                                  x_zoom_min=20,
                                                  x_zoom_max=40,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_histogram_standard_zoom_both_num_bins.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  num_bins=15,
                                                  x_zoom_min=20,
                                                  x_zoom_max=40,
                                                  base_size=15))
})

test_that("rt_explore_plot_histogram_with_categoric_comparison", {
    dataset <- read.csv("data/credit.csv", header=TRUE)
    variable <- 'months_loan_duration'
    comparison_variable <- 'checking_balance'

    test_save_plot(file_name='data/rt_explore_plot_histogram_with_comp_standard.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  num_bins=30,
                                                  x_zoom_min=NULL,
                                                  x_zoom_max=NULL,
                                                  base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_histogram_with_comp_pretty.png',
                   plot=rt_explore_plot_histogram(dataset=rt_pretty_dataset(dataset),
                                                  variable=rt_pretty_text(variable),
                                                  comparison_variable=rt_pretty_text(comparison_variable),
                                                  num_bins=30,
                                                  x_zoom_min=NULL,
                                                  x_zoom_max=NULL,
                                                  base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_histogram_with_comp_num_bins.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  num_bins=15,
                                                  x_zoom_min=NULL,
                                                  x_zoom_max=NULL,
                                                  base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_histogram_with_comp_zoom_min.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  num_bins=30,
                                                  x_zoom_min=20,
                                                  x_zoom_max=NULL,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_histogram_with_comp_zoom_max.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  num_bins=30,
                                                  x_zoom_min=NULL,
                                                  x_zoom_max=40,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_histogram_with_comp_zoom_both.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  num_bins=30,
                                                  x_zoom_min=20,
                                                  x_zoom_max=40,
                                                  base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_histogram_with_comp_zoom_both_num_bins.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  num_bins=15,
                                                  x_zoom_min=20,
                                                  x_zoom_max=40,
                                                  base_size=15))

})


test_that("rt_explore_plot_scatterplot", {
    dataset <- read.csv("data/housing.csv", header=TRUE)
    variable <- 'median_income'
    comparison_variable <- 'median_house_value'

    test_save_plot(file_name='data/rt_explore_plot_scatter.png',
                    plot=rt_explore_plot_scatter(dataset=dataset,
                                                 variable=variable,
                                                 comparison_variable=comparison_variable,
                                                 alpha=0.3,
                                                 x_zoom_min=NULL,
                                                 x_zoom_max=NULL,
                                                 y_zoom_min=NULL,
                                                 y_zoom_max=NULL,
                                                 base_size=11))


    test_save_plot(file_name='data/rt_explore_plot_scatter_pretty.png',
                   plot=rt_explore_plot_scatter(dataset=rt_pretty_dataset(dataset),
                                                variable=rt_pretty_text(variable),
                                                comparison_variable=rt_pretty_text(comparison_variable),
                                                alpha=0.3,
                                                x_zoom_min=NULL,
                                                x_zoom_max=NULL,
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_alpha.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.1,
                                                x_zoom_min=NULL,
                                                x_zoom_max=NULL,
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_scatter_x_zoom_min.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.3,
                                                x_zoom_min=5,
                                                x_zoom_max=NULL,
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_x_zoom_max.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.3,
                                                x_zoom_min=NULL,
                                                x_zoom_max=10,
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_x_zoom_both.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.3,
                                                x_zoom_min=5,
                                                x_zoom_max=10,
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_y_zoom_min.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.3,
                                                x_zoom_min=NULL,
                                                x_zoom_max=NULL,
                                                y_zoom_min=200000,
                                                y_zoom_max=NULL,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_y_zoom_max.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.3,
                                                x_zoom_min=NULL,
                                                x_zoom_max=NULL,
                                                y_zoom_min=NULL,
                                                y_zoom_max=300000,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_y_zoom_both.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.3,
                                                x_zoom_min=NULL,
                                                x_zoom_max=NULL,
                                                y_zoom_min=200000,
                                                y_zoom_max=300000,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_all.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.1,
                                                x_zoom_min=5,
                                                x_zoom_max=10,
                                                y_zoom_min=200000,
                                                y_zoom_max=300000,
                                                base_size=15))
})

test_that('rt_explore_plot_scatterplot_size_color', {
    dataset <- read.csv("data/housing.csv", header=TRUE)
    variable <- 'median_income'
    comparison_variable <- 'median_house_value'


    test_save_plot(file_name='data/rt_explore_plot_scatter_color.png',
                   rt_explore_plot_scatter(dataset=dataset,
                                           variable=variable,
                                           comparison_variable=comparison_variable,
                                           color_variable = 'ocean_proximity',
                                           alpha=0.3,
                                           x_zoom_min=NULL,
                                           x_zoom_max=NULL,
                                           y_zoom_min=NULL,
                                           y_zoom_max=NULL,
                                           base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_size.png',
                   rt_explore_plot_scatter(dataset=dataset,
                                           variable=variable,
                                           comparison_variable=comparison_variable,
                                           size_variable = 'housing_median_age',
                                           alpha=0.3,
                                           x_zoom_min=NULL,
                                           x_zoom_max=NULL,
                                           y_zoom_min=NULL,
                                           y_zoom_max=NULL,
                                           base_size=11))


    test_save_plot(file_name='data/rt_explore_plot_scatter_size_color_numeric.png',
                   rt_explore_plot_scatter(dataset=dataset,
                                           variable=variable,
                                           comparison_variable=comparison_variable,
                                           color_variable = 'total_rooms',
                                           size_variable = 'housing_median_age',
                                           alpha=0.3,
                                           x_zoom_min=NULL,
                                           x_zoom_max=NULL,
                                           y_zoom_min=NULL,
                                           y_zoom_max=NULL,
                                           base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_scatter_size_color_categoric.png',
                   rt_explore_plot_scatter(dataset=dataset,
                                           variable=variable,
                                           comparison_variable=comparison_variable,
                                           color_variable = 'ocean_proximity',
                                           size_variable = 'housing_median_age',
                                           alpha=0.3,
                                           x_zoom_min=NULL,
                                           x_zoom_max=NULL,
                                           y_zoom_min=NULL,
                                           y_zoom_max=NULL,
                                           base_size=11))
})

test_that("rt_explore_plot_scatterplot_jitter", {
    dataset <- iris
    variable <- 'Sepal.Length'
    comparison_variable <- 'Sepal.Length'

    test_save_plot(file_name='data/rt_explore_plot_scatter_jitter.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.1,
                                                jitter=TRUE,
                                                base_size=15))
})
