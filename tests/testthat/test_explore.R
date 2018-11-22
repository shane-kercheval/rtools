context('Exploratory Analysis')
library(testthat)
library(ggplot2)

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
    correlation_plot_file <- 'data/rt_explore_plot_correlations_credit.png'
    if (file.exists(correlation_plot_file)) file.remove(correlation_plot_file)
    ggsave(filename=correlation_plot_file, plot=rt_explore_plot_correlations(dataset=credit_data))
    expect_true(file.exists(correlation_plot_file))

    # change base_size
    correlation_plot_file <- 'data/rt_explore_plot_correlations_base_size.png'
    if (file.exists(correlation_plot_file)) file.remove(correlation_plot_file)
    ggsave(filename=correlation_plot_file, plot=rt_explore_plot_correlations(dataset=credit_data,
                                                                             base_size=16))
    expect_true(file.exists(correlation_plot_file))

    # lower p_value_threshold
    correlation_plot_file <- 'data/rt_explore_plot_correlations_credit_pvalue.png'
    if (file.exists(correlation_plot_file)) file.remove(correlation_plot_file)
    ggsave(filename=correlation_plot_file,
           plot=rt_explore_plot_correlations(dataset=credit_data,
                                             #corr_threshold=0,
                                             p_value_threshold=0.3))
    expect_true(file.exists(correlation_plot_file))

    # lower p_value_threshold
    correlation_plot_file <- 'data/rt_explore_plot_correlations_credit_corr_treshold.png'
    if (file.exists(correlation_plot_file)) file.remove(correlation_plot_file)
    ggsave(filename=correlation_plot_file,
           plot=rt_explore_plot_correlations(dataset=credit_data,
                                             corr_threshold=0.115
                                             #p_value_threshold=0.3
                                             ))
    expect_true(file.exists(correlation_plot_file))

    # lower p_value_threshold
    correlation_plot_file <- 'data/rt_explore_plot_correlations_credit_both_parameters.png'
    if (file.exists(correlation_plot_file)) file.remove(correlation_plot_file)
    ggsave(filename=correlation_plot_file,
           plot=rt_explore_plot_correlations(dataset=credit_data,
                                             corr_threshold=0.115,
                                             p_value_threshold=0.3))
    expect_true(file.exists(correlation_plot_file))
})

test_that("rt_explore_unique_values", {
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

    unique_values <- rt_explore_unique_values(dataset=credit_data, variable=variable)

    expect_true(all(levels(unique_values$checking_balance) == custom_levels))

    expect_true(all(unique_values$checking_balance[1:4] == c('unknown', '< 0 DM', '1 - 200 DM', '> 200 DM')))
    expect_true(is.na(unique_values$checking_balance[5]))
    expect_true(all(unique_values$count == c(394, 273, 269, 63, 1)))
    expect_true(all(unique_values$percent == c(0.394, 0.273, 0.269, 0.063, 0.001)))

    # plot without order
    plot_file <- 'data/rt_explore_plot_unique_values_no_order.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              order_by_count=FALSE,
                                              base_size=11))
    expect_true(file.exists(plot_file))

    # plot without order
    plot_file <- 'data/rt_explore_plot_unique_values_no_group_totals.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              show_group_totals=FALSE,
                                              base_size=11))
    expect_true(file.exists(plot_file))

    # plot with order
    plot_file <- 'data/rt_explore_plot_unique_values_with_order.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              order_by_count=TRUE,
                                              base_size=11))
    expect_true(file.exists(plot_file))

    ##########################################################################################################
    # test without factor
    ##########################################################################################################
    credit_data$checking_balance <- as.character(credit_data$checking_balance)
    unique_values <- rt_explore_unique_values(dataset=credit_data, variable=variable)

    # this is the only thing that should change
    expect_true(is.null(levels(unique_values$checking_balance)))

    # all of these should remain the same
    expect_true(all(unique_values$checking_balance[1:4] == c('unknown', '< 0 DM', '1 - 200 DM', '> 200 DM')))
    expect_true(is.na(unique_values$checking_balance[5]))
    expect_true(all(unique_values$count == c(394, 273, 269, 63, 1)))
    expect_true(all(unique_values$percent == c(0.394, 0.273, 0.269, 0.063, 0.001)))

    # plot without order
    plot_file <- 'data/rt_explore_plot_unique_values_no_factor_no_order.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              order_by_count=FALSE,
                                              base_size=11))
    expect_true(file.exists(plot_file))

    # plot with order
    plot_file <- 'data/rt_explore_plot_unique_values_no_factor_with_order.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              order_by_count=TRUE,
                                              base_size=11))
    expect_true(file.exists(plot_file))
})

test_that("rt_explore_plot_unique_values_against_categorical", {
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
    plot_file <- 'data/rt_explore_plot_unique_values_comparison_variable_defaults.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable='default',
                                              order_by_count=TRUE,
                                              show_group_totals=TRUE,
                                              show_comparison_totals=TRUE))
    expect_true(file.exists(plot_file))


    # plot with labels
    plot_file <- 'data/rt_explore_plot_unique_values_comparison_variable_not_order_by_count.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable='default',
                                              order_by_count=FALSE,
                                              show_group_totals=TRUE,
                                              show_comparison_totals=TRUE))
    expect_true(file.exists(plot_file))

    # plot with labels
    plot_file <- 'data/rt_explore_plot_unique_values_comparison_variable_not_show_group_totals.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable='default',
                                              order_by_count=FALSE,
                                              show_group_totals=FALSE,
                                              show_comparison_totals=TRUE))
    expect_true(file.exists(plot_file))

    # plot with labels
    plot_file <- 'data/rt_explore_plot_unique_values_comparison_variable_not_show_comparison_totals.png'
    if (file.exists(plot_file)) file.remove(plot_file)
    ggsave(filename=plot_file,
           plot=rt_explore_plot_unique_values(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable='default',
                                              order_by_count=FALSE,
                                              show_group_totals=FALSE,
                                              show_comparison_totals=FALSE))
    expect_true(file.exists(plot_file))
})
