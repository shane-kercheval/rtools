context('Exploratory Analysis')
library(testthat)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gapminder)
library(nycflights13)
library(forcats)
# library(scales)

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

test_that("rt_explore_correlations_credit_min_missing_nas_in_column", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    # only include columns that have <= x% missing values
    max_missing_perc <- 0.05

    set.seed(42)
    rows_to_make_na_valid <- sample(nrow(credit_data), nrow(credit_data) * (max_missing_perc - 0.01))
    set.seed(43)
    rows_to_make_na_invalid <- sample(nrow(credit_data), nrow(credit_data) * (max_missing_perc + 0.01))

    # plots should include months_loan_duration and exclude age
    credit_data[rows_to_make_na_valid, 'months_loan_duration'] <- NA
    credit_data[rows_to_make_na_invalid, 'age'] <- NA

    #correlations <- rt_explore_correlations(dataset=credit_data, max_missing_column_perc=max_missing_perc)

    # use correlation parameters from above
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit_2.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data,
                                                     max_missing_column_perc=max_missing_perc))

    # lower p_value_threshold
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit_pvalue_2.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data,
                                                     max_missing_column_perc=max_missing_perc,
                                                     p_value_threshold=0.3))

    # lower p_value_threshold
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit_corr_treshold_2.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data,
                                                     max_missing_column_perc=max_missing_perc,
                                                     corr_threshold=0.115))

    # lower p_value_threshold
    test_save_plot(file_name='data/rt_explore_plot_correlations_credit_both_parameters_2.png',
                   plot=rt_explore_plot_correlations(dataset=credit_data,
                                                     max_missing_column_perc=max_missing_perc,
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

    unique_values <- suppressWarnings(rt_explore_value_totals(dataset=credit_data, variable=variable))
    expected_values <- suppressWarnings(credit_data %>%
                                            count(checking_balance, sort = TRUE) %>%
                                            rename(count = n))
    expected_values <- expected_values %>% mutate(percent = count / sum(expected_values$count))
    expect_true(rt_are_dataframes_equal(expected_values, unique_values))

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

test_that("rt_get_colors_from_values", {

    dataset <- diamonds
    custom_colors <- rt_colors()[1:5]

    ###################
    # TEST AS CHARACTER
    ####################
    returned_colors <- rt_get_colors_from_values(as.character(dataset[['cut']]))
    expect_identical(custom_colors, returned_colors)

    ###################
    # TEST AS FACTOR
    ####################
    returned_colors <- rt_get_colors_from_values(dataset[['cut']])
    expect_identical(custom_colors[c(1, 2, 5, 4, 3)], returned_colors)

    ##################################################
    # TEST WITH NA
    ##################################################
    dataset[1, 'cut'] <- NA
    ###################
    # TEST AS CHARACTER
    ####################
    returned_colors <- rt_get_colors_from_values(as.character(dataset[['cut']]))
    expect_identical(custom_colors, returned_colors)

    ###################
    # TEST AS FACTOR
    ####################
    returned_colors <- rt_get_colors_from_values(dataset[['cut']])
    expect_identical(custom_colors[c(1, 2, 5, 4, 3)], returned_colors)

    temp_dataset <- dataset
    # Ideal < Premium < Very Good < Good < Fair
    # "Fair"      "Good"      "Ideal"     "Premium"   "Very Good"
    expected_order <- c(3, 4, 5, 2, 1)
    temp_dataset$cut <- fct_infreq(temp_dataset$cut, ordered = TRUE)
    returned_colors <- rt_get_colors_from_values(temp_dataset[['cut']])
    expect_identical(custom_colors[expected_order], returned_colors)
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

    expected_total_sum <- sum(credit_data$amount, na.rm = TRUE)
    expected_unknown <- credit_data %>% filter(checking_balance == 'unknown') %>% rt_get_vector('amount') %>% sum(na.rm=TRUE)
    expected_1_200 <- credit_data %>% filter(checking_balance == '1 - 200 DM') %>% rt_get_vector('amount') %>% sum(na.rm=TRUE)
    expected_0 <- credit_data %>% filter(checking_balance == '< 0 DM') %>% rt_get_vector('amount') %>% sum(na.rm=TRUE)
    expected_200 <- credit_data %>% filter(checking_balance == '> 200 DM') %>% rt_get_vector('amount') %>% sum(na.rm=TRUE)
    expected_na <- credit_data %>% filter(is.na(checking_balance)) %>% rt_get_vector('amount') %>% sum(na.rm=TRUE)
    expected_sums <- c(expected_unknown, expected_1_200, expected_0, expected_200, expected_na)

    expect_equal(sum(value_sums$sum), expected_total_sum)
    expect_true(all(value_sums$sum == expected_sums))
    expect_true(all(value_sums$percent == expected_sums / sum(expected_sums)))
    expect_equal(sum(value_sums$percent), 1)

    # change to character
    credit_data$checking_balance <- as.character(credit_data$checking_balance)
    value_sums <- rt_explore_value_totals(dataset=credit_data, variable=variable, sum_by_variable=sum_by)

    expect_true(all(colnames(value_sums) == c('checking_balance', 'sum', 'percent')))
    expect_true(all(levels(value_sums$checking_balance) == custom_levels))

    expect_equal(sum(value_sums$sum), expected_total_sum)
    expect_true(all(value_sums$checking_balance[1:4] == c('unknown', '1 - 200 DM', '< 0 DM', '> 200 DM')))
    expect_true(is.na(value_sums$checking_balance[5]))
    expect_true(all(value_sums$sum == expected_sums))
    expect_true(all(value_sums$percent == expected_sums / sum(expected_sums)))
    expect_equal(sum(value_sums$percent), 1)
})

test_that("rt_explore_plot_value_counts", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    variable <- 'checking_balance'

    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA

    # plot without order
    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data %>%
                                                         mutate(checking_balance = as.character(checking_balance)),
                                                      variable=variable,
                                                      order_by_count=FALSE,
                                                      base_size=11))

    # plot without order
    temp_dataset <- credit_data
    temp_dataset <- temp_dataset %>%
        mutate(checking_balance = factor(as.character(checking_balance),
                                         levels=c("< 0 DM", "1 - 200 DM", "> 200 DM", "unknown"),
                                         ordered = TRUE))
    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_order__factor.png',
                   plot=rt_explore_plot_value_totals(dataset=temp_dataset,
                                                     variable=variable,
                                                     order_by_count=FALSE,
                                                     base_size=11))

    # plot without order
    temp_dataset$checking_balance <- fct_infreq(temp_dataset$checking_balance, ordered = TRUE)
    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_order__ordered.png',
                   plot=rt_explore_plot_value_totals(dataset=temp_dataset,
                                                     variable=variable,
                                                     order_by_count=FALSE,
                                                     base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_order__ordered2.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     order_by_count=TRUE,
                                                     base_size=11))

    t <- credit_data %>% mutate(checking_balance = ifelse(checking_balance == 'unknown', NA, as.character(checking_balance)))
    # plot without order
    test_save_plot(file_name='data/rt_explore_plot_value_counts_nas.png',
                   plot=rt_explore_plot_value_totals(dataset=t,
                                                     variable=variable,
                                                     order_by_count=FALSE,
                                                     base_size=11))

    # plot without order
    test_save_plot(file_name='data/rt_explore_plot_value_counts_no_group_totals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      show_variable_totals=FALSE,
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

test_that("rt_explore_plot_value_counts: logical", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    credit_data[1, 'default'] <- NA
    credit_data_logical <- credit_data %>%
        mutate(default = ifelse(default == 'yes', TRUE, FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_totals__logical.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data_logical,
                                                     variable='default',
                                                     base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_value_totals__logical_comparison.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data_logical,
                                                     variable='checking_balance',
                                                     comparison_variable='default',
                                                     base_size=11))
})

test_that("rt_explore_plot_boxplot: logical", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    credit_data[1, 'default'] <- NA
    credit_data_logical <- credit_data %>%
        mutate(default = ifelse(default == 'yes', TRUE, FALSE))

    test_save_plot(file_name='data/rt_explore_plot_boxplot__logical.png',
                   plot=rt_explore_plot_boxplot(dataset=credit_data_logical,
                                                variable='amount',
                                                comparison_variable='default'))

    test_save_plot(file_name='data/rt_explore_plot_boxplot__logical_color.png',
                   plot=rt_explore_plot_boxplot(dataset=credit_data_logical,
                                                variable='amount',
                                                comparison_variable='checking_balance',
                                                color_variable = 'default'))
})

test_that("rt_explore_plot_scatter: logical", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    credit_data_logical <- credit_data %>%
        mutate(default = ifelse(default == 'yes', TRUE, FALSE))

    test_save_plot(file_name='data/rt_explore_plot_scatter__logical_size.png',
                   plot=rt_explore_plot_scatter(dataset=credit_data_logical,
                                                variable='amount',
                                                comparison_variable='months_loan_duration',
                                                size_variable = 'default'))

    credit_data_logical[1, 'default'] <- NA
    test_save_plot(file_name='data/rt_explore_plot_scatter__logical.png',
                   plot=rt_explore_plot_scatter(dataset=credit_data_logical,
                                                variable='amount',
                                                comparison_variable='months_loan_duration'))

    test_save_plot(file_name='data/rt_explore_plot_scatter__logical_color.png',
                   plot=rt_explore_plot_scatter(dataset=credit_data_logical,
                                                variable='amount',
                                                comparison_variable='months_loan_duration',
                                                color_variable = 'default'))
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
                                                      show_variable_totals=TRUE,
                                                      show_comparison_totals=TRUE))

    # plot pretty
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comparison_variable_pretty.png',
                   plot=rt_explore_plot_value_totals(dataset=rt_pretty_dataset(credit_data),
                                                      variable=rt_pretty_text(variable),
                                                      comparison_variable=rt_pretty_text('default'),
                                                      order_by_count=TRUE,
                                                      show_variable_totals=TRUE,
                                                      show_comparison_totals=TRUE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts_comparison_variable_swapped.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable='default',
                                                     comparison_variable=variable,
                                                     order_by_count=TRUE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE))


    # plot with labels
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comparison_variable_not_order_by_count.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      comparison_variable='default',
                                                      order_by_count=FALSE,
                                                      show_variable_totals=TRUE,
                                                      show_comparison_totals=TRUE))

    # plot with labels
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comp_var_not_show_group_totals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      comparison_variable='default',
                                                      order_by_count=FALSE,
                                                      show_variable_totals=FALSE,
                                                      show_comparison_totals=TRUE))

    # plot with labels
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comp_var_not_show_comparison_totals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      comparison_variable='default',
                                                      order_by_count=FALSE,
                                                      show_variable_totals=FALSE,
                                                      show_comparison_totals=FALSE))

    ##########################################################################################################
    # ORDERED FACTORS
    ##########################################################################################################
    credit_data$checking_balance <- factor(credit_data$checking_balance,
                                           levels=c("< 0 DM", "1 - 200 DM", "> 200 DM", "unknown"),
                                           ordered=TRUE)
    credit_data$default <- factor(credit_data$default,
                                           levels=c("no", "yes"),
                                           ordered=TRUE)
    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     order_by_count=FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_stacked.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack Percent",
                                                     order_by_count=FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_stacked_rev.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack Percent",
                                                     order_by_count=FALSE,
                                                     reverse_stack=FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_stacked_total.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack",
                                                     order_by_count=FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_stacked_total_rev.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack",
                                                     order_by_count=FALSE,
                                                     reverse_stack=FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_stacked_amount.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack",
                                                     sum_by_variable = 'amount',
                                                     order_by_count=TRUE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_stacked_amount_rev.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack",
                                                     sum_by_variable = 'amount',
                                                     order_by_count=TRUE,
                                                     reverse_stack=FALSE))

    test_save_plot(file_name='data/value_counts__ordered_factor_stacked_amount_no_var_totals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack",
                                                     sum_by_variable = 'amount',
                                                     order_by_count=TRUE,
                                                     show_variable_totals=FALSE))

    test_save_plot(file_name='data/value_counts__ordered_factor_stacked_amount_no_comp_totals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack",
                                                     sum_by_variable = 'amount',
                                                     order_by_count=TRUE,
                                                     show_comparison_totals=FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_stacked_amount2.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack",
                                                     sum_by_variable = 'amount',
                                                     order_by_count=TRUE,
                                                     show_dual_axes=TRUE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_conf.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Confidence Interval",
                                                     order_by_count=FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_conf2.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Confidence Interval - within Variable",
                                                     order_by_count=FALSE))

    # change the order of the secondary/comparison variable
    credit_data$default <- factor(credit_data$default,
                                  levels=c("yes", "no"),
                                  ordered=TRUE)
    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor__swapped_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     order_by_count=FALSE))
    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_stacked__swapped_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Stack Percent",
                                                     order_by_count=FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_conf__swapped_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Confidence Interval",
                                                     order_by_count=FALSE))


    test_save_plot(file_name='data/rt_explore_plot_value_counts__ordered_factor_conf2__swapped_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='default',
                                                     view_type="Confidence Interval - within Variable",
                                                     order_by_count=FALSE))
})

test_that("rt_explore_plot_value_totals__daul_axes", {

    test_save_plot(file_name='data/rt_explore_plot_value_totals__diamonds__cut__daul.png',
                   plot=rt_explore_plot_value_totals(dataset=diamonds,
                                                     variable='cut',
                                                     show_dual_axes = TRUE))

    test_save_plot(file_name='data/rt_explore_plot_value_totals__diamonds__comparison__dual.png',
                   plot=rt_explore_plot_value_totals(dataset=diamonds,
                                                     variable='cut',
                                                     comparison_variable='color',
                                                     show_dual_axes = TRUE))

    test_save_plot(file_name='data/rt_explore_plot_value_totals__diamonds__comparison__no_dual.png',
                   plot=rt_explore_plot_value_totals(dataset=diamonds,
                                                     variable='cut',
                                                     comparison_variable='color',
                                                     show_dual_axes = FALSE))

    test_save_plot(file_name='data/rt_explore_plot_value_totals__diamonds__sum_by_total__daul.png',
                   plot=rt_explore_plot_value_totals(dataset=diamonds,
                                                     variable='cut',
                                                     sum_by_variable='price',
                                                     show_dual_axes = TRUE))
    test_save_plot(file_name='data/rt_explore_plot_value_totals__diamonds__sum_comparison__daul.png',
                   plot=rt_explore_plot_value_totals(dataset=diamonds,
                                                     variable='cut',
                                                     comparison_variable='color',
                                                     sum_by_variable='price',
                                                     show_variable_totals=FALSE,
                                                     show_comparison_totals=FALSE,
                                                     show_dual_axes = TRUE))

    test_save_plot(file_name='data/rt_explore_plot_value_totals__diamonds__sum_comparison__no_dual.png',
                   plot=rt_explore_plot_value_totals(dataset=diamonds,
                                                     variable='cut',
                                                     comparison_variable='color',
                                                     sum_by_variable='price',
                                                     show_variable_totals=FALSE,
                                                     show_comparison_totals=FALSE,
                                                     show_dual_axes = FALSE))

    # STACK - should NOT show dual axes regardless if stacked (all percentages)
    test_save_plot(file_name='data/rt_explore_plot_value_totals__diamonds__comparison_stacked__dual.png',
                   plot=rt_explore_plot_value_totals(dataset=diamonds,
                                                     variable='cut',
                                                     comparison_variable='color',
                                                     view_type="Stack Percent",
                                                     show_dual_axes = TRUE))

    test_save_plot(file_name='data/rt_explore_plot_value_totals__diamonds__comparison_sum_stacked__dual.png',
                   plot=rt_explore_plot_value_totals(dataset=diamonds,
                                                     variable='cut',
                                                     comparison_variable='color',
                                                     sum_by_variable='price',
                                                     view_type="Stack Percent",
                                                     show_dual_axes = TRUE))
})

test_that("rt_explore_plot_value_totals__conf_intervals", {
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
    comparison_variable <- 'housing'
    sum_by_variable <- 'amount'

    ##########################################################################################################
    # VARIABLE ONLY
    # Test c("Bar", "Confidence Interval")
    ##########################################################################################################
    test_save_plot(file_name='data/plot_value_totals__var__bar__dual.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=NULL,
                                                     sum_by_variable=NULL,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Bar",
                                                     show_dual_axes=TRUE))

    test_save_plot(file_name='data/plot_value_totals__var__CI.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=NULL,
                                                     sum_by_variable=NULL,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Confidence Interval",
                                                     show_dual_axes=FALSE))

    test_save_plot(file_name='data/plot_value_totals__var__CI__no_vals.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=NULL,
                                                     sum_by_variable=NULL,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=FALSE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Confidence Interval",
                                                     show_dual_axes=FALSE))

    multi_value_credit_data <- credit_data %>%
        mutate(purpose = case_when(
            purpose == 'car' ~ 'car, car_test',
            purpose == 'business' ~ 'business, business_test',
            TRUE ~ as.character(purpose))) %>%
        mutate(purpose = as.factor(purpose))

    test_save_plot(file_name='data/plot_value_totals__conf__multi_value.png',
                   plot=rt_explore_plot_value_totals(dataset=multi_value_credit_data,
                                                     variable='purpose',
                                                     comparison_variable = NULL,
                                                     view_type="Confidence Interval",
                                                     multi_value_delimiter=', '))

    expect_error(rt_explore_plot_value_totals(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable=NULL,
                                              sum_by_variable=NULL,
                                              order_by_count=FALSE,
                                              show_variable_totals=TRUE,
                                              show_comparison_totals=TRUE,
                                              view_type="Facet by Comparison",
                                              show_dual_axes=TRUE))

    expect_error(rt_explore_plot_value_totals(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable=NULL,
                                              sum_by_variable=NULL,
                                              order_by_count=FALSE,
                                              show_variable_totals=TRUE,
                                              show_comparison_totals=TRUE,
                                              view_type="Confidence Interval - within Variable",
                                              show_dual_axes=TRUE))

    expect_error(rt_explore_plot_value_totals(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable=NULL,
                                              sum_by_variable=NULL,
                                              order_by_count=FALSE,
                                              show_variable_totals=TRUE,
                                              show_comparison_totals=TRUE,
                                              view_type="Stack Percent",
                                              show_dual_axes=TRUE))
    ##########################################################################################################
    # VARIABLE, SUM_BY_VARIABLE
    # Test c("Bar")
    ##########################################################################################################
    test_save_plot(file_name='data/plot_value_totals__var__sum_by__bar.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=NULL,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Bar",
                                                     show_dual_axes=TRUE))

    expect_error(rt_explore_plot_value_totals(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable=NULL,
                                              sum_by_variable=sum_by_variable,
                                              order_by_count=FALSE,
                                              show_variable_totals=TRUE,
                                              show_comparison_totals=TRUE,
                                              view_type="Confidence Interval",
                                              show_dual_axes=TRUE))

    expect_error(rt_explore_plot_value_totals(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable=NULL,
                                              sum_by_variable=sum_by_variable,
                                              order_by_count=FALSE,
                                              show_variable_totals=TRUE,
                                              show_comparison_totals=TRUE,
                                              view_type="Facet by Comparison",
                                              show_dual_axes=TRUE))

    expect_error(rt_explore_plot_value_totals(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable=NULL,
                                              sum_by_variable=sum_by_variable,
                                              order_by_count=FALSE,
                                              show_variable_totals=TRUE,
                                              show_comparison_totals=TRUE,
                                              view_type="Stack Percent",
                                              show_dual_axes=TRUE))

    ##########################################################################################################
    # VARIABLE, COMPARISON_VARIABLE
    # Test c("Bar", "Confidence Interval", "Facet by Comparison", "Confidence Interval - within Variable", "Stack")
    ##########################################################################################################
    test_save_plot(file_name='data/plot_value_totals__var__comp__bar.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=NULL,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Bar",
                                                     show_dual_axes=TRUE))

    test_save_plot(file_name='data/plot_value_totals__var__comp__CI.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=NULL,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Confidence Interval",
                                                     show_dual_axes=TRUE))

    test_save_plot(file_name='data/plot_value_totals__var__comp__facet.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=NULL,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Facet by Comparison",
                                                     show_dual_axes=TRUE))

    test_save_plot(file_name='data/plot_value_totals__var__comp__CI_var.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=NULL,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Confidence Interval - within Variable",
                                                     show_dual_axes=TRUE))

    test_save_plot(file_name='data/plot_value_totals__var__comp__stack.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=NULL,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Stack Percent",
                                                     show_dual_axes=TRUE))

    ##########################################################################################################
    # VARIABLE, COMPARISON_VARIABLE, SUM_BY_VARIABLE
    # Test c("Bar", "Facet by Comparison", "Stack")
    ##########################################################################################################
    test_save_plot(file_name='data/plot_value_totals__var__comp__sum__bar.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Bar",
                                                     show_dual_axes=TRUE))

    expect_error(rt_explore_plot_value_totals(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable=comparison_variable,
                                              sum_by_variable=sum_by_variable,
                                              order_by_count=FALSE,
                                              show_variable_totals=TRUE,
                                              show_comparison_totals=TRUE,
                                              view_type="Confidence Interval",
                                              show_dual_axes=TRUE))

    test_save_plot(file_name='data/plot_value_totals__var__comp__sum__facet.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Facet by Comparison",
                                                     show_dual_axes=TRUE))

    expect_error(rt_explore_plot_value_totals(dataset=credit_data,
                                              variable=variable,
                                              comparison_variable=comparison_variable,
                                              sum_by_variable=sum_by_variable,
                                              order_by_count=FALSE,
                                              show_variable_totals=TRUE,
                                              show_comparison_totals=TRUE,
                                              view_type="Confidence Interval - within Variable",
                                              show_dual_axes=TRUE))

    test_save_plot(file_name='data/plot_value_totals__var__comp__sum__stack.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Stack Percent",
                                                     show_dual_axes=TRUE))
})

test_that("rt_explore_plot_value_counts_against_categorical_fill", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)

    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA
    variable <- 'checking_balance'

    # plot with labels
    test_save_plot(file_name='data/rt_explore_plot_value_counts_comparison_variable_purpose_stack.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                      variable=variable,
                                                      comparison_variable='purpose',
                                                      order_by_count=TRUE,
                                                      show_variable_totals=TRUE,
                                                      show_comparison_totals=TRUE,
                                                      view_type="Stack Percent"))

    test_save_plot(file_name='data/rt_explore_plot_value_counts_comparison_variable_purpose_stack_sum.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable='purpose',
                                                     sum_by_variable='amount',
                                                     order_by_count=TRUE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     view_type="Stack Percent"))
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
                                                     show_variable_totals=FALSE,
                                                     show_comparison_totals=FALSE,
                                                     base_size=16))

    test_save_plot(file_name='data/rt_explore_plot_value_totals_sums_comparison_defaults.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=TRUE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=TRUE,
                                                     base_size=14))

    test_save_plot(file_name='data/rt_explore_plot_value_totals_sums_comparison_defaults_no_order.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable=comparison_variable,
                                                     sum_by_variable=sum_by_variable,
                                                     order_by_count=FALSE,
                                                     show_variable_totals=TRUE,
                                                     show_comparison_totals=FALSE,
                                                     base_size=14))
})

test_that("rt_explore_plot_value_totals_multivalue_column", {

    credit_data <- read.csv("data/credit.csv", header=TRUE)

    expected_totals <- rt_explore_value_totals(dataset=credit_data,
                                               variable='purpose',
                                               multi_value_delimiter=NULL)
    expect_equal(sum(expected_totals$percent), 1)

    # first test with a delimiter when none of the columns are delimited
    found_totals <- rt_explore_value_totals(dataset=credit_data,
                                            variable='purpose',
                                            multi_value_delimiter=', ')
    expect_true(rt_are_dataframes_equal(expected_totals, found_totals))

    expected_sum_by_variable <- credit_data %>%
        count(purpose, wt = months_loan_duration, sort = TRUE)
    expected_sum_by_variable <- expected_sum_by_variable %>%
        rename(sum = n) %>%
        mutate(percent = sum / sum(expected_sum_by_variable$n))
    expect_equal(sum(expected_sum_by_variable$percent), 1)

    expect_true(rt_are_dataframes_equal(expected_sum_by_variable,
                                        rt_explore_value_totals(dataset=credit_data,
                                                                variable='purpose',
                                                                sum_by_variable='months_loan_duration',
                                                                multi_value_delimiter=', ')))

    # now test with multi-value columns
    #expected_totals =
    expected_totals <- rbind(expected_totals,
                             expected_totals %>%
                                 filter(purpose == 'car' | purpose == 'business') %>%
                                 mutate(purpose = paste0(purpose, '_test'))) %>%
        arrange(desc(count), purpose)
    expect_true(sum(expected_totals$percent) > 1)


    expected_sum_by_variable <- rbind(expected_sum_by_variable,
                                      expected_sum_by_variable %>%
                                          filter(purpose == 'car' | purpose == 'business') %>%
                                          mutate(purpose = paste0(purpose, '_test'))) %>%
        arrange(desc(sum), purpose)
    expect_true(sum(expected_sum_by_variable$percent) > 1)

    expected_totals <- expected_totals %>%
        mutate(purpose = factor(purpose, levels=sort(as.character(expected_totals$purpose))))
    # create multi-value column

    credit_data <- credit_data %>%
        mutate(purpose = case_when(
            purpose == 'car' ~ 'car, car_test',
            purpose == 'business' ~ 'business, business_test',
            TRUE ~ as.character(purpose))) %>%
        mutate(purpose = as.factor(purpose))

    found_totals <- rt_explore_value_totals(dataset=credit_data,
                                            variable='purpose',
                                            multi_value_delimiter=', ')

    expect_true(rt_are_dataframes_equal(expected_totals, found_totals))

    found_sums <- rt_explore_value_totals(dataset=credit_data,
                                          variable='purpose',
                                          sum_by_variable='months_loan_duration',
                                          multi_value_delimiter=', ')

    expect_true(rt_are_dataframes_equal(expected_sum_by_variable, found_sums))

    variable <- 'purpose'
    comparison_variable <- NULL
    test_save_plot(file_name='data/rt_explore_plot_value_totals_purose_multivalue.png',
                   plot=rt_explore_plot_value_totals(dataset=credit_data,
                                                     variable=variable,
                                                     comparison_variable = NULL,
                                                     multi_value_delimiter=', '))
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

    test_save_plot(file_name='data/rt_explore_plot_boxplot_comparison2.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                variable=variable,
                                                comparison_variable='checking_balance',
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_boxplot_color__defualt.png',
                   plot=rt_explore_plot_boxplot(dataset=dataset,
                                                variable=variable,
                                                comparison_variable='checking_balance',
                                                color_variable='default',
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))
    temp_dataset <- dataset
    temp_dataset[1, 'default'] <- NA
    test_save_plot(file_name='data/rt_explore_plot_boxplot_color__NAs.png',
                   plot=rt_explore_plot_boxplot(dataset=temp_dataset,
                                                variable=variable,
                                                comparison_variable='checking_balance',
                                                color_variable='default',
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))
    temp_dataset[1, 'checking_balance'] <- NA
    test_save_plot(file_name='data/rt_explore_plot_boxplot_color__NAs2.png',
                   plot=rt_explore_plot_boxplot(dataset=temp_dataset,
                                                variable=variable,
                                                comparison_variable='checking_balance',
                                                color_variable='default',
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))
    temp_dataset[2, 'checking_balance'] <- NA
    test_save_plot(file_name='data/rt_explore_plot_boxplot_color__NAs3.png',
                   plot=rt_explore_plot_boxplot(dataset=temp_dataset,
                                                variable=variable,
                                                comparison_variable='checking_balance',
                                                color_variable='default',
                                                y_zoom_min=NULL,
                                                y_zoom_max=NULL,
                                                base_size=11))
    temp_dataset[3, 'default'] <- NA
    test_save_plot(file_name='data/rt_explore_plot_boxplot_color__NAs4.png',
                   plot=rt_explore_plot_boxplot(dataset=temp_dataset,
                                                variable=variable,
                                                comparison_variable='checking_balance',
                                                color_variable='default',
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
    test_save_plot(file_name='data/rt_explore_plot_histogram_with_comp_density.png',
                   plot=rt_explore_plot_histogram(dataset=dataset,
                                                  variable=variable,
                                                  comparison_variable=comparison_variable,
                                                  density = TRUE,
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
    variable <- 'median_house_value'
    comparison_variable <- 'median_income'

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

        test_save_plot(file_name='data/rt_explore_plot_scatter_swap.png',
                    plot=rt_explore_plot_scatter(dataset=dataset,
                                                 variable=comparison_variable,
                                                 comparison_variable=variable,
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

    test_save_plot(file_name='data/rt_explore_plot_scatter_zoom_min_both.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                alpha=0.3,
                                                x_zoom_min=10,
                                                x_zoom_max=NULL,
                                                y_zoom_min=200000,
                                                y_zoom_max=NULL,
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
    variable <- 'median_house_value'
    comparison_variable <- 'median_income'


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

    test_save_plot(file_name='data/rt_explore_plot_scatter_size__categoric.png',
                   rt_explore_plot_scatter(dataset=dataset,
                                           variable=variable,
                                           comparison_variable=comparison_variable,
                                           size_variable = 'ocean_proximity',
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

test_that("rt_explore_plot_aggregate_2_numerics", {
    dataset <- read.csv("data/credit.csv", header=TRUE)
    # make sure it handles NAs
    dataset[1, 'months_loan_duration'] <- NA
    variable <- 'amount'
    comparison_variable <- 'months_loan_duration'

    aggregation_function <- rt_geometric_mean
    aggregation_function_name <- "Geometric Mean"

    test_save_plot(file_name='data/rt_explore_plot_aggregate_2_numerics__boxplot_0_min.png',
                   plot=suppressWarnings(rt_explore_plot_aggregate_2_numerics(dataset=dataset,
                                                             variable=variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=NULL,
                                                             aggregation_function_name=NULL,
                                                             aggregation_count_minimum=0, # need at least 30 samples, otherwise when we bootstrap resample e.g. with a group that has 1 sample we'd pull e.g. 100 random samples of the same value
                                                             show_resampled_confidence_interval=TRUE,
                                                             show_points=TRUE,
                                                             show_labels=TRUE,
                                                             x_zoom_min=NULL,
                                                             x_zoom_max=NULL,
                                                             y_zoom_min=NULL,
                                                             y_zoom_max=NULL,
                                                             base_size=11)))

    test_save_plot(file_name='data/rt_explore_plot_aggregate_2_numerics__boxplot_30_min.png',
                   plot=rt_explore_plot_aggregate_2_numerics(dataset=dataset,
                                                             variable=variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=NULL,
                                                             aggregation_function_name=NULL,
                                                             aggregation_count_minimum=30, # need at least 30 samples, otherwise when we bootstrap resample e.g. with a group that has 1 sample we'd pull e.g. 100 random samples of the same value
                                                             show_resampled_confidence_interval=TRUE,
                                                             show_points=TRUE,
                                                             show_labels=TRUE,
                                                             x_zoom_min=NULL,
                                                             x_zoom_max=NULL,
                                                             y_zoom_min=NULL,
                                                             y_zoom_max=NULL,
                                                             base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_aggregate_2_numerics__geometric_mean.png',
                   plot=rt_explore_plot_aggregate_2_numerics(dataset=dataset,
                                                             variable=variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=aggregation_function,
                                                             aggregation_function_name=aggregation_function_name,
                                                             aggregation_count_minimum=30, # need at least 30 samples, otherwise when we bootstrap resample e.g. with a group that has 1 sample we'd pull e.g. 100 random samples of the same value
                                                             show_resampled_confidence_interval=TRUE,
                                                             show_points=TRUE,
                                                             show_labels=TRUE,
                                                             x_zoom_min=NULL,
                                                             x_zoom_max=NULL,
                                                             y_zoom_min=NULL,
                                                             y_zoom_max=NULL,
                                                             base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_aggregate_2_numerics__geometric_mean__2.png',
                   plot=rt_explore_plot_aggregate_2_numerics(dataset=dataset,
                                                             variable=variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=aggregation_function,
                                                             aggregation_function_name=aggregation_function_name,
                                                             aggregation_count_minimum=30, # need at least 30 samples, otherwise when we bootstrap resample e.g. with a group that has 1 sample we'd pull e.g. 100 random samples of the same value
                                                             show_resampled_confidence_interval=FALSE,
                                                             show_points=FALSE,
                                                             show_labels=TRUE,
                                                             x_zoom_min=10,
                                                             x_zoom_max=40,
                                                             y_zoom_min=1900,
                                                             y_zoom_max=5000,
                                                             base_size=11))

    variable <- 'months_loan_duration'
    comparison_variable <- 'existing_loans_count'

    aggregation_function <- function(values) {
        return (mean(values, na.rm = TRUE))
    }
    aggregation_function_name <- "Mean"

    test_save_plot(file_name='data/rt_explore_plot_aggregate_2_numerics__loan_count__boxplot_0_min.png',
                   plot=suppressWarnings(rt_explore_plot_aggregate_2_numerics(dataset=dataset,
                                                             variable=variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=NULL,
                                                             aggregation_function_name=NULL,
                                                             aggregation_count_minimum=0, # need at least 30 samples, otherwise when we bootstrap resample e.g. with a group that has 1 sample we'd pull e.g. 100 random samples of the same value
                                                             show_resampled_confidence_interval=TRUE,
                                                             show_points=TRUE,
                                                             show_labels=TRUE,
                                                             x_zoom_min=NULL,
                                                             x_zoom_max=NULL,
                                                             y_zoom_min=NULL,
                                                             y_zoom_max=NULL,
                                                             base_size=11)))

    test_save_plot(file_name='data/rt_explore_plot_aggregate_2_numerics__loan_count__boxplot_30_min.png',
                   plot=suppressWarnings(rt_explore_plot_aggregate_2_numerics(dataset=dataset,
                                                             variable=variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=NULL,
                                                             aggregation_function_name=NULL,
                                                             aggregation_count_minimum=30, # need at least 30 samples, otherwise when we bootstrap resample e.g. with a group that has 1 sample we'd pull e.g. 100 random samples of the same value
                                                             show_resampled_confidence_interval=TRUE,
                                                             show_points=TRUE,
                                                             show_labels=TRUE,
                                                             x_zoom_min=NULL,
                                                             x_zoom_max=NULL,
                                                             y_zoom_min=NULL,
                                                             y_zoom_max=NULL,
                                                             base_size=11)))

    test_save_plot(file_name='data/rt_explore_plot_aggregate_2_numerics__loan_count__mean.png',
                   plot=rt_explore_plot_aggregate_2_numerics(dataset=dataset,
                                                             variable=variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=aggregation_function,
                                                             aggregation_function_name=aggregation_function_name,
                                                             aggregation_count_minimum=30, # need at least 30 samples, otherwise when we bootstrap resample e.g. with a group that has 1 sample we'd pull e.g. 100 random samples of the same value
                                                             show_resampled_confidence_interval=TRUE,
                                                             show_points=TRUE,
                                                             show_labels=TRUE,
                                                             x_zoom_min=NULL,
                                                             x_zoom_max=NULL,
                                                             y_zoom_min=NULL,
                                                             y_zoom_max=NULL,
                                                             base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_aggregate_2_numerics__loan_count__mean__2.png',
                   plot=rt_explore_plot_aggregate_2_numerics(dataset=dataset,
                                                             variable=variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=aggregation_function,
                                                             aggregation_function_name=aggregation_function_name,
                                                             aggregation_count_minimum=30, # need at least 30 samples, otherwise when we bootstrap resample e.g. with a group that has 1 sample we'd pull e.g. 100 random samples of the same value
                                                             show_resampled_confidence_interval=FALSE,
                                                             show_points=FALSE,
                                                             show_labels=TRUE,
                                                             x_zoom_min=-1,
                                                             x_zoom_max=3,
                                                             y_zoom_min=4,
                                                             y_zoom_max=25,
                                                             base_size=11))

})

test_that("rt_explore_plot_scatterplot_labels", {
    dataset <- data.frame(gapminder) %>%
        filter(year == 2002)
    variable <- 'lifeExp'
    comparison_variable <- 'gdpPercap'

    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__defaults.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable))

    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__country_label.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                label_variables='country'))

    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__country_label__size.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                label_variables='country',
                                                size_variable='pop'))

    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__country_label__zoom.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                label_variables='country',
                                                x_zoom_min = 25000,
                                                x_zoom_max = 40000,
                                                y_zoom_min = 75))

    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__country_label__zoom_size.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                label_variables='country',
                                                size_variable = 'pop',
                                                x_zoom_min = 25000,
                                                x_zoom_max = 40000,
                                                y_zoom_min = 75))

    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__lifeExp_label.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                label_variables='lifeExp'))

    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__pop_label.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                label_variables='pop'))

    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__multi_label.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                label_variables=c('country', 'year')))
    test_save_plot(file_name='data/rt_explore_plot_scatter__gapminder__multi2_label.png',
                   plot=rt_explore_plot_scatter(dataset=dataset,
                                                variable=variable,
                                                comparison_variable=comparison_variable,
                                                label_variables=c('year', 'country'),
                                                size_variable = 'pop',
                                                x_zoom_min = 25000,
                                                x_zoom_max = 40000,
                                                y_zoom_min = 75))
})

test_that('rt_explore_plot_time_series', {
    dataset <- data.frame(flights %>%
                              mutate(date = lubridate::make_date(year, month, day),
                                     cohort = paste0(year, '-',
                                                     lubridate::week(date)))) %>%
       select(date, dep_delay, dep_time, origin, cohort)

    variable <- 'date'
    comparison_variable <- 'dep_delay'

    comp_func_sum <- function(x) {
        return (sum(x, na.rm=TRUE))
    }
    comp_func_mean <- function(x) {
        return (mean(x, na.rm=TRUE))
    }
    comp_func_median <- function(x) {
        return (median(x, na.rm=TRUE))
    }

    test_save_plot(file_name='data/rt_explore_plot_time_series_default.png',
                   plot=rt_explore_plot_time_series(dataset=dataset, variable=variable))

    test_save_plot(file_name='data/rt_explore_plot_time_series_default_include_zero_false.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable, include_zero_y_axis = FALSE))

    # rquires both comparison_function and comparison_function_name
    expect_error(rt_explore_plot_time_series(dataset=dataset, variable=variable,
                                             comparison_variable=comparison_variable))
    expect_error(rt_explore_plot_time_series(dataset=dataset, variable=variable,
                                             comparison_variable=comparison_variable,
                                             comparison_function=comp_func_sum))
    expect_error(rt_explore_plot_time_series(dataset=dataset, variable=variable,
                                             comparison_variable=comparison_variable,
                                             comparison_function_name='Sum'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_comparison_sum.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_sum,
                                                    comparison_function_name='Sum of'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_comparison_mean.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_comparison_median.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_comparison_median_zoom_min.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median',
                                                    y_zoom_min=0))

    test_save_plot(file_name='data/rt_explore_plot_time_series_comparison_median_zoom_minmax.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median',
                                                    y_zoom_min=0,
                                                    y_zoom_max=10,
                                                    base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_time_series_zoom_min.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    y_zoom_min=750,
                                                    base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_time_series_dep_time_zoom_min.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable='dep_time',
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median',
                                                    y_zoom_min=1000,
                                                    y_zoom_max=NULL,
                                                    base_size=15))

    color_variable <- 'origin'
    test_save_plot(file_name='data/rt_explore_plot_time_series_comparison_median_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median',
                                                    color_variable = color_variable,
                                                    # y_zoom_min=0,
                                                    show_labels = TRUE,
                                                    y_zoom_max=25,
                                                    base_size=15))

    test_save_plot(file_name='data/rt_explore_plot_time_series_count_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    color_variable=color_variable))

    test_save_plot(file_name='data/rt_explore_plot_time_series_count_color_points_labels.png',
                   plot=rt_explore_plot_time_series(dataset=dataset %>% filter(month(date) == 12),
                                                    variable=variable,
                                                    color_variable=color_variable,
                                                    show_points=TRUE,
                                                    show_labels=TRUE,
                                                    date_breaks = '1 day'))
})

test_that('rt_explore_plot_time_series__POSIXct', {
    dataset <- data.frame(flights %>%
                              mutate(date = lubridate::make_date(year, month, day),
                                     cohort = paste0(year, '-',
                                                     lubridate::week(date)))) %>%
        select(date, dep_delay, dep_time, origin, cohort) %>%
        mutate(date = as.POSIXct(date))

    variable <- 'date'
    comparison_variable <- 'dep_delay'

    # was POSIXct failing
    test_save_plot(file_name='data/rt_explore_plot_time_series_default__POSIXct.png',
                   plot=rt_explore_plot_time_series(dataset=dataset, variable=variable))
})

test_that('rt_explore_plot_time_series_breaks_floors', {
    dataset <- data.frame(flights %>%
                              mutate(date = lubridate::make_date(year, month, day),
                                     cohort = paste0(year, '-',
                                                     lubridate::week(date)))) %>%
        select(date, dep_delay, dep_time, origin, cohort)

    variable <- 'date'
    comparison_variable <- 'dep_delay'

    comp_func_sum <- function(x) {
        return (sum(x, na.rm=TRUE))
    }
    comp_func_mean <- function(x) {
        return (mean(x, na.rm=TRUE))
    }
    comp_func_median <- function(x) {
        return (median(x, na.rm=TRUE))
    }

    ##########################################################################################################
    # week/month/quarter/year defaults
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_time_series_week.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = NULL))

    test_save_plot(file_name='data/rt_explore_plot_time_series_month.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = NULL,
                                                    date_breaks_width = NULL))

    test_save_plot(file_name='data/rt_explore_plot_time_series_quarter.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'quarter',
                                                    date_break_format = NULL,
                                                    date_breaks_width = NULL))


    test_save_plot(file_name='data/rt_explore_plot_time_series_year.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'year',
                                                    date_break_format = NULL,
                                                    date_breaks_width = NULL))

    ##########################################################################################################
    # week/month/quarter/year widths
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_time_series_week_width.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 weeks'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_month_width.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '4 months'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_quarter_width.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'quarter',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '3 months'))


    ##########################################################################################################
    # week/month/quarter/year formats
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_time_series_week_format.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 weeks'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_month_format.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '4 months'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_quarter_format.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'quarter',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '3 months'))

    ##########################################################################################################
    # week/month/quarter/year other variables & options
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_time_series_sum_week.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_sum,
                                                    comparison_function_name='Sum of',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'week'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_sum_month.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_sum,
                                                    comparison_function_name='Sum of',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'month'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_sum_quarter.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_sum,
                                                    comparison_function_name='Sum of',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'quarter'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_sum_year.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_sum,
                                                    comparison_function_name='Sum of',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'year'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_average_week.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'week'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_average_month.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'month'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_average_quarter.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'quarter'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_average_year.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'year'))


    test_save_plot(file_name='data/rt_explore_plot_time_series_median_week.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'week'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_median_month.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'month'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_median_quarter.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'quarter'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_median_year.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_median,
                                                    comparison_function_name='Median',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'year'))

    ##########################################################################################################
    # week/month/quarter/year other variables & options
    ##########################################################################################################
    color_variable <- 'origin'
    test_save_plot(file_name='data/rt_explore_plot_time_series_week_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    color_variable=color_variable,
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'week'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_average_week_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average',
                                                    color_variable = color_variable,
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'week'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_month_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    color_variable=color_variable,
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'month'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_average_month_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average',
                                                    color_variable = color_variable,
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'month'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_quarter_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    color_variable=color_variable,
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'quarter'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_average_quarter_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average',
                                                    color_variable = color_variable,
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'quarter'))
    test_save_plot(file_name='data/rt_explore_plot_time_series_year_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    color_variable=color_variable,
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'year'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_average_year_color.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_mean,
                                                    comparison_function_name='Average',
                                                    color_variable = color_variable,
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'year'))
})

test_that('rt_explore_plot_time_series_breaks_floors_date_time', {
    dataset_10 <- data.frame(flights) %>%
        filter(floor_date(time_hour, unit = 'days') <= ymd('2013-02-10'))
    dataset_11 <- data.frame(flights) %>%
        filter(floor_date(time_hour, unit = 'days') <= ymd('2013-02-11'))
    # max(dataset_10$time_hour)
    # max(dataset_11$time_hour)
    # dataset_11 %>%
    #     mutate(cohort = floor_date(time_hour, unit='weeks',week_start = 1)) %>%
    #     count(cohort)

    variable <- 'time_hour'
    comparison_variable <- 'dep_delay'

    comp_func_sum <- function(x) {
        return (sum(x, na.rm=TRUE))
    }
    comp_func_mean <- function(x) {
        return (mean(x, na.rm=TRUE))
    }
    comp_func_median <- function(x) {
        return (median(x, na.rm=TRUE))
    }

    ##########################################################################################################
    # week/month/quarter/year defaults
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_week_210.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_10,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = NULL))

    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_week_211.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_11,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = NULL))

    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_week_210_day.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_10,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = NULL))

    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_week_211_day.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_11,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = NULL))
    ##########################################################################################################
    # week/month/quarter/year widths
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_week_width_210.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_10,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 weeks'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_week_width_211.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_11,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 weeks'))

    ##########################################################################################################
    # week/month/quarter/year formats
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_week_format_210.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_10,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 weeks'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_week_format_211.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_11,
                                                    variable=variable,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 weeks'))

    ##########################################################################################################
    # week/month/quarter/year other variables & options
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_sum_week_210.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_10,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_sum,
                                                    comparison_function_name='Sum of',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'week'))

    test_save_plot(file_name='data/rt_explore_plot_time_series_datetime_sum_week_211.png',
                   plot=rt_explore_plot_time_series(dataset=dataset_11,
                                                    variable=variable,
                                                    comparison_variable=comparison_variable,
                                                    comparison_function=comp_func_sum,
                                                    comparison_function_name='Sum of',
                                                    show_points = TRUE,
                                                    show_labels = TRUE,
                                                    date_floor = 'week'))

})

test_that('rt_explore_plot_time_facet_yoy', {
    # bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
    # bike_traffic <- bike_traffic %>% mutate(date = mdy_hms(date))
    dataset <- readRDS('data/bike_traffic.RDS')
    set.seed(42)
    dataset <- dataset %>% sample_n(10000)
    dataset <- dataset %>% mutate(crossing = fct_lump(crossing, 1))
    #dataset %>% count(crossing)

    variable <- 'date'
    comparison_variable <- 'bike_count'

    comp_func_sum <- function(x) {
        return (sum(x, na.rm=TRUE))
    }

    ##########################################################################################################
    # weekly
    ##########################################################################################################

    ##################
    # Year-over-Year
    ##################
    # use to verify numbers
    # dataset %>%
    #     mutate(date = format_format('%Y-%W')(date)) %>%
    #     count(date) %>% arrange(date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__yoy_week.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    year_over_year = TRUE,
                                                    facet_variable = NULL,
                                                    color_variable = NULL,
                                                    comparison_variable = NULL,
                                                    comparison_function = NULL,
                                                    comparison_function_name = NULL,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '8 weeks'))

    ##################
    # count
    ##################
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_week.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    color_variable = NULL,
                                                    comparison_variable = NULL,
                                                    comparison_function = NULL,
                                                    comparison_function_name = NULL,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '8 weeks'))

    # use to verify numbers
    # dataset %>%
    #     mutate(date = format_format('%Y-%W')(date)) %>%
    #     count(date, direction) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_week__yoy.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    year_over_year = TRUE,
                                                    color_variable = NULL,
                                                    comparison_variable = NULL,
                                                    comparison_function = NULL,
                                                    comparison_function_name = NULL,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '8 weeks'))

    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_color_week.png',
                   plot=rt_explore_plot_time_series(dataset=dataset %>%
                                                        mutate(direction = fct_lump(direction, 1)),
                                                    variable=variable,
                                                    facet_variable = 'crossing',
                                                    color_variable = 'direction',
                                                    comparison_variable = NULL,
                                                    comparison_function = NULL,
                                                    comparison_function_name = NULL,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '8 weeks'))
    ##################
    # sum
    ##################
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'week')) %>%
    #     count(date, direction, wt=bike_count) %>% arrange(direction, date) %>% as.data.frame() %>% head(40)
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_week__sum.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    color_variable = NULL,
                                                    comparison_variable = comparison_variable,
                                                    comparison_function = comp_func_sum,
                                                    comparison_function_name = 'SUM',
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '8 weeks'))

    # use to verify numbers
    # dataset %>%
    #     mutate(date = format_format('%Y-%W')(date)) %>%
    #     count(date, direction, wt=bike_count) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_week__sum__yoy.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    year_over_year = TRUE,
                                                    color_variable = NULL,
                                                    comparison_variable = comparison_variable,
                                                    comparison_function = comp_func_sum,
                                                    comparison_function_name = 'SUM',
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '8 weeks'))

    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'week')) %>%
    #     mutate(direction = fct_lump(direction, 1)) %>%
    #     count(date, crossing, direction, wt=bike_count) %>% arrange(crossing, direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_color_week__sum.png',
                   plot=rt_explore_plot_time_series(dataset=dataset %>%
                                                        mutate(direction = fct_lump(direction, 1)),
                                                    variable=variable,
                                                    facet_variable = 'crossing',
                                                    color_variable = 'direction',
                                                    comparison_variable = comparison_variable,
                                                    comparison_function = comp_func_sum,
                                                    comparison_function_name = 'SUM',
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'week',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '8 weeks'))

    ##########################################################################################################
    # monthly
    ##########################################################################################################
    ##################
    # count
    ##################
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'month')) %>%
    #     count(date, direction) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_month.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    color_variable = NULL,
                                                    comparison_variable = NULL,
                                                    comparison_function = NULL,
                                                    comparison_function_name = NULL,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 months'))

    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'month')) %>%
    #     count(date, direction) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_month__yoy.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    year_over_year = TRUE,
                                                    color_variable = NULL,
                                                    comparison_variable = NULL,
                                                    comparison_function = NULL,
                                                    comparison_function_name = NULL,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 months'))
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'month')) %>%
    #     mutate(direction = fct_lump(direction, 1)) %>%
    #     count(date, crossing, direction) %>% arrange(crossing, direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_color_month.png',
                   plot=rt_explore_plot_time_series(dataset=dataset %>%
                                                        mutate(direction = fct_lump(direction, 1)),
                                                    variable=variable,
                                                    facet_variable = 'crossing',
                                                    color_variable = 'direction',
                                                    comparison_variable = NULL,
                                                    comparison_function = NULL,
                                                    comparison_function_name = NULL,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 months'))

    ##################
    # sum
    ##################
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'month')) %>%
    #     count(date, direction, wt=bike_count) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_month__sum.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    color_variable = NULL,
                                                    comparison_variable = comparison_variable,
                                                    comparison_function = comp_func_sum,
                                                    comparison_function_name = 'SUM',
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 months'))

    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'month')) %>%
    #     count(date, direction, wt=bike_count) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_month__sum__yoy.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    year_over_year = TRUE,
                                                    color_variable = NULL,
                                                    comparison_variable = comparison_variable,
                                                    comparison_function = comp_func_sum,
                                                    comparison_function_name = 'SUM',
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 months'))
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'month')) %>%
    #     mutate(direction = fct_lump(direction, 1)) %>%
    #     count(date, crossing, direction, wt=bike_count) %>% arrange(crossing, direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_color_month__sum.png',
                   plot=rt_explore_plot_time_series(dataset=dataset %>%
                                                        mutate(direction = fct_lump(direction, 1)),
                                                    variable=variable,
                                                    facet_variable = 'crossing',
                                                    color_variable = 'direction',
                                                    comparison_variable = comparison_variable,
                                                    comparison_function = comp_func_sum,
                                                    comparison_function_name = 'SUM',
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 months'))

    # to test include_zero_y_axis, lets make the numbers much higher than they are and make sure no values are 0
    comp_func_custom <- function(x) {
        x <- ifelse(x == 0, 100000, x * 10000)
        x <- sum(x, na.rm=TRUE)
        x <- ifelse(x > 30000000, 30000000, x)
        x <- ifelse(x < 1000000, 1000000, x)
        return (x)
    }
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_color_month__include_zero.png',
                   plot=rt_explore_plot_time_series(dataset=dataset %>%
                                                        mutate(direction = fct_lump(direction, 1)),
                                                    variable=variable,
                                                    facet_variable = 'crossing',
                                                    color_variable = 'direction',
                                                    include_zero_y_axis = FALSE,
                                                    comparison_variable = comparison_variable,
                                                    comparison_function = comp_func_custom,
                                                    comparison_function_name = 'Custom',
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'month',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 months'))
})

test_that('rt_explore_plot_time_series', {
    dataset <- data.frame(flights %>%
                              mutate(date = lubridate::make_date(year, month, day),
                                     cohort = paste0(year, '-',
                                                     lubridate::week(date)))) %>%
        select(date, dep_delay, dep_time, origin, cohort) %>%
        mutate(origin = factor(origin, levels = c("JFK", "LGA", "EWR"), ordered=TRUE))

    variable <- 'date'
    comparison_variable <- 'dep_delay'
    comp_func_sum <- function(x) {
        return (sum(x, na.rm=TRUE))
    }

    test_save_plot(file_name='data/rt_explore_plot_time_series__colors.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    color_variable='origin'))

    test_save_plot(file_name='data/rt_explore_plot_time_series__colors__comp.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    color_variable='origin',
                                                    comparison_variable=comparison_variable,
                                                    comparison_function = comp_func_sum,
                                                    comparison_function_name = 'Sum'))
})

test_that('rt_explore_plot_time_series__many_nas', {

    dataset <- data.frame(flights %>% mutate(date = lubridate::make_date(year, month, day))) %>%
        select(date, dep_delay, dep_time, origin)

    update_indexes <- which(dataset$date < ymd('2013-09-01'))
    dataset$date[update_indexes] <- NA

    variable <- 'date'
    color_variable <- 'origin'

    # if there are many NAs, it will mess up the count scale, and we can't plot them anyway
    test_save_plot(file_name='data/rt_explore_plot_time_series__many_nas.png',
                   plot=rt_explore_plot_time_series(dataset,
                                                    variable = variable,
                                                    color_variable = NULL))
    test_save_plot(file_name='data/rt_explore_plot_time_series__many_nas__color.png',
                   plot=rt_explore_plot_time_series(dataset,
                                                    variable = variable,
                                                    color_variable = color_variable))
})

test_that('rt_plot_funnel', {

    steps <- c("Step Z", "Step Y", "Step X", "Step W")
    values <- c(200, 60, 20, 10)

    test_save_plot(file_name='data/rt_plot_funnel_proportionate_FALSE.png',
                   plot=rt_funnel_plot(step_names=steps, step_values=values,
                                       title="My title", subtitle = "My Subtitle", caption = "My Caption",
                                       proportionate=FALSE))
    test_save_plot(file_name='data/rt_plot_funnel_proportionate_TRUE.png',
                   plot=rt_funnel_plot(step_names=steps, step_values=values,
                                       title="My title", subtitle = "My Subtitle", caption = "My Caption",
                                       proportionate=TRUE))

    steps <- c("Step W", "Step X", "Step Y", "Step Z")
    values <- c(2000, 1111, 50, 11)

    test_save_plot(file_name='data/rt_plot_funnel_2_proportionate_FALSE.png',
                   plot=rt_funnel_plot(step_names=steps, step_values=values, proportionate=FALSE))
    test_save_plot(file_name='data/rt_plot_funnel_2_proportionate_TRUE.png',
                   plot=rt_funnel_plot(step_names=steps, step_values=values, proportionate=TRUE))
})

test_that('rt_explore_plot_conversion_rates', {

    sample_size <- 20000
    conversion_rate <- 0.3

    set.seed(42)
    conversion_data <- data.frame(index=1:sample_size,
                                  first_visit=ymd_hms('2019-01-01 00:00:00') +
                                      days(round(runif(n=sample_size, min=0, max=600))) +
                                      hours(round(runif(n=sample_size, min=0, max=23))) +
                                      minutes(round(runif(n=sample_size, min=0, max=60))) +
                                      seconds(round(runif(n=sample_size, min=0, max=60))))

    set.seed(43)
    conversion_data$converted <- as.logical(rbinom(n=sample_size, size=1, prob=conversion_rate))

    get_rand_binom_num <- function(seed, max_num) {
        set.seed(seed)
        rbinom(1, max_num, 0.3)
    }
    get_rand_unif_num <- function(seed, max_num) {
        set.seed(seed)
        as.integer(round(runif(n=1, min=0, max=max_num)))
    }

    conversion_data$num_days <- map_int(conversion_data$index, ~ get_rand_binom_num(., 39))
    conversion_data$num_hours <- map_int(conversion_data$index, ~ get_rand_unif_num(., 23))
    conversion_data <- conversion_data %>%
        mutate(conversion_date = first_visit +
                   days(num_days) +
                   hours(num_hours)) %>%
        select(-num_days, -num_hours)

    conversion_data$conversion_date[which(!conversion_data$converted)] <- NA
    conversion_data <- conversion_data %>% select(-index, -converted)

    mock_reference_date <- max(conversion_data$first_visit)

    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__days_month.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     reference_date=mock_reference_date,
                                     snapshots=c(6, 7, 10, 14),
                                     snapshot_units='days',
                                     date_floor='month',
                                     color_or_facet='color',
                                     year_over_year=FALSE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=TRUE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format=NULL,
                                     date_breaks_width=NULL))

    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__days_month_facet.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     reference_date=mock_reference_date,
                                     snapshots=c(6, 7, 10, 14),
                                     snapshot_units='days',
                                     date_floor='month',
                                     color_or_facet='facet',
                                     year_over_year=FALSE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=TRUE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format=NULL,
                                     date_breaks_width=NULL))
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__days_month_yoy.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     reference_date=mock_reference_date,
                                     snapshots=c(6, 7, 10, 14),
                                     snapshot_units='days',
                                     date_floor='month',
                                     color_or_facet='facet',
                                     year_over_year=TRUE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=FALSE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format=NULL,
                                     date_breaks_width=NULL))
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__2.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     reference_date=mock_reference_date,
                                     snapshots=c(7, 14, 21),
                                     snapshot_units='days',
                                     date_floor='month',
                                     color_or_facet='facet',
                                     year_over_year=TRUE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=FALSE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format=NULL,
                                     date_breaks_width=NULL))

    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__weeks_weeks.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     reference_date=mock_reference_date,
                                     snapshots=c(1, 2, 3),
                                     snapshot_units='weeks',
                                     date_floor='weeks',
                                     color_or_facet='color',
                                     year_over_year=FALSE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=TRUE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format=NULL,
                                     date_breaks_width='4 weeks'))

    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__weeks_weeks_facet.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     reference_date=mock_reference_date,
                                     snapshots=c(1, 2, 3),
                                     snapshot_units='weeks',
                                     date_floor='weeks',
                                     color_or_facet='facet',
                                     year_over_year=FALSE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=TRUE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format=NULL,
                                     date_breaks_width='4 weeks'))
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__weeks_weeks_yoy.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     reference_date=mock_reference_date,
                                     snapshots=c(1, 2, 3),
                                     snapshot_units='weeks',
                                     date_floor='weeks',
                                     color_or_facet='facet',
                                     year_over_year=TRUE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=TRUE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format=NULL,
                                     date_breaks_width='4 weeks'))
    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__weeks_weeks_2.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     reference_date=mock_reference_date,
                                     snapshots=c(1, 2, 3),
                                     snapshot_units='weeks',
                                     date_floor='weeks',
                                     color_or_facet='facet',
                                     year_over_year=TRUE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=FALSE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format='%Y-%m-%d',
                                     date_breaks_width='4 weeks'))
})
