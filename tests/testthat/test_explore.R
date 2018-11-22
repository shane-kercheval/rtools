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

    correlations <- rt_explore_correlations(dataset=credit_data,
                                            corr_threshold=0,
                                            p_value_threshold=1,
                                            type='pearson')

    rds_file <- 'data/rt_correlations_credit.RDS'
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=data.frame(correlations), rds_file=rds_file))

    correlation_plot_file <- 'data/rt_explore_plot_correlations_credit.png'
    if (file.exists(correlation_plot_file)) file.remove(correlation_plot_file)
    ggsave(filename=correlation_plot_file,
           plot=rt_explore_plot_correlations(dataset=credit_data,
                                             corr_threshold=0,
                                             p_value_threshold=1,
                                             type='pearson'))

    expect_true(file.exists(correlation_plot_file))



    # todo: test different parameters


})
