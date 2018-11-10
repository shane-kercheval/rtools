context('Exploratory Analysis')
library(ggplot2)

test_that("rt_explore_numeric_summary", {
    iris[1, 'Sepal.Width'] <- NA
    iris[2, 'Sepal.Width'] <- NA
    iris[3, 'Sepal.Width'] <- NA
    iris[1, 'Petal.Length'] <- NA
    iris[1, 'Sepal.Length'] <- 0
    iris[2, 'Sepal.Length'] <- 0
    iris[3, 'Sepal.Length'] <- 0
    iris[1, 'Petal.Width'] <- 0

    result <- rt_explore_numeric_summary(dataset=iris)

    rds_file <- 'data/rt_explore_numeric_summary_iris.RDS'
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=result, rds_file=rds_file))
})

test_that("rt_explore_categoric_summary", {

    iris[1, 'Species'] <- NA
    iris[2, 'Species'] <- NA
    iris[3, 'Species'] <- NA

    result <- rt_explore_categoric_summary(dataset=iris)

    rds_file <- 'data/rt_explore_categoric_summary_iris.RDS'
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=result, rds_file=rds_file))
})
