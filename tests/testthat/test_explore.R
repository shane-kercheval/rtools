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

    result <- rt_explore_numeric_summary(iris)

    rds_file <- 'data/rt_explore_numeric_summary_iris.RDS'
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=result, rds_file=rds_file))
})
