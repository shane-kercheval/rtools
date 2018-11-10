context('Exploratory Analysis')
library(ggplot2)

test_that("str_length is number of characters", {
    iris[1, 'Sepal.Width'] <- NA
    iris[2, 'Sepal.Width'] <- NA
    iris[3, 'Sepal.Width'] <- NA
    iris[1, 'Petal.Length'] <- NA
    iris[1, 'Sepal.Length'] <- 0
    iris[2, 'Sepal.Length'] <- 0
    iris[3, 'Sepal.Length'] <- 0
    iris[1, 'Petal.Width'] <- 0

    result <- explore_numeric_summary(iris)
    expect_equal(nrow(result), 4)
    #expect_true(round(result['Sepal.Width'], 5) == 3.05733)
})
