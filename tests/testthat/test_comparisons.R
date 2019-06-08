context('Test Comparisons')
library(ggplot2)

test_that("rt_are_dataframes_equal", {

    # test NA values
    other <- iris
    expect_true(rt_are_dataframes_equal(dataframe1=iris, dataframe2=other))
    other[1, 'Sepal.Width'] <- NA
    expect_false(rt_are_dataframes_equal(dataframe1=iris, dataframe2=other))

    # test different values
    other <- iris
    expect_true(rt_are_dataframes_equal(dataframe1=iris, dataframe2=other))
    other[1, 'Sepal.Width'] <- 0
    expect_false(rt_are_dataframes_equal(dataframe1=iris, dataframe2=other))

    # test different row names
    other <- iris
    expect_true(rt_are_dataframes_equal(dataframe1=iris, dataframe2=other))
    rownames(other)[1] <- 'a'
    expect_false(rt_are_dataframes_equal(dataframe1=iris, dataframe2=other))

    # test different column names
    other <- iris
    expect_true(rt_are_dataframes_equal(dataframe1=iris, dataframe2=other))
    colnames(other)[1] <- 'a'
    expect_false(rt_are_dataframes_equal(dataframe1=iris, dataframe2=other))

    # testing this becaue it originally didn't fucking work because apparently `all(rownames(iris) == rownames(list()))`` is TRUE
    expect_false(rt_are_dataframes_equal(dataframe1=iris, dataframe2=list()))
})

test_that("rt_are_dataframes_equal_from_file", {
    rds_file <- 'temp.RDS'

    # test same values
    saveRDS(object=iris, file=rds_file)
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=iris, rds_file=rds_file))
    file.remove(rds_file)


    # test NA values
    other <- iris
    other[1, 'Sepal.Width'] <- NA
    saveRDS(object=other, file=rds_file)
    expect_false(rt_are_dataframes_equal_from_file(dataframe1=iris, rds_file=rds_file))
    file.remove(rds_file)

    # test different values
    other <- iris
    other[1, 'Sepal.Width'] <- 0
    saveRDS(object=other, file=rds_file)
    expect_false(rt_are_dataframes_equal_from_file(dataframe1=iris, rds_file=rds_file))
    file.remove(rds_file)

    # test different row names
    other <- iris
    rownames(other)[1] <- 'a'
    saveRDS(object=other, file=rds_file)
    expect_false(rt_are_dataframes_equal_from_file(dataframe1=iris, rds_file=rds_file))
    file.remove(rds_file)

    # test different column names
    other <- iris
    colnames(other)[1] <- 'a'
    saveRDS(object=other, file=rds_file)
    expect_false(rt_are_dataframes_equal_from_file(dataframe1=iris, rds_file=rds_file))
    file.remove(rds_file)
})

test_that("rt_setsubset", {
    set_a <- c(1, 2, 3)
    set_b <- c('a', 'b', 'c')

    expect_true(rt_setsubset(1, set_a))
    expect_true(rt_setsubset(2, set_a))
    expect_true(rt_setsubset(3, set_a))
    expect_true(rt_setsubset(c(1), set_a))
    expect_true(rt_setsubset(c(1, 2), set_a))
    expect_true(rt_setsubset(c(2, 3), set_a))
    expect_true(rt_setsubset(c(2, 2, 3), set_a))
    expect_true(rt_setsubset(c(1, 2, 3, 3, 2, 1), set_a))

    expect_false(rt_setsubset(4, set_a))
    expect_false(rt_setsubset(0, set_a))
    expect_false(rt_setsubset(1.0001, set_a))
    expect_false(rt_setsubset(c(1, 2, 4), set_a))
    expect_false(rt_setsubset(c(1, 2, 3, 3, 2, 1, 0), set_a))

    expect_true(rt_setsubset('a', set_b))
    expect_true(rt_setsubset('b', set_b))
    expect_true(rt_setsubset('c', set_b))
    expect_true(rt_setsubset(c('a'), set_b))
    expect_true(rt_setsubset(c('a', 'b'), set_b))
    expect_true(rt_setsubset(c('b', 'c'), set_b))
    expect_true(rt_setsubset(c('b', 'b', 'c'), set_b))
    expect_true(rt_setsubset(c('a', 'b', 'c', 'c', 'b', 'a'), set_b))

    expect_false(rt_setsubset('f', set_b))
    expect_false(rt_setsubset(c('a', 'b', 'd'), set_b))
    expect_false(rt_setsubset(c('a', 'b', 'c', 'c', 'b', 'a', 0), set_b))
})
