context('Test Helpers')
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
