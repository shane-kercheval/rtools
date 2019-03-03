context('General')
library(testthat)
library(dplyr)

test_that("rt_get_date_fields_lubridate", {

    #library(lubridate)
    #library(timeDate)
    #library(purrr)
    #library(dplyr)
    #library(stringr)
    reference_date <- lubridate::as_date('2018-12-01')
    expect_true(year(reference_date) == 2018)
    expect_true(month(reference_date) == 12)
    expect_true(day(reference_date) == 1)

    date_vector <- lubridate::as_date('2018-01-01') + seq(0, 400)
    # make sure NAs are handled
    date_vector[2] <- NA
    date_vector[30] <- NA
    date_vector[31] <- NA

    results <- rt_get_date_fields(date_vector = date_vector, reference_date=reference_date)
    expect_identical(levels(results$month_name), c('January', 'February', 'March', 'April', 'May', 'June',
                                                   'July', 'August', 'September', 'October', 'November',
                                                   'December'))
    expect_identical(levels(results$day_name), c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
                                                 'Saturday', 'Sunday'))
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=results,
                                                  rds_file='data/rt_get_date_fields_lubridate.RDS'))
    expect_identical(levels(results$cohort_quarter), c("2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4", "2019-Q1"))

    # same thing but with string reference date and date vector
    reference_date <- '2018-12-01'

    date_vector <- as.character(lubridate::as_date('2018-01-01') + seq(0, 400))
    # make sure NAs are handled
    date_vector[2] <- NA
    date_vector[30] <- NA
    date_vector[31] <- NA

    results <- rt_get_date_fields(date_vector = date_vector, reference_date=reference_date)

    expect_identical(levels(results$month_name),
                     c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
                       'October', 'November', 'December'))
    expect_identical(levels(results$day_name),
                     c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=results,
                                                  rds_file='data/rt_get_date_fields_lubridate.RDS'))
})

test_that("rt_get_date_fields_POSIXlt", {

    # same reference date as above but with POSIXlt
    reference_date <- as.POSIXlt('2018-12-01')
    expect_true(year(reference_date) == 2018)
    expect_true(month(reference_date) == 12)
    expect_true(day(reference_date) == 1)

    # same date vector as above but with POSIXlt
    date_vector <- as.POSIXlt('2018-01-01') + as.difftime(seq(0, 400), unit='days')
    # make sure NAs are handled
    date_vector[2] <- NA
    date_vector[30] <- NA
    date_vector[31] <- NA

    results <- rt_get_date_fields(date_vector = date_vector, reference_date=reference_date)
    expect_identical(levels(results$month_name),
                     c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
                       'October', 'November', 'December'))
    expect_identical(levels(results$day_name),
                    c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=results,
                                                  rds_file='data/rt_get_date_fields_lubridate.RDS'))
})

test_that("rt_get_date_fields_POSIXlt", {

    # same reference date as above but with POSIXlt
    reference_date <- as.POSIXct('2018-12-01 9:00:00')
    expect_true(year(reference_date) == 2018)
    expect_true(month(reference_date) == 12)
    expect_true(day(reference_date) == 1)

    # same date vector as above but with POSIXlt
    date_vector <- as.POSIXct('2018-01-01 10:00:00') + as.difftime(seq(0, 400), unit='days')
    # make sure NAs are handled
    date_vector[2] <- NA
    date_vector[30] <- NA
    date_vector[31] <- NA

    results <- rt_get_date_fields(date_vector = date_vector, reference_date=reference_date)
    expect_identical(levels(results$month_name), c('January', 'February', 'March', 'April', 'May', 'June',
                                                   'July', 'August', 'September', 'October', 'November',
                                                   'December'))
    expect_identical(levels(results$day_name), c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
                                                 'Saturday', 'Sunday'))
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=results,
                                                  rds_file='data/rt_get_date_fields_lubridate.RDS'))
})

test_that("rt_get_vector", {
    expected_a <- c(1, 2, 3)
    expected_b <- c('a', 'b', 'c')
    df <- data.frame(a=expected_a, b=expected_b, stringsAsFactors = FALSE)
    expect_identical(expected_a, df %>% rt_get_vector('a'))
    expect_identical(expected_b, df %>% rt_get_vector('b'))

    # test when getting only 1 row
    expect_equal(df %>% filter(a == 2) %>% rt_get_vector('a'), 2)
    expect_equal(df %>% filter(a == 2) %>% rt_get_vector('b'), 'b')

    expect_equal(df %>% filter(b == 'c') %>% rt_get_vector('a'), 3)
    expect_equal(df %>% filter(b == 'c') %>% rt_get_vector('b'), 'c')

    # empty df
    df <- df %>% filter(a == 4)
    expect_equal(length(df %>% rt_get_vector('a')), 0)
})

test_that("rt_remove_val", {
    vector_a <- c(1, 2, 3)
    vector_b <- c('a', 'b', 'c')

    expect_identical(vector_a %>% rt_remove_val(1), c(2, 3))
    expect_identical(vector_a %>% rt_remove_val(2), c(1, 3))
    expect_identical(vector_a %>% rt_remove_val(3), c(1, 2))
    # doesn't exist
    expect_identical(vector_a %>% rt_remove_val(4), vector_a)

    expect_identical(vector_b %>% rt_remove_val('a'), c('b', 'c'))
    expect_identical(vector_b %>% rt_remove_val('b'), c('a', 'c'))
    expect_identical(vector_b %>% rt_remove_val('c'), c('a', 'b'))
    # doesn't exist
    expect_identical(vector_b %>% rt_remove_val('d'), vector_b)
})

test_that("rt_ceiling_nearest_x", {
    nearest_x <- 0.05
    expect_equal(0, rt_ceiling_nearest_x(0, nearest_x))
    expect_equal(0.05, rt_ceiling_nearest_x(0.01, nearest_x))
    expect_equal(0.05, rt_ceiling_nearest_x(0.04, nearest_x))
    expect_equal(0.05, rt_ceiling_nearest_x(0.05, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.06, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.09, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.10, nearest_x))
    expect_equal(0.15, rt_ceiling_nearest_x(0.11, nearest_x))

    expect_equal(-0.05, rt_ceiling_nearest_x(-0.01, nearest_x))
    expect_equal(-0.05, rt_ceiling_nearest_x(-0.04, nearest_x))
    expect_equal(-0.05, rt_ceiling_nearest_x(-0.05, nearest_x))
    expect_equal(-0.10, rt_ceiling_nearest_x(-0.06, nearest_x))
    expect_equal(-0.10, rt_ceiling_nearest_x(-0.09, nearest_x))
    expect_equal(-0.10, rt_ceiling_nearest_x(-0.10, nearest_x))
    expect_equal(-0.15, rt_ceiling_nearest_x(-0.11, nearest_x))

    nearest_x <- 0.10
    expect_equal(0, rt_ceiling_nearest_x(0, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.01, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.04, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.05, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.06, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.09, nearest_x))
    expect_equal(0.10, rt_ceiling_nearest_x(0.10, nearest_x))
    expect_equal(0.20, rt_ceiling_nearest_x(0.11, nearest_x))

    expect_equal(-0.10, rt_ceiling_nearest_x(-0.01, nearest_x))
    expect_equal(-0.10, rt_ceiling_nearest_x(-0.04, nearest_x))
    expect_equal(-0.10, rt_ceiling_nearest_x(-0.05, nearest_x))
    expect_equal(-0.10, rt_ceiling_nearest_x(-0.06, nearest_x))
    expect_equal(-0.10, rt_ceiling_nearest_x(-0.09, nearest_x))
    expect_equal(-0.10, rt_ceiling_nearest_x(-0.10, nearest_x))
    expect_equal(-0.20, rt_ceiling_nearest_x(-0.11, nearest_x))
})

test_that("rt_stopif", {

    good_condition <- TRUE
    bad_condition <- FALSE

    # stopifnot expects you to pass in what you consider as "good" or expected
    expect_null(stopifnot(good_condition))
    expect_error(stopifnot(bad_condition))

    # rt_stopif expects you to pass in what you consider as "bad" or unspected
    expect_null(rt_stopif(bad_condition))
    expect_error(rt_stopif(good_condition))
})
