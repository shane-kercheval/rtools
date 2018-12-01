context('Exploratory Analysis')
library(testthat)
library(ggplot2)

test_that("rt_get_date_fields_lubridate", {

    #library(lubridate)
    #library(timeDate)
    #library(purrr)
    #library(dplyr)
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
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=results,
                                                  rds_file='data/rt_get_date_fields_lubridate.RDS'))
})
