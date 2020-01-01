context('Exploratory Analysis - Time Series')
library(testthat)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gapminder)
library(nycflights13)
library(forcats)
# library(scales)

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

test_that('rt_as_year_qtr_format', {

    expected_values <- c("2019-Q1", "", "", "", "2019-Q2", "", "", "2019-Q3", "", "", "2019-Q4", "", "", "", "2020-Q1", NA, "2019-Q1")
    actual_values <- rt_as_year_qtr_format(c(ymd('2019-01-01',
                                                 '2019-01-02',
                                                 '2019-02-01',
                                                 '2019-03-04',
                                                 '2019-04-01',
                                                 '2019-04-04',
                                                 '2019-06-01',
                                                 '2019-07-01',
                                                 '2019-08-01',
                                                 '2019-09-01',
                                                 '2019-10-01',
                                                 '2019-11-01',
                                                 '2019-12-01',
                                                 '2019-12-31',
                                                 '2020-01-01',
                                                 NA,
                                                 '2019-01-01')))
    expect_identical(expected_values, actual_values)
    actual_values <- rt_as_year_qtr_format(as.POSIXct(c(ymd('2019-01-01',
                                                 '2019-01-02',
                                                 '2019-02-01',
                                                 '2019-03-04',
                                                 '2019-04-01',
                                                 '2019-04-04',
                                                 '2019-06-01',
                                                 '2019-07-01',
                                                 '2019-08-01',
                                                 '2019-09-01',
                                                 '2019-10-01',
                                                 '2019-11-01',
                                                 '2019-12-01',
                                                 '2019-12-31',
                                                 '2020-01-01',
                                                 NA,
                                                 '2019-01-01'))))
    expect_identical(expected_values, actual_values)
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

    ##########################################################################################################
    # quarterly
    ##########################################################################################################
    ##################
    # count
    ##################
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'quarter')) %>%
    #     count(date, direction) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_quarter.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    color_variable = NULL,
                                                    comparison_variable = NULL,
                                                    comparison_function = NULL,
                                                    comparison_function_name = NULL,
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'quarter',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 quarters'))

    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'quarter')) %>%
    #     count(date, direction) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_quarter__yoy.png',
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
                                                    date_floor = 'quarter',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 quarters'))
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'quarter')) %>%
    #     mutate(direction = fct_lump(direction, 1)) %>%
    #     count(date, crossing, direction) %>% arrange(crossing, direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_color_quarter.png',
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
                                                    date_floor = 'quarter',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 quarters'))

    ##################
    # sum
    ##################
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'quarter')) %>%
    #     count(date, direction, wt=bike_count) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_quarter__sum.png',
                   plot=rt_explore_plot_time_series(dataset=dataset,
                                                    variable=variable,
                                                    facet_variable = 'direction',
                                                    color_variable = NULL,
                                                    comparison_variable = comparison_variable,
                                                    comparison_function = comp_func_sum,
                                                    comparison_function_name = 'SUM',
                                                    show_labels = TRUE,
                                                    show_points = TRUE,
                                                    date_floor = 'quarter',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 quarters'))

    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'quarter')) %>%
    #     count(date, direction, wt=bike_count) %>% arrange(direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_quarter__sum__yoy.png',
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
                                                    date_floor = 'quarter',
                                                    date_break_format = NULL,
                                                    date_breaks_width = '2 quarters'))
    # use to verify numbers
    # dataset %>%
    #     mutate(date = rt_floor_date_factor(date, 'quarter')) %>%
    #     mutate(direction = fct_lump(direction, 1)) %>%
    #     count(date, crossing, direction, wt=bike_count) %>% arrange(crossing, direction, date) %>% as.data.frame() %>% View()
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_color_quarter__sum.png',
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
                                                    date_floor = 'quarter',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 quarters'))

    # to test include_zero_y_axis, lets make the numbers much higher than they are and make sure no values are 0
    comp_func_custom <- function(x) {
        x <- ifelse(x == 0, 100000, x * 10000)
        x <- sum(x, na.rm=TRUE)
        x <- ifelse(x > 30000000, 30000000, x)
        x <- ifelse(x < 1000000, 1000000, x)
        return (x)
    }
    test_save_plot(file_name='data/rt_explore_plot_time_series__facet_color_quarter__include_zero.png',
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
                                                    date_floor = 'quarter',
                                                    date_break_format = '%Y-%m-%d',
                                                    date_breaks_width = '2 quarters'))
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
    conversion_data <- conversion_data %>% mutate(group=ifelse(index %% 2 == 0, 'A', 'B'))
    conversion_data <- conversion_data %>% select(-index, -converted)

    mock_reference_date <- max(conversion_data$first_visit)

    ##########################################################################################################
    # Non-Groups
    ##########################################################################################################
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

    # there was a bug where unique values were being removed (e.g. so duplicated first_visit's would be removed)
    # so the above and below conversion rates, which should be (about) the same, were actually different
    # the graphs won't actually be the same because by doing round_date we lose some precision on the date,
    # and therefore the date-diff between the first-visit and the conversion-date, but they should be close
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__days_month__floor_bug.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data %>%
                                                             mutate(first_visit = round_date(first_visit, unit='days')),
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
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__days_month_yoy_ignore_color.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                                         first_date='first_visit',
                                                         second_date='conversion_date',
                                                         reference_date=mock_reference_date,
                                                         snapshots=c(6, 7, 10, 14),
                                                         snapshot_units='days',
                                                         date_floor='month',
                                                         color_or_facet='color',
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

    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__weeks_weeks2.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data %>%
                                                             filter(first_visit >= ymd_hms('2020-01-01 00:00:00')),
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
                                                         date_break_format='%Y-%m-%d'
                                                         #date_breaks_width='4 weeks'
                                                         ))

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

    ##########################################################################################################
    # GROUPS
    ##########################################################################################################


    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__days_month__groups.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     group_variable='group',
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


    # there was a bug where unique values were being removed (e.g. so duplicated first_visit's would be removed)
    # so the above and below conversion rates, which should be (about) the same, were actually different
    # the graphs won't actually be the same because by doing round_date we lose some precision on the date,
    # and therefore the date-diff between the first-visit and the conversion-date, but they should be close
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__days_month__groups__floor_bug.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data %>%
                                                             mutate(first_visit = round_date(first_visit, unit='days')),
                                                         first_date='first_visit',
                                                         second_date='conversion_date',
                                                         group_variable='group',
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
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__days_month_facet__groups.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     group_variable='group',
                                     reference_date=mock_reference_date,
                                     snapshots=c(6, 7, 10, 14),
                                     snapshot_units='days',
                                     date_floor='month',
                                     color_or_facet='facet',
                                     year_over_year=FALSE,
                                     y_zoom_min=NULL,
                                     y_zoom_max=NULL,
                                     include_zero_y_axis=FALSE,
                                     show_points=TRUE,
                                     show_labels=TRUE,
                                     date_break_format=NULL,
                                     date_breaks_width=NULL))
    ##########################################################################################################
    expect_error(rt_explore_plot_conversion_rates(dataset=conversion_data,
                                                  first_date='first_visit',
                                                  second_date='conversion_date',
                                                  group_variable='group',
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
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__weeks_weeks__groups.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     group_variable='group',
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

    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__weeks_weeks2__groups.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data %>%
                                                             filter(first_visit >= ymd_hms('2020-01-01 00:00:00')),
                                                         first_date='first_visit',
                                                         second_date='conversion_date',
                                                         group_variable='group',
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
                                                         date_break_format='%Y-%m-%d'
                                                         #date_breaks_width='4 weeks'
                                                         ))

    ##########################################################################################################
    test_save_plot(file_name='data/rt_explore_plot_conversion_rates__weeks_weeks_facet__groups.png',
                   plot=rt_explore_plot_conversion_rates(dataset=conversion_data,
                                     first_date='first_visit',
                                     second_date='conversion_date',
                                     group_variable='group',
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
})

test_that('rt_explore_plot_cohorted_adoption', {
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

    test_save_plot(file_name='data/rt_explore_plot_cohorted_adoption__30_days_month.png',
                   plot=rt_explore_plot_cohorted_adoption(dataset=conversion_data,
                                                          first_date='first_visit',
                                                          second_date='conversion_date',
                                                          reference_date=mock_reference_date,
                                                          last_n_cohorts=10,
                                                          n_units_after_first_date=30,
                                                          units='days',
                                                          date_floor='month',
                                                          y_zoom_min=NULL,
                                                          y_zoom_max=NULL,
                                                          include_zero_y_axis=TRUE,
                                                          show_points=FALSE,
                                                          show_labels=FALSE,
                                                          date_break_format=NULL,
                                                          base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_cohorted_adoption__30_days_month_options.png',
                   plot=rt_explore_plot_cohorted_adoption(dataset=conversion_data,
                                                          first_date='first_visit',
                                                          second_date='conversion_date',
                                                          reference_date=mock_reference_date,
                                                          last_n_cohorts=10,
                                                          n_units_after_first_date=30,
                                                          units='days',
                                                          date_floor='month',
                                                          y_zoom_min=NULL,
                                                          y_zoom_max=0.30,
                                                          include_zero_y_axis=TRUE,
                                                          show_points=TRUE,
                                                          show_labels=TRUE,
                                                          date_break_format='%Y-%m-%d',
                                                          base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_cohorted_adoption__3_weeks_week.png',
                   plot=rt_explore_plot_cohorted_adoption(dataset=conversion_data,
                                                          first_date='first_visit',
                                                          second_date='conversion_date',
                                                          reference_date=mock_reference_date,
                                                          last_n_cohorts=15,
                                                          n_units_after_first_date=3,
                                                          units='weeks',
                                                          date_floor='week',
                                                          y_zoom_min=NULL,
                                                          #y_zoom_max=0.30,
                                                          include_zero_y_axis=TRUE,
                                                          #show_points=TRUE,
                                                          #show_labels=TRUE,
                                                          #date_break_format='%Y-%m-%d',
                                                          base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_cohorted_adoption__3_weeks_month_options1.png',
                   plot=rt_explore_plot_cohorted_adoption(dataset=conversion_data,
                                                          first_date='first_visit',
                                                          second_date='conversion_date',
                                                          reference_date=mock_reference_date,
                                                          last_n_cohorts=100,
                                                          n_units_after_first_date=4,
                                                          units='weeks',
                                                          date_floor='month',
                                                          y_zoom_min=NULL,
                                                          separated_colors=FALSE,
                                                          #y_zoom_max=0.30,
                                                          include_zero_y_axis=TRUE,
                                                          show_points=TRUE,
                                                          #show_labels=TRUE,
                                                          #date_break_format='%Y-%W',
                                                          base_size=11))

    test_save_plot(file_name='data/rt_explore_plot_cohorted_adoption__3_weeks_month_options2.png',
                   plot=rt_explore_plot_cohorted_adoption(dataset=conversion_data,
                                                          first_date='first_visit',
                                                          second_date='conversion_date',
                                                          reference_date=mock_reference_date,
                                                          last_n_cohorts=100,
                                                          n_units_after_first_date=4,
                                                          units='weeks',
                                                          date_floor='month',
                                                          y_zoom_min=NULL,
                                                          separated_colors=TRUE,
                                                          #y_zoom_max=0.30,
                                                          include_zero_y_axis=TRUE,
                                                          show_points=TRUE,
                                                          #show_labels=TRUE,
                                                          #date_break_format='%Y-%W',
                                                          base_size=11))
})
