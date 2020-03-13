context('General')
library(testthat)
library(lubridate)
library(dplyr)

test_that("rt_get_date_fields_lubridate", {

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

    expected_week_levels <- c('2018-W01', '2018-W02', '2018-W03', '2018-W04', '2018-W05', '2018-W06', '2018-W07',
                              '2018-W08', '2018-W09', '2018-W10', '2018-W11', '2018-W12', '2018-W13', '2018-W14', '2018-W15',
                              '2018-W16', '2018-W17', '2018-W18', '2018-W19', '2018-W20', '2018-W21', '2018-W22', '2018-W23',
                              '2018-W24', '2018-W25', '2018-W26', '2018-W27', '2018-W28', '2018-W29', '2018-W30', '2018-W31',
                              '2018-W32', '2018-W33', '2018-W34', '2018-W35', '2018-W36', '2018-W37', '2018-W38', '2018-W39',
                              '2018-W40', '2018-W41', '2018-W42', '2018-W43', '2018-W44', '2018-W45', '2018-W46', '2018-W47',
                              '2018-W48', '2018-W49', '2018-W50', '2018-W51', '2018-W52', '2018-W53', '2019-W01', '2019-W02',
                              '2019-W03', '2019-W04', '2019-W05', '2019-W06')
    expect_identical(levels(results$cohort_week), expected_week_levels)
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

test_that("rt_get_date_fields_random_order", {


    reference_date <- lubridate::as_date('2018-12-01')
    expect_true(year(reference_date) == 2018)
    expect_true(month(reference_date) == 12)
    expect_true(day(reference_date) == 1)

    date_vector <- lubridate::as_date('2018-01-01') + seq(0, 400)
    # make sure NAs are handled
    date_vector[2] <- NA
    date_vector[30] <- NA
    date_vector[31] <- NA
    # randomize
    set.seed(42)
    new_indices <- base::sample(length(date_vector))
    date_vector <- date_vector[new_indices]

    t <- data.frame(original_indices=1:length(date_vector),
               new_indices=new_indices)
    original_indices_map <- t %>% arrange(new_indices) %>% rt_get_vector('original_indices')

    results <- rt_get_date_fields(date_vector = date_vector, reference_date=reference_date)
    expect_identical(levels(results$month_name), c('January', 'February', 'March', 'April', 'May', 'June',
                                                   'July', 'August', 'September', 'October', 'November',
                                                   'December'))
    expect_identical(levels(results$day_name), c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
                                                 'Saturday', 'Sunday'))
    expected_week_levels <- c('2018-W01', '2018-W02', '2018-W03', '2018-W04', '2018-W05', '2018-W06', '2018-W07',
                              '2018-W08', '2018-W09', '2018-W10', '2018-W11', '2018-W12', '2018-W13', '2018-W14', '2018-W15',
                              '2018-W16', '2018-W17', '2018-W18', '2018-W19', '2018-W20', '2018-W21', '2018-W22', '2018-W23',
                              '2018-W24', '2018-W25', '2018-W26', '2018-W27', '2018-W28', '2018-W29', '2018-W30', '2018-W31',
                              '2018-W32', '2018-W33', '2018-W34', '2018-W35', '2018-W36', '2018-W37', '2018-W38', '2018-W39',
                              '2018-W40', '2018-W41', '2018-W42', '2018-W43', '2018-W44', '2018-W45', '2018-W46', '2018-W47',
                              '2018-W48', '2018-W49', '2018-W50', '2018-W51', '2018-W52', '2018-W53', '2019-W01', '2019-W02',
                              '2019-W03', '2019-W04', '2019-W05', '2019-W06')
    expect_identical(levels(results$cohort_week), expected_week_levels)

    results <- results[original_indices_map, ]
    rownames(results) <- t$original_indices
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

    date_vector <- date_vector[new_indices]

    results <- rt_get_date_fields(date_vector = date_vector, reference_date=reference_date)

    expect_identical(levels(results$month_name),
                     c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
                       'October', 'November', 'December'))
    expect_identical(levels(results$day_name),
                     c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    expected_week_levels <- c('2018-W01', '2018-W02', '2018-W03', '2018-W04', '2018-W05', '2018-W06', '2018-W07',
                              '2018-W08', '2018-W09', '2018-W10', '2018-W11', '2018-W12', '2018-W13', '2018-W14', '2018-W15',
                              '2018-W16', '2018-W17', '2018-W18', '2018-W19', '2018-W20', '2018-W21', '2018-W22', '2018-W23',
                              '2018-W24', '2018-W25', '2018-W26', '2018-W27', '2018-W28', '2018-W29', '2018-W30', '2018-W31',
                              '2018-W32', '2018-W33', '2018-W34', '2018-W35', '2018-W36', '2018-W37', '2018-W38', '2018-W39',
                              '2018-W40', '2018-W41', '2018-W42', '2018-W43', '2018-W44', '2018-W45', '2018-W46', '2018-W47',
                              '2018-W48', '2018-W49', '2018-W50', '2018-W51', '2018-W52', '2018-W53', '2019-W01', '2019-W02',
                              '2019-W03', '2019-W04', '2019-W05', '2019-W06')
    expect_identical(levels(results$cohort_week), expected_week_levels)
    results <- results[original_indices_map, ]
    rownames(results) <- t$original_indices
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
    expected_week_levels <- c('2018-W01', '2018-W02', '2018-W03', '2018-W04', '2018-W05', '2018-W06', '2018-W07',
                              '2018-W08', '2018-W09', '2018-W10', '2018-W11', '2018-W12', '2018-W13', '2018-W14', '2018-W15',
                              '2018-W16', '2018-W17', '2018-W18', '2018-W19', '2018-W20', '2018-W21', '2018-W22', '2018-W23',
                              '2018-W24', '2018-W25', '2018-W26', '2018-W27', '2018-W28', '2018-W29', '2018-W30', '2018-W31',
                              '2018-W32', '2018-W33', '2018-W34', '2018-W35', '2018-W36', '2018-W37', '2018-W38', '2018-W39',
                              '2018-W40', '2018-W41', '2018-W42', '2018-W43', '2018-W44', '2018-W45', '2018-W46', '2018-W47',
                              '2018-W48', '2018-W49', '2018-W50', '2018-W51', '2018-W52', '2018-W53', '2019-W01', '2019-W02',
                              '2019-W03', '2019-W04', '2019-W05', '2019-W06')
    expect_identical(levels(results$cohort_week), expected_week_levels)
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

test_that("rt_equal_include_na", {

    expect_true(rt_equal_include_na(0, 0))
    expect_true(rt_equal_include_na(NA, NA))
    expect_true(rt_equal_include_na('a', 'a'))
    expect_true(all(rt_equal_include_na(x=c(1, NA, 'a'), y=c(1, NA, 'a'))))
    expect_true(all(rt_equal_include_na(x=c(NA, NA, NA), y=c(NA, NA, NA))))

    expect_error(rt_equal_include_na(NULL, 1))
    expect_error(rt_equal_include_na(1, NULL))
    expect_error(rt_equal_include_na(NULL, NULL))


    expect_false(rt_equal_include_na('a', 'b'))
    expect_false(rt_equal_include_na('a', 1))
    expect_false(rt_equal_include_na(0.00001, 0))
    expect_false(rt_equal_include_na(1, NA))
    expect_false(rt_equal_include_na(NA, 1))
    expect_identical(rt_equal_include_na(x=c(NA, NA, 'a'), y=c(1, NA, 'a')), c(FALSE, TRUE, TRUE))
    expect_identical(rt_equal_include_na(x=c(1, NA, 'a'), y=c(1, NA, NA)), c(TRUE, TRUE, FALSE))


    expect_identical(rt_equal_include_na(x=c(NA, 1, 'a'), y='a'), c(FALSE, FALSE, TRUE))
    expect_identical(rt_equal_include_na(x=c(NA, 1, 'a'), y=NA), c(TRUE, FALSE, FALSE))

    expect_identical(rt_equal_include_na(x=c(NA, 1, 'a'), y='a'), c(FALSE, FALSE, TRUE))
    expect_identical(rt_equal_include_na(x=c(NA, 1, 'a'), y=NA), c(TRUE, FALSE, FALSE))

    expect_identical(rt_equal_include_na(x='a', y=c(NA, 1, 'a')), c(FALSE, FALSE, TRUE))
    expect_identical(rt_equal_include_na(x=NA, y=c(NA, 1, 'a')), c(TRUE, FALSE, FALSE))

    # make sure I can use this as intended with e.g. dplyr filter
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    credit_history_good <- credit_data %>% filter(credit_history == 'good')

    check <- credit_data %>% filter(rt_equal_include_na(credit_history, 'good'))

    expect_true(rt_are_dataframes_equal(credit_history_good, check))

    check <- credit_data %>%
        mutate(credit_history = ifelse(credit_history == 'good', NA, credit_history)) %>%
        filter(rt_equal_include_na(credit_history, NA))
    expect_true(rt_are_dataframes_equal(credit_history_good %>% select(-credit_history),
                                        check %>% select(-credit_history)))
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

test_that("rt_colors", {

    all_colors <- rt_plot_colors()
    test_save_plot(file_name='data/rt_colors_all_colors.png',
                   plot=all_colors)
    test_save_plot(file_name='data/rt_colors_set_1.png',
                   plot=rt_plot_colors(set=1))
    test_save_plot(file_name='data/rt_colors_set_2.png',
                   plot=rt_plot_colors(set=2))
    test_save_plot(file_name='data/rt_colors_set_3.png',
                   plot=rt_plot_colors(set=3))
    test_save_plot(file_name='data/rt_colors_set_4.png',
                   plot=rt_plot_colors(set=4))
    test_save_plot(file_name='data/rt_colors_set_5.png',
                   plot=rt_plot_colors(set=5))
})

test_that("rt_colors_names", {

    custom_color_names <- c('tuplip_tree', 'custom_green', 'crail', 'flamingo', 'red_clay', 'granite')
    test_save_plot(file_name='data/rt_colors_names.png',
                   plot=rt_plot_colors(color_names = custom_color_names))
})

test_that("rt_colors_good_bad", {

    create_color_df <- function(custom_colors, rev_factor_names=FALSE) {
        factor_names <- names(custom_colors)
        if(rev_factor_names) {
            factor_names <- rev(names(custom_colors))
        }
        data.frame(name=names(custom_colors),
                   hex=custom_colors,
                   value=1,
                   stringsAsFactors = FALSE) %>%
            mutate(name = factor(name, levels=factor_names))
    }
    custom_colors <- rt_colors_good_bad(good_first = TRUE)
    custom_color_names <- c("good", "bad")
    names(custom_colors) <- custom_color_names
    rt_stopif(any(duplicated(custom_colors)))
    rt_stopif(any(duplicated(names(custom_colors))))
    expect_identical(custom_color_names, names(custom_colors))
    colors_df <- create_color_df(custom_colors)
    all_colors <- colors_df %>%
        ggplot(aes(x=name, y=value, fill=name)) +
        geom_col() +
        scale_fill_manual(values=custom_colors) +
        theme(legend.position = 'none')
    test_save_plot(file_name='data/rt_colors_good_bad.png',
                   plot=all_colors)

    custom_colors <- rt_colors_good_bad(good_first = FALSE)
    custom_color_names <- c("bad", "good")
    names(custom_colors) <- custom_color_names
    rt_stopif(any(duplicated(custom_colors)))
    rt_stopif(any(duplicated(names(custom_colors))))
    expect_identical(custom_color_names, names(custom_colors))
    colors_df <- create_color_df(custom_colors)
    all_colors <- colors_df %>%
        ggplot(aes(x=name, y=value, fill=name)) +
        geom_col() +
        scale_fill_manual(values=custom_colors) +
        theme(legend.position = 'none')
    test_save_plot(file_name='data/rt_colors_bad_good.png',
                   plot=all_colors)
})

test_that("rt_difftime_numeric", {

    first_date <- ymd_hms("2019-01-01 00:00:00")
    second_date <- ymd_hms("2019-01-02 00:00:13")

    expect_error(rt_difftime_numeric(date_last=second_date, date_first=first_date, units='auto'))
    expect_error(rt_difftime_numeric(date_last=second_date, date_first=first_date, units=c('secs', 'mins')))

    test_units <- c("secs")
    expect_equal(rt_difftime_numeric(second_date, first_date, units=test_units), as.numeric(difftime(second_date, first_date, units=test_units)))
    test_units <- c("mins")
    expect_equal(rt_difftime_numeric(second_date, first_date, units=test_units), as.numeric(difftime(second_date, first_date, units=test_units)))
    test_units <- c("hours")
    expect_equal(rt_difftime_numeric(second_date, first_date, units=test_units), as.numeric(difftime(second_date, first_date, units=test_units)))
    test_units <- c("days")
    expect_equal(rt_difftime_numeric(second_date, first_date, units=test_units), as.numeric(difftime(second_date, first_date, units=test_units)))
    test_units <- c("weeks")
    expect_equal(rt_difftime_numeric(second_date, first_date, units=test_units), as.numeric(difftime(second_date, first_date, units=test_units)))

    test_units <- c("secs")
    expect_equal(rt_difftime_numeric(first_date, second_date, units=test_units), as.numeric(difftime(first_date, second_date, units=test_units)))
    test_units <- c("mins")
    expect_equal(rt_difftime_numeric(first_date, second_date, units=test_units), as.numeric(difftime(first_date, second_date, units=test_units)))
    test_units <- c("hours")
    expect_equal(rt_difftime_numeric(first_date, second_date, units=test_units), as.numeric(difftime(first_date, second_date, units=test_units)))
    test_units <- c("days")
    expect_equal(rt_difftime_numeric(first_date, second_date, units=test_units), as.numeric(difftime(first_date, second_date, units=test_units)))
    test_units <- c("weeks")
    expect_equal(rt_difftime_numeric(first_date, second_date, units=test_units), as.numeric(difftime(first_date, second_date, units=test_units)))
})

test_that("rt_floor_date_factor", {

    start_date <- ymd_hms("2019-01-01 23:59:59")
    all_dates <- start_date + days(0:366)

    week_vector <- rt_floor_date_factor(date_vector=all_dates, date_floor='week')
    expect_true(is.factor(week_vector))
    expect_true(is.ordered(week_vector))
    expect_true(rt_are_dataframes_equal(table(week_vector) %>% as.data.frame(),
                                        read.csv(file = 'data/rt_floor_date_factor__week.csv') %>%
                                            select(-X)))

    month_vector <- rt_floor_date_factor(date_vector=all_dates, date_floor='month')
    expect_true(is.factor(month_vector))
    expect_true(is.ordered(month_vector))
    expect_true(rt_are_dataframes_equal(table(month_vector) %>% as.data.frame(),
                                        read.csv(file = 'data/rt_floor_date_factor__month.csv') %>%
                                            select(-X)))

    quarter_vector <- rt_floor_date_factor(date_vector=all_dates, date_floor='quarter')
    expect_true(is.factor(quarter_vector))
    expect_true(is.ordered(quarter_vector))
    expect_true(rt_are_dataframes_equal(table(quarter_vector) %>% as.data.frame(),
                                        read.csv(file = 'data/rt_floor_date_factor__quarter.csv') %>%
                                            select(-X)))

    year_vector <- rt_floor_date_factor(date_vector=all_dates, date_floor='year')
    expect_true(is.factor(year_vector))
    expect_true(is.ordered(year_vector))
    freq_df <- table(year_vector) %>% as.data.frame()
    expect_identical(as.character(freq_df$year_vector), c('2019', '2020'))
    expect_identical(levels(freq_df$year_vector), c('2019', '2020'))
    expect_true(all(freq_df$Freq == c(365, 2)))
})

test_that("rt_are_numerics_equal", {

    expect_true(rt_are_numerics_equal(n1=1, n2=1, num_decimals=10))
    expect_true(rt_are_numerics_equal(n1=1, n2=1.00000000001, num_decimals=10))
    expect_true(rt_are_numerics_equal(n1=1.00000000001, n2=1, num_decimals=10))
    expect_true(rt_are_numerics_equal(n1=1.00000000001, n2=1.00000000001, num_decimals=10))
    expect_true(rt_are_numerics_equal(n1=c(1, 2, 3), n2=c(1, 2, 3), num_decimals=10))
    expect_true(rt_are_numerics_equal(n1=c(1.00049, 1, 1), n2=1, num_decimals=3))
    expect_true(rt_are_numerics_equal(n1=c(1.00049, 1, 1), n2=1.00049, num_decimals=3))
    expect_true(rt_are_numerics_equal(n1=c(0.9999, 2, 3), n2=c(1.0001, 2.0001, 3.0001), num_decimals=3))

    expect_false(rt_are_numerics_equal(n1=1.00000000001, n2=1, num_decimals=11))
    expect_false(rt_are_numerics_equal(n1=1, n2=1.0000000001, num_decimals=10))
    expect_false(rt_are_numerics_equal(n1=1.00000000001, n2=1, num_decimals=11))
    expect_false(rt_are_numerics_equal(n1=1.000000000019, n2=1.00000000001, num_decimals=11))
    expect_false(rt_are_numerics_equal(n1=c(1.00049, 1, 1), n2=1, num_decimals=4))
    expect_false(rt_are_numerics_equal(n1=c(1.00049, 1, 1), n2=1.00049, num_decimals=4))
    expect_false(rt_are_numerics_equal(n1=c(0.9999, 2, 3), n2=c(1.0001, 2.0001, 3.0001), num_decimals=4))
})

test_that("rt_transform_multi_value_df", {
    credit_data <- read.csv("data/credit.csv", header=TRUE)
    ##########################################################################################################
    # test with factor
    # change the levels to verify that the original levels are retained if order_by_count==FALSE
    ##########################################################################################################
    custom_levels <- c('< 0 DM', '1 - 200 DM', '> 200 DM', 'unknown')
    credit_data$checking_balance <- factor(credit_data$checking_balance, levels=custom_levels)

    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA
    credit_data[2, 'purpose'] <- NA

    # many variables
    variable <- 'purpose'
    transformed_df <- rt_transform_multi_value_df(dataset=credit_data,
                                                  variable=variable,
                                                  multi_value_delimiter=', ')
    expect_true(rt_are_dataframes_equal(credit_data, transformed_df))

    # only 1 variable
    transformed_df <- rt_transform_multi_value_df(dataset=credit_data %>% select(checking_balance),
                                                  variable='checking_balance',
                                                  multi_value_delimiter=', ')
    expect_true(rt_are_dataframes_equal(credit_data %>% select(checking_balance), transformed_df))

    multi_value_credit_data <- credit_data %>%
        mutate(purpose = case_when(
            purpose == 'car' ~ 'car, car_test',
            purpose == 'business' ~ 'business, business_test',
            TRUE ~ as.character(purpose))) %>%
        mutate(purpose = as.factor(purpose))

    variable <- 'purpose'
    transformed_df <- rt_transform_multi_value_df(dataset=multi_value_credit_data,
                                                  variable=variable,
                                                  multi_value_delimiter=', ')
    expect_identical(colnames(multi_value_credit_data), colnames(transformed_df))
    # when removing the _test values from purpose, the dataset should be the same as the original dataset
    expect_true(rt_are_dataframes_equal(credit_data,
                                        transformed_df %>% filter(!purpose %in% c('car_test', 'business_test'))))

    test_df <- transformed_df %>% filter(purpose %in% c('car_test', 'business_test'))
    original_df <- transformed_df %>% filter(purpose %in% c('car', 'business'))

    # asside from purpose, the values of the other columns should not be changed
    expect_true(rt_are_dataframes_equal(test_df %>% select(-purpose), original_df %>% select(-purpose)))
    # the count of the original & _test values should be the same
    expect_true(all(test_df %>% count(purpose) %>% pull(n) == original_df %>% count(purpose) %>% pull(n)))


    # only 1 column
    transformed_df <- rt_transform_multi_value_df(dataset=multi_value_credit_data %>% select(purpose),
                                                  variable=variable,
                                                  multi_value_delimiter=', ')
    expect_identical(colnames(transformed_df), 'purpose')
    # when removing the _test values from purpose, the dataset should be the same as the original dataset
    expect_true(rt_are_dataframes_equal(credit_data %>% select(purpose),
                                        transformed_df %>% filter(!purpose %in% c('car_test', 'business_test'))))

    test_df <- transformed_df %>% filter(purpose %in% c('car_test', 'business_test'))
    original_df <- transformed_df %>% filter(purpose %in% c('car', 'business'))
    # the count of the original & _test values should be the same
    expect_true(all(test_df %>% count(purpose) %>% pull(n) == original_df %>% count(purpose) %>% pull(n)))
})

test_that("rt_select_all_of", {
    expected <- c('a', 'b', 'c', 'd', 'e')
    x_a <- 'a'
    x_b <- c('b')
    x_cd <- c('c', 'd')
    x_e <- 'e'
    expect_identical(expected, rt_params_to_vector(expected))
    expect_identical(expected, rt_params_to_vector('a', c('b'), c('c', 'd'), 'e'))
    expect_identical(expected, rt_params_to_vector(x_a, x_b, x_cd, x_e))
    expect_identical(expected, rt_params_to_vector(x_a, c(x_b, x_cd), x_e))

    iris_columns <- colnames(iris)
    expect_true(rt_are_dataframes_equal(iris, iris %>% rt_select_all_of(iris_columns)))
    expect_true(rt_are_dataframes_equal(iris, iris %>% rt_select_all_of(iris_columns[1], iris_columns[2:5])))
    expect_true(rt_are_dataframes_equal(iris %>% select(Sepal.Length, Sepal.Width),
                                        iris %>% rt_select_all_of(iris_columns[1:2])))
    expect_true(rt_are_dataframes_equal(iris %>% select(Sepal.Length, Sepal.Width),
                                        iris %>% rt_select_all_of("Sepal.Length", "Sepal.Width")))

    colnames(iris) <- str_replace_all(string=colnames(iris), pattern = '\\.', replacement = ' ')
    iris_columns <- colnames(iris)
    expect_true(rt_are_dataframes_equal(iris, iris %>% rt_select_all_of(iris_columns)))
    expect_true(rt_are_dataframes_equal(iris, iris %>% rt_select_all_of(iris_columns[1], iris_columns[2:5])))
    expect_true(rt_are_dataframes_equal(iris %>% select(`Sepal Length`, `Sepal Width`),
                                        iris %>% rt_select_all_of(iris_columns[1:2])))
    expect_true(rt_are_dataframes_equal(iris %>% select(`Sepal Length`, `Sepal Width`),
                                        iris %>% rt_select_all_of("Sepal Length", "Sepal Width")))
})

test_that("rt_group_by_all_of", {

    dataset <- read.csv("data/credit.csv", header=TRUE)
    colnames(dataset) <- paste(rt_pretty_text(colnames(dataset)), 'Column')

    expected <- dataset %>% group_by(`Default Column`, `Phone Column`) %>% summarise(n=n())
    actual <- dataset %>% rt_group_by_all_of('Default Column', 'Phone Column') %>% summarise(n=n())
    expect_true(rt_are_dataframes_equal(expected, actual))

    actual <- dataset %>% rt_group_by_all_of(c('Default Column', 'Phone Column')) %>% summarise(n=n())
    expect_true(rt_are_dataframes_equal(expected, actual))

    expected <- dataset %>% group_by(`Default Column`) %>% summarise(n=n())
    actual <- dataset %>% rt_group_by_all_of('Default Column') %>% summarise(n=n())
    expect_true(rt_are_dataframes_equal(expected, actual))
})

test_that("rt_get_year_month_factors", {

    date_vector <- ymd('2020-01-01') + months(0:23)

    factor_list <- rt_get_year_month_factors(date_vector, .abbreviate = FALSE)
    expect_identical(levels(factor_list$year_factor), c('2020', '2021'))
    expect_identical(as.character(factor_list$year_factor), c(rep('2020', 12), rep('2021', 12)))
    expect_identical(levels(factor_list$month_factor), month.name)
    expect_identical(as.character(factor_list$month_factor), rep(month.name, 2))

    factor_list <- rt_get_year_month_factors(date_vector, .abbreviate = TRUE)
    expect_identical(levels(factor_list$year_factor), c('2020', '2021'))
    expect_identical(as.character(factor_list$year_factor), c(rep('2020', 12), rep('2021', 12)))
    expect_identical(levels(factor_list$month_factor), month.abb)
    expect_identical(as.character(factor_list$month_factor), rep(month.abb, 2))

})

test_that("rt_add_year_month_factors", {

    date_vector <- ymd('2020-01-01') + months(0:23)
    date_df <- data.frame(date_column=date_vector)
    date_df <- date_df %>% rt_add_year_month_factors(date_column, .abbreviate = FALSE)
    expect_identical(levels(date_df$date_column_year), c('2020', '2021'))
    expect_identical(as.character(date_df$date_column_year), c(rep('2020', 12), rep('2021', 12)))
    expect_identical(levels(date_df$date_column_month), month.name)
    expect_identical(as.character(date_df$date_column_month), rep(month.name, 2))

    date_vector <- ymd('2020-01-01') + months(0:23)
    date_df <- data.frame(date_column=date_vector)
    date_df <- date_df %>% rt_add_year_month_factors(date_column, .abbreviate = TRUE)
    expect_identical(levels(date_df$date_column_year), c('2020', '2021'))
    expect_identical(as.character(date_df$date_column_year), c(rep('2020', 12), rep('2021', 12)))
    expect_identical(levels(date_df$date_column_month), month.abb)
    expect_identical(as.character(date_df$date_column_month), rep(month.abb, 2))
})
