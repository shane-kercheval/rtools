context('General')
library(testthat)
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
    custom_colors <- rt_colors(return_named_vector=TRUE)
    rt_stopif(any(duplicated(custom_colors)))
    rt_stopif(any(duplicated(names(custom_colors))))
    colors_df <- create_color_df(custom_colors, rev_factor_names=TRUE)
    all_colors <- colors_df %>%
        ggplot(aes(x=name, y=value, fill=name)) +
        geom_col() +
        scale_fill_manual(values=rev(custom_colors)) +
        theme(legend.position = 'none') +
        coord_flip()
    test_save_plot(file_name='data/rt_colors_all_colors.png',
                   plot=all_colors)


    plot_set <- function(set) {

        custom_colors <- rt_colors(sets=set, return_named_vector=TRUE)
        rt_stopif(any(duplicated(custom_colors)))
        rt_stopif(any(duplicated(names(custom_colors))))
        colors_df <- create_color_df(custom_colors)
        colors_df %>%
            ggplot(aes(x=name, y=value, fill=name)) +
            geom_col() +
            scale_fill_manual(values=custom_colors) +
            theme(legend.position = 'none')
    }
    test_save_plot(file_name='data/rt_colors_set_1.png',
                   plot=plot_set(1))
    test_save_plot(file_name='data/rt_colors_set_2.png',
                   plot=plot_set(2))
    test_save_plot(file_name='data/rt_colors_set_3.png',
                   plot=plot_set(3))
    test_save_plot(file_name='data/rt_colors_set_4.png',
                   plot=plot_set(4))
    test_save_plot(file_name='data/rt_colors_set_5.png',
                   plot=plot_set(5))
})

test_that("rt_colors_names", {

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
    custom_color_names <- c('tuplip_tree', 'custom_green', 'crail', 'flamingo', 'red_clay', 'granite')
    custom_colors <- rt_colors(color_names=custom_color_names,
                               return_named_vector=TRUE)
    rt_stopif(any(duplicated(custom_colors)))
    rt_stopif(any(duplicated(names(custom_colors))))
    expect_identical(custom_color_names, names(custom_colors))
    colors_df <- create_color_df(custom_colors, rev_factor_names=TRUE)
    all_colors <- colors_df %>%
        ggplot(aes(x=name, y=value, fill=name)) +
        geom_col() +
        scale_fill_manual(values=rev(custom_colors)) +
        theme(legend.position = 'none') +
        coord_flip()
    test_save_plot(file_name='data/rt_colors_names.png',
                   plot=all_colors)
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
