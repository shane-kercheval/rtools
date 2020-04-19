context('Attribution')
library(testthat)
library(dplyr)
library(ggplot2)
library(scales)
source('test_helpers.R')

test_that("rt__mock__attribution_to_clickstream", {

    ####
    # Even though this is mock data we should still test that it is transformed as expected since
    # other tests relay on the validity of the mock data
    ####

    campaign_data <- readRDS('data/campaign_data__small.RDS')
    clickstream_data <- rt__mock__attribution_to_clickstream(campaign_data)

    expect_equal(sum(campaign_data$num_conversions), sum(clickstream_data$num_conversions))
    expect_equal(sum(campaign_data$conversion_value), sum(clickstream_data$conversion_value))

    ####
    # make sure conversion events in clickstream_data match expected values
    ####
    expected_df <- campaign_data %>%
        filter(num_conversions > 0) %>%
        select(id, timestamp, num_conversions, conversion_value) %>%
        # the timestamp will either be the same 1 or second after as the corresponding step (won't work if
        # of the timestamps that is incremenented by a second is midnight)
        mutate(timestamp=floor_date(timestamp, unit = 'day'))

    actual_df <- clickstream_data %>%
        filter(num_conversions > 0) %>%
        select(id, timestamp, num_conversions, conversion_value) %>%
        # the timestamp will either be the same 1 or second after as the corresponding step (won't work if
        # of the timestamps that is incremenented by a second is midnight)
        mutate(timestamp=floor_date(timestamp, unit = 'day'))

    expect_true(rt_are_dataframes_equal(expected_df, actual_df))

    ####
    # make sure non-conversion events in clickstream_data match expected values
    ####
    expected_df <- campaign_data %>%
        # the clickstream data should have all of the original events, but non of them will have
        # num_conversions or conversion_value values
        mutate(num_conversions = 0,
               conversion_value = 0)

    actual_df <- clickstream_data %>%
        filter(num_conversions == 0)

    expect_true(rt_are_dataframes_equal(expected_df, actual_df))

})

test_that("rt_clickstream_to_attribution", {

    campaign_data_original <- readRDS('data/campaign_data__small.RDS') %>%
        arrange(id, timestamp, step)
    clickstream_data <- rt__mock__attribution_to_clickstream(campaign_data_original)
    campaign_data_new <- rt_clickstream_to_attribution(clickstream_data) %>%
        arrange(id, timestamp, step)
    expect_true(rt_are_dataframes_equal(campaign_data_original %>% arrange(id, timestamp, step),
                                        campaign_data_new %>% arrange(id, timestamp, step)))
})

test_that("rt_campaign_add_columns", {

    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions()

    set.seed(42)
    new_indexes <- sample(nrow(campaign_data), replace = FALSE)

    ######
    # .use_first_conversion=TRUE
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data[new_indexes, ],
                                                         .use_first_conversion=TRUE,
                                                         .sort=TRUE)

    expected_df <- test_helper__campaign_filter_first_conversions(campaign_data)

    expect_true(rt_are_dataframes_equal(expected_df,
                                        campaign_data_transformed %>%
                                            select(-.path_id) %>%
                                            arrange(id, timestamp, conversion_value, step)))
    expect_identical(campaign_data_transformed$.path_id, campaign_data_transformed$id)
    expect_identical(campaign_data_transformed$.path_id, expected_df$id)

    ######
    # .use_first_conversion=FALSE
    # .reset_upon_conversion=TRUE
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data[new_indexes, ],
                                                         .use_first_conversion=FALSE,
                                                         .sort=TRUE)

    expect_true(rt_are_dataframes_equal(campaign_data %>%
                                            arrange(id, timestamp, conversion_value, step),
                                        campaign_data_transformed %>%
                                            select(-.path_id) %>%
                                            arrange(id, timestamp, conversion_value, step)))
#     campaign_data %>% rt_peak()
#     campaign_data_transformed %>% rt_peak()

    # test that the expected number of paths based on number of conversions
    # if there are no additional steps after the last conversion, the number of paths should equal the number
    # of conversions
    # if there are additional steps after the last conversion then the number of paths will be 1 greater than
    # the number of conversions
    campaign_data_summary <- suppressWarnings(campaign_data_transformed %>%
        mutate(conversion_timestamp = ifelse(num_conversions > 0, timestamp, NA)) %>%
        group_by(id) %>%
        mutate(step_index = row_number(timestamp),
               conversion_index = row_number(conversion_timestamp),
               max_conversion_index = max(conversion_index, na.rm = TRUE),
               step_index_of_max_conversion_index = max(step_index[conversion_index == max_conversion_index], na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(id) %>%
        summarise(total_conversions = sum(num_conversions > 0),
                  num_path_ids = n_distinct(.path_id),
                  max_conversion_index = max(conversion_index, na.rm = TRUE),
                  max_index_equal_conversion_index = max(step_index) == max(step_index_of_max_conversion_index, na.rm = TRUE),
                  conversion_is_last_step = total_conversions > 0 & max(step_index) == max(step_index_of_max_conversion_index, na.rm = TRUE)
                  ))
    # test that the expected number of paths based on number of conversions
    expect_true(all(with(campaign_data_summary, ifelse(conversion_is_last_step, total_conversions == num_path_ids, total_conversions == num_path_ids - 1))))
})

test_that("rt_campaign_to_markov_paths", {

    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions()



    ######
    # .use_first_conversion=TRUE
    # .separate_paths_ids=TRUE
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data,
                                                         .use_first_conversion=TRUE,
                                                         .sort=TRUE)

    campaign_data_paths <- rt_campaign_to_markov_paths(campaign_data_transformed,
                                                       .separate_paths_ids=TRUE)

    # get the dataset that contains paths that either don't have a conversion or only includes up to first conversion
    campaign_data__first_conversions <- test_helper__campaign_filter_first_conversions(campaign_data)

    campaign_data__first_conversions__last_step <- campaign_data__first_conversions %>%
        arrange(id, timestamp, num_conversions, step) %>%
        group_by(id) %>%
        # because there might be multiple events with the same timestamp; we don't care for this check
        filter(row_number(timestamp) == max(row_number(timestamp))) %>%
        ungroup()

    # since we only keep the first event, the last event in campaign_data__first_conversions should have
    # the same number of conversions that the end result of campaign_data_paths
    expect_identical(campaign_data_paths$num_conversions, campaign_data__first_conversions__last_step$num_conversions)
    expect_identical(campaign_data_paths$conversion_value, campaign_data__first_conversions__last_step$conversion_value)
    expect_identical(campaign_data_paths$null_conversions, ifelse(campaign_data__first_conversions__last_step$num_conversions > 0, 0, 1))

    # the number of times a step appears in the path sequence should match the number of times the event
    # appears in the (first time) campaign data
    path_split <- str_split(campaign_data_paths$path_sequence, pattern = ' > ', simplify =  TRUE)
    step_counts_found <- table(path_split)
    step_counts_found <- step_counts_found[-1]
    step_counts_expected <- campaign_data__first_conversions %>% count(step)
    expect_identical(names(step_counts_found), step_counts_expected$step)
    expect_equal(as.numeric(step_counts_found), step_counts_expected$n)

    num_steps <- map_int(campaign_data_paths$path_sequence, ~ length(str_split(., pattern = ' > ', simplify =  FALSE)[[1]]))
    expect_equal(num_steps, campaign_data_transformed %>% count(.path_id) %>% pull(n))

    ######
    # .use_first_conversion=TRUE
    # .separate_paths_ids=FALSE
    # this should be the same as .separate_paths_ids=TRUE since we are only using the first conversion event
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data,
                                                         .use_first_conversion=TRUE,
                                                         .sort=TRUE)

    campaign_data_paths_2 <- rt_campaign_to_markov_paths(campaign_data_transformed,
                                                       .separate_paths_ids=FALSE)

    expect_true(rt_are_dataframes_equal(campaign_data_paths, campaign_data_paths_2))

    ######
    # .use_first_conversion=FALSE
    # .separate_paths_ids=TRUE
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data,
                                                         .use_first_conversion=FALSE,
                                                         .sort=TRUE)

    campaign_data_paths <- rt_campaign_to_markov_paths(campaign_data_transformed,
                                                       # each path id gets its own row & path_sequence
                                                       # each person will be represented >= 1 times, but each path-id will be different
                                                        .separate_paths_ids=TRUE)

    # make sure we didn't lose any conversions or conversion values
    per_id_counts_found <- campaign_data_paths %>%
        separate(col=path_id, into = c('id', 'path_num')) %>%
        group_by(id) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value))

    per_id_counts_expected <- campaign_data %>%
        group_by(id) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value))

    expect_true(rt_are_dataframes_equal(per_id_counts_expected, per_id_counts_found))

    # since we only keep the first event, the last event in campaign_data__first_conversions should have
    # the same number of conversions that the end result of campaign_data_paths
    expect_equal(sum(campaign_data_paths$num_conversions), sum(campaign_data$num_conversions))
    expect_equal(sum(campaign_data_paths$conversion_value), sum(campaign_data$conversion_value))
    expect_identical(campaign_data_paths$null_conversions, ifelse(campaign_data_paths$num_conversions > 0, 0, 1))

    # the number of times a step appears in the path sequence should match the number of times the event
    # appears in the (first time) campaign data
    path_split <- str_split(campaign_data_paths$path_sequence, pattern = ' > ', simplify =  TRUE)
    step_counts_found <- table(path_split)
    step_counts_found <- step_counts_found[-1]
    step_counts_expected <- campaign_data %>% count(step)
    expect_identical(names(step_counts_found), step_counts_expected$step)
    expect_equal(as.numeric(step_counts_found), step_counts_expected$n)

    num_steps <- map_int(campaign_data_paths$path_sequence, ~ length(str_split(., pattern = ' > ', simplify =  FALSE)[[1]]))
    expect_equal(num_steps, campaign_data_transformed %>% count(.path_id) %>% pull(n))

    ######
    # .use_first_conversion=FALSE
    # .reset_upon_conversion=FALSE
    # uses all of the path data
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data,
                                                         .use_first_conversion=FALSE,
                                                         .sort=TRUE)

    campaign_data_paths <- rt_campaign_to_markov_paths(campaign_data_transformed,
                                                       # each person will only be counted once, with their entire path,
                                                       # cumulative conversions/conversion-value/null-conversions
                                                       .separate_paths_ids=FALSE)


    # make sure we didn't lose any conversions or conversion values
    per_id_counts_found <- campaign_data_paths %>%
        group_by(path_id) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value))

    per_id_counts_expected <- campaign_data %>%
        group_by(id) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value)) %>%
        rename(path_id = id)

    expect_true(rt_are_dataframes_equal(per_id_counts_expected, per_id_counts_found))

    expect_true(rt_are_dataframes_equal(per_id_counts_expected,
                                        campaign_data_paths %>%
                                            select(path_id, num_conversions, conversion_value)))

    # since we only keep the first event, the last event in campaign_data__first_conversions should have
    # the same number of conversions that the end result of campaign_data_paths
    expect_equal(sum(campaign_data_paths$num_conversions), sum(campaign_data$num_conversions))
    expect_equal(sum(campaign_data_paths$conversion_value), sum(campaign_data$conversion_value))

    # now, null_conversions should be the number of paths that don't lead to a conversion
    # this will either occur A) if there are no conversions, or if there aren't any steps after the last conversion
    campaign_data_summary <- suppressWarnings(campaign_data_transformed %>%
                                                  mutate(conversion_timestamp = ifelse(num_conversions > 0, timestamp, NA)) %>%
                                                  group_by(id) %>%
                                                  mutate(step_index = row_number(timestamp),
                                                         conversion_index = row_number(conversion_timestamp),
                                                         max_conversion_index = max(conversion_index, na.rm = TRUE),
                                                         step_index_of_max_conversion_index = max(step_index[conversion_index == max_conversion_index], na.rm = TRUE)) %>%
                                                  ungroup() %>%
                                                  group_by(id) %>%
                                                  summarise(total_conversions = sum(num_conversions > 0),
                                                            num_path_ids = n_distinct(.path_id),
                                                            max_conversion_index = max(conversion_index, na.rm = TRUE),
                                                            max_index_equal_conversion_index = max(step_index) == max(step_index_of_max_conversion_index, na.rm = TRUE),
                                                            conversion_is_last_step = total_conversions > 0 & max(step_index) == max(step_index_of_max_conversion_index, na.rm = TRUE)
                                                  ))

    expect_equal(campaign_data_paths$null_conversions,
                     ifelse(campaign_data_paths$num_conversions == 0 | !campaign_data_summary$conversion_is_last_step,
                            1, 0))

    # the number of times a step appears in the path sequence should match the number of times the event
    # appears in the (first time) campaign data
    path_split <- str_split(campaign_data_paths$path_sequence, pattern = ' > ', simplify =  TRUE)
    step_counts_found <- table(path_split)
    step_counts_found <- step_counts_found[-1]
    step_counts_expected <- campaign_data %>% count(step)
    expect_identical(names(step_counts_found), step_counts_expected$step)
    expect_equal(as.numeric(step_counts_found), step_counts_expected$n)

    num_steps <- map_int(campaign_data_paths$path_sequence, ~ length(str_split(., pattern = ' > ', simplify =  FALSE)[[1]]))
    expect_equal(num_steps, campaign_data_transformed %>% count(id) %>% pull(n))
})

test_that("rt_markov_model", {
    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions()

    steps <- campaign_data %>%
        select(step, step_type) %>%
        distinct()

    channel_categories <- steps$step_type
    names(channel_categories) <- steps$step

    ########
    # first conversion: TRUE
    # separate path_ids
    ########
    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)
    markov_model_results <- rt_markov_model(campaign_paths)

    # don't need to test separate_paths_ids=FALSE with .use_first_conversion=TRUE because it should be the
    # same as .separate_paths_ids=TRUE
    campaign_paths_2 <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=FALSE)
    expect_true(rt_are_dataframes_equal(campaign_paths, campaign_paths_2))

    # the number and value of conversions should be the same in the campaign data and markov results
    campaign_data_first_conversions <- campaign_data %>% test_helper__campaign_filter_first_conversions()
    expect_equal(sum(campaign_data_first_conversions$num_conversions), sum(markov_model_results$result$total_conversions))
    expect_equal(sum(campaign_data_first_conversions$conversion_value), sum(markov_model_results$result$total_conversion_value))

    test_save_plot(file_name='data/rt_plot_markov_removal_effects__first_conversion.png',
                   plot=rt_plot_markov_removal_effects(markov_model_results))

    test_save_plot(file_name='data/rt_plot_markov_removal_effects__first_conversion__categories.png',
                   plot=rt_plot_markov_removal_effects(markov_model_results,
                                                       .channel_categories = channel_categories))
    ########
    # first conversion: FALSE
    # separate path_ids: TRUE
    ########
    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=FALSE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)
    markov_model_results <- rt_markov_model(campaign_paths)

    # the number and value of conversions should be the same in the campaign data and markov results
    expect_equal(sum(campaign_data$num_conversions), sum(markov_model_results$result$total_conversions))
    expect_equal(sum(campaign_data$conversion_value), sum(markov_model_results$result$total_conversion_value))

    test_save_plot(file_name='data/rt_plot_markov_removal_effects__all_conversions__sep_path.png',
                   plot=rt_plot_markov_removal_effects(markov_model_results))

    test_save_plot(file_name='data/rt_plot_markov_removal_effects__all_conversions__sep_path__categories.png',
                   plot=rt_plot_markov_removal_effects(markov_model_results,
                                                       .channel_categories = channel_categories))
    ########
    # first conversion: FALSE
    # separate path_ids: FALSE
    ########
    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=FALSE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=FALSE)
    markov_model_results <- rt_markov_model(campaign_paths)

    # the number and value of conversions should be the same in the campaign data and markov results
    expect_equal(sum(campaign_data$num_conversions), sum(markov_model_results$result$total_conversions))
    expect_equal(sum(campaign_data$conversion_value), sum(markov_model_results$result$total_conversion_value))

    test_save_plot(file_name='data/rt_plot_markov_removal_effects__all_conversions__all_path.png',
                   plot=rt_plot_markov_removal_effects(markov_model_results))

    test_save_plot(file_name='data/rt_plot_markov_removal_effects__all_conversions__all_path__categories.png',
                   plot=rt_plot_markov_removal_effects(markov_model_results,
                                                       .channel_categories = channel_categories))
})

test_that("rt_get_channel_attribution", {
    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions()

    ########
    # first conversion: TRUE
    # separate path_ids
    ########
    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)

    channel_attribution <- rt_get_channel_attribution(campaign_paths)
    channel_attribution_2 <- rt_get_channel_attribution(campaign_paths, .conversion_value = NULL)
    expect_true(rt_are_dataframes_equal(channel_attribution %>%
                                            # for some reason, including .conversion_value slightly changes results for markov
                                            filter(attribution_type == 'Conversion' & attribution_name != 'Markov'),
                                        channel_attribution_2 %>%
                                            # for some reason, including .conversion_value slightly changes results for markov
                                            filter(attribution_name != 'Markov')))

    found_conversions <- channel_attribution %>%
        filter(attribution_type == 'Conversion') %>%
        pivot_wider(names_from = 'attribution_name',
                    values_from = 'attribution_value') %>%
        select_if(is.numeric) %>%
        colSums() %>%
        unique()
    expect_equal(length(found_conversions), 1)
    expect_equal(found_conversions, sum(campaign_paths$num_conversions))

    found_conversions <- channel_attribution %>%
        filter(attribution_type == 'Conversion Value') %>%
        pivot_wider(names_from = 'attribution_name',
                    values_from = 'attribution_value') %>%
        select_if(is.numeric) %>%
        colSums() %>%
        unique()
    expect_equal(length(found_conversions), 1)
    expect_equal(found_conversions, sum(campaign_paths$conversion_value))

    expected_campaign_conversions <- campaign_data %>%
        test_helper__campaign_filter_first_conversions() %>%
        group_by(id) %>%
        mutate(visit_index = row_number(timestamp)) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value),
                  first_step = step[visit_index == 1],
                  last_step = step[visit_index == max(visit_index)]
                  ) %>%
        filter(num_conversions > 0)

    expected_first_touch_conversions <- expected_campaign_conversions %>%
        group_by(first_step) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value)) %>%
        ungroup() %>%
        rename(channel_name = first_step)

    expected_last_touch_conversions <- expected_campaign_conversions %>%
        group_by(last_step) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value)) %>%
        ungroup() %>%
        rename(channel_name = last_step)

    expect_equal(expected_first_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(num_conversions),
                 channel_attribution %>%
                     filter(attribution_name == 'First Touch' & attribution_type == 'Conversion') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_first_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(conversion_value),
                 channel_attribution %>%
                     filter(attribution_name == 'First Touch' & attribution_type == 'Conversion Value') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_last_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(num_conversions),
                 channel_attribution %>%
                     filter(attribution_name == 'Last Touch' & attribution_type == 'Conversion') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_last_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(conversion_value),
                 channel_attribution %>%
                     filter(attribution_name == 'Last Touch' & attribution_type == 'Conversion Value') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    ########
    # first conversion: FALSE
    # separate path_ids: FALSE
    ########
    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=FALSE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=FALSE)

    channel_attribution <- rt_get_channel_attribution(campaign_paths)
    channel_attribution_2 <- rt_get_channel_attribution(campaign_paths, .conversion_value = NULL)
    expect_true(rt_are_dataframes_equal(channel_attribution %>%
                                            # for some reason, including .conversion_value slightly changes results for markov
                                            filter(attribution_type == 'Conversion' & attribution_name != 'Markov'),
                                        channel_attribution_2 %>%
                                            # for some reason, including .conversion_value slightly changes results for markov
                                            filter(attribution_name != 'Markov')))

    found_conversions <- channel_attribution %>%
        filter(attribution_type == 'Conversion') %>%
        pivot_wider(names_from = 'attribution_name',
                    values_from = 'attribution_value') %>%
        select_if(is.numeric) %>%
        colSums() %>%
        unique()
    expect_equal(length(found_conversions), 1)
    expect_equal(found_conversions, sum(campaign_paths$num_conversions))

    found_conversions <- channel_attribution %>%
        filter(attribution_type == 'Conversion Value') %>%
        pivot_wider(names_from = 'attribution_name',
                    values_from = 'attribution_value') %>%
        select_if(is.numeric) %>%
        colSums() %>%
        round(6) %>%
        unique()

    expect_equal(length(found_conversions), 1)
    expect_equal(found_conversions, sum(campaign_paths$conversion_value))

    expected_campaign_conversions <- campaign_data %>%
        group_by(id) %>%
        mutate(visit_index = row_number(timestamp)) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value),
                  first_step = step[visit_index == 1],
                  last_step = step[visit_index == max(visit_index)]
        ) %>%
        filter(num_conversions > 0)

    expected_first_touch_conversions <- expected_campaign_conversions %>%
        group_by(first_step) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value)) %>%
        ungroup() %>%
        rename(channel_name = first_step)

    expected_last_touch_conversions <- expected_campaign_conversions %>%
        group_by(last_step) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value)) %>%
        ungroup() %>%
        rename(channel_name = last_step)

    expect_equal(expected_first_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(num_conversions),
                 channel_attribution %>%
                     filter(attribution_name == 'First Touch' & attribution_type == 'Conversion') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_first_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(conversion_value),
                 channel_attribution %>%
                     filter(attribution_name == 'First Touch' & attribution_type == 'Conversion Value') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_last_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(num_conversions),
                 channel_attribution %>%
                     filter(attribution_name == 'Last Touch' & attribution_type == 'Conversion') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_last_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(conversion_value),
                 channel_attribution %>%
                     filter(attribution_name == 'Last Touch' & attribution_type == 'Conversion Value') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    ########
    # first conversion: FALSE
    # separate path_ids: TRUE
    ########
    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=FALSE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)

    channel_attribution <- rt_get_channel_attribution(campaign_paths)
    channel_attribution_2 <- rt_get_channel_attribution(campaign_paths, .conversion_value = NULL)
    expect_true(rt_are_dataframes_equal(channel_attribution %>%
                                            # for some reason, including .conversion_value slightly changes results for markov
                                            filter(attribution_type == 'Conversion' & attribution_name != 'Markov'),
                                        channel_attribution_2 %>%
                                            # for some reason, including .conversion_value slightly changes results for markov
                                            filter(attribution_name != 'Markov')))

    found_conversions <- channel_attribution %>%
        filter(attribution_type == 'Conversion') %>%
        pivot_wider(names_from = 'attribution_name',
                    values_from = 'attribution_value') %>%
        select_if(is.numeric) %>%
        colSums() %>%
        unique()
    expect_equal(length(found_conversions), 1)
    expect_equal(found_conversions, sum(campaign_paths$num_conversions))

    found_conversions <- channel_attribution %>%
        filter(attribution_type == 'Conversion Value') %>%
        pivot_wider(names_from = 'attribution_name',
                    values_from = 'attribution_value') %>%
        select_if(is.numeric) %>%
        colSums() %>%
        round(6) %>%
        unique()

    expect_equal(length(found_conversions), 1)
    expect_equal(found_conversions, sum(campaign_paths$conversion_value))

    conversions_paths <- campaign_paths %>% filter(num_conversions > 0)

    path_split <- str_split(conversions_paths$path_sequence, pattern = ' > ', simplify =  FALSE)
    conversions_paths$first_touch <- map_chr(path_split, ~ .[[1]])
    conversions_paths$last_touch <- map_chr(path_split, ~ .[[length(.)]])

    expected_first_touch_conversions <- conversions_paths %>%
        group_by(first_touch) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value)) %>%
        ungroup() %>%
        rename(channel_name = first_touch)

    expected_last_touch_conversions <- conversions_paths %>%
        group_by(last_touch) %>%
        summarise(num_conversions = sum(num_conversions),
                  conversion_value = sum(conversion_value)) %>%
        ungroup() %>%
        rename(channel_name = last_touch)

    expect_equal(expected_first_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(num_conversions),
                 channel_attribution %>%
                     filter(attribution_name == 'First Touch' & attribution_type == 'Conversion') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_first_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(conversion_value),
                 channel_attribution %>%
                     filter(attribution_name == 'First Touch' & attribution_type == 'Conversion Value') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_last_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(num_conversions),
                 channel_attribution %>%
                     filter(attribution_name == 'Last Touch' & attribution_type == 'Conversion') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))

    expect_equal(expected_last_touch_conversions %>%
                     arrange(channel_name) %>%
                     pull(conversion_value),
                 channel_attribution %>%
                     filter(attribution_name == 'Last Touch' & attribution_type == 'Conversion Value') %>%
                     arrange(channel_name) %>%
                     pull(attribution_value))
})

test_that("rt_plot_channel_attribution", {
    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions()

    steps <- campaign_data %>%
        select(step, step_type) %>%
        distinct()
    channel_categories <- steps$step_type
    names(channel_categories) <- steps$step

    ########
    # first conversion: TRUE
    # separate path_ids
    ########
    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)
    channel_attribution <- rt_get_channel_attribution(campaign_paths)

    test_save_plot(file_name='data/rt_plot_channel_attribution__first_conversion__separate_paths.png',
                   plot=rt_plot_channel_attribution(channel_attribution))

    test_save_plot(file_name='data/rt_plot_channel_attribution__first_conversion__separate_paths_2.png',
                   plot=rt_plot_channel_attribution(channel_attribution, channel_categories))

    test_save_plot(file_name='data/rt_plot_channel_attribution__first_conversion__separate_paths_conv.png',
                   plot=rt_plot_channel_attribution(channel_attribution %>%
                                                        filter(attribution_type == 'Conversion')))

    test_save_plot(file_name='data/rt_plot_channel_attribution__first_conversion__separate_paths_chan.png',
                   plot=rt_plot_channel_attribution(channel_attribution %>%
                                                        filter(attribution_type == 'Conversion'),
                                                    channel_categories))

    ########
    # first conversion: FALSE
    # separate path_ids TRUE
    ########
    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=FALSE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)
    channel_attribution <- rt_get_channel_attribution(campaign_paths)

    test_save_plot(file_name='data/rt_plot_channel_attribution__all_conversion__separate_paths.png',
                   plot=rt_plot_channel_attribution(channel_attribution))

    test_save_plot(file_name='data/rt_plot_channel_attribution__all_conversion__separate_paths_2.png',
                   plot=rt_plot_channel_attribution(channel_attribution, channel_categories))

    test_save_plot(file_name='data/rt_plot_channel_attribution__all_conversion__separate_paths_conv.png',
                   plot=rt_plot_channel_attribution(channel_attribution %>%
                                                        filter(attribution_type == 'Conversion')))

    test_save_plot(file_name='data/rt_plot_channel_attribution__all_conversion__separate_paths_chan.png',
                   plot=rt_plot_channel_attribution(channel_attribution %>%
                                                        filter(attribution_type == 'Conversion'),
                                                    channel_categories))
})

test_that("rt_get_any_touch_attribution", {

    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions() %>%
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE)

    # function has internal checks
    conversion_matrix <- rt_get_any_touch_attribution(campaign_data, .conversion_column = 'num_conversions') %>%
        arrange(channel_name)
    expect_identical(as.character(conversion_matrix$channel_name),
                     c("Facebook", "Instagram", "Online Display", "Online Video", "Paid Search"))
    expect_true(all.equal(round(conversion_matrix$any_touch, 5), round(c(0.2932551, 0.1700880, 0.1202346, 0.1524927, 0.2639296), 5)))
    expect_true(sum(conversion_matrix$any_touch) == 1)

    # function has internal checks
    conversion_matrix <- rt_get_any_touch_attribution(campaign_data, .conversion_column = 'conversion_value') %>%
        arrange(channel_name)
    expect_identical(as.character(conversion_matrix$channel_name),
                     c("Facebook", "Instagram", "Online Display", "Online Video", "Paid Search"))
    expect_true(all.equal(round(conversion_matrix$any_touch, 5), round(c(0.3014159, 0.1732661, 0.1187905, 0.1569474, 0.2495800), 5)))
    expect_true(sum(conversion_matrix$any_touch) == 1)

    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions() %>%
        rt_campaign_add_path_id(.use_first_conversion=FALSE, .sort=TRUE)

    # function has internal checks
    conversion_matrix <- rt_get_any_touch_attribution(campaign_data, .conversion_column = 'num_conversions') %>%
        arrange(channel_name)
    expect_identical(as.character(conversion_matrix$channel_name),
                     c("Facebook", "Instagram", "Online Display", "Online Video", "Paid Search"))
    expect_true(all.equal(round(conversion_matrix$any_touch, 5), round(c(0.2979275, 0.1735751, 0.1191710, 0.1450777, 0.2642487), 5)))
    expect_true(sum(conversion_matrix$any_touch) == 1)

    # function has internal checks
    conversion_matrix <- rt_get_any_touch_attribution(campaign_data, .conversion_column = 'conversion_value') %>%
        arrange(channel_name)
    expect_identical(as.character(conversion_matrix$channel_name),
                     c("Facebook", "Instagram", "Online Display", "Online Video", "Paid Search"))
    expect_true(all.equal(round(conversion_matrix$any_touch, 5), round(c(0.3109495, 0.1813516, 0.1146279, 0.1509837, 0.2420873), 5)))
    expect_true(sum(conversion_matrix$any_touch) == 1)
})

test_that("rt_get_any_touch_attribution2", {

    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions() %>%
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE)

    # function has internal checks
    conversion_matrix_conv <- rt_get_any_touch_attribution(campaign_data, .conversion_column = 'num_conversions') %>%
        rename(any_touch_conversions = any_touch)

    conversion_matrix_value <- rt_get_any_touch_attribution(campaign_data, .conversion_column = 'conversion_value') %>%
        rename(any_touch_value = any_touch)

    any_touch_attribution <- inner_join(conversion_matrix_conv, conversion_matrix_value, by = 'channel_name')
    any_touch_attribution <- rt_attribution_pivot_longer(any_touch_attribution)

    campaign_paths <- campaign_data %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)

    channel_attribution <- rt_get_channel_attribution(campaign_paths)

    all_models <- any_touch_attribution %>%
        bind_rows(channel_attribution %>%
                      group_by(attribution_name, attribution_type) %>%
                      mutate(total_attribution = sum(attribution_value)) %>%
                      ungroup() %>%
                      mutate(attribution_value = attribution_value / total_attribution) %>%
                      select(-total_attribution))

    test_save_plot(file_name='data/rt_plot_channel_attribution__any_touch_all_models.png',
                   plot=rt_plot_channel_attribution(all_models))
})

test_that("TODO", {
    skip("sandbox")

    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions()

    steps <- campaign_data %>%
        select(step, step_type) %>%
        distinct()

    channel_categories <- steps$step_type
    names(channel_categories) <- steps$step

    ########
    # first conversion: TRUE
    # separate path_ids
    ########
    campaign_paths <- campaign_data %>%
        mutate(num_conversions = ifelse(num_conversions > 0, 1, 0)) %>%
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)
    markov_model_results <- rt_markov_model(campaign_paths)



    P <- markov_model_results$transition_matrix %>%
        pivot_wider(names_from = c('channel_to'), values_from='transition_probability')

    rowSums(P %>% select(-channel_from), na.rm = TRUE)

    P <- P %>%
        bind_rows(data.frame(channel_from="(conversion)")) %>%
        bind_rows(data.frame(channel_from="(null)"))
    P[['(start)']] <- NA

    identical(sort(P$channel_from), sort(colnames(P) %>% rt_remove_val('channel_from')))

    P <- P %>% select(P$channel_from) %>% as.matrix()
    rownames(P) <- colnames(P)
    P[is.na(P)] <- 0
    P["(conversion)", "(conversion)"] <- 1
    P["(null)", "(null)"] <- 1
    rowSums(P)
    P %>% View()

    P_states <- unique()




    matrixpower <- function(mat,k) {
        if (k == 0) {

            mat_diag <- diag(dim(mat)[1])
            rownames(mat_diag) <- rownames(mat)
            colnames(mat_diag) <- colnames(mat)
            return (mat_diag)
        }
        if (k == 1) return(mat)
        if (k > 1) return( mat %*% matrixpower(mat, k-1))
    }

    long_term <- matrixpower(P, 1000)
    rowSums(long_term)

    Q_absorb <- function(P, transient_states) {
        P[transient_states, transient_states]
    }

    R_absorb <- function(P, transient_states, absorbing_states) {
        P[transient_states, absorbing_states]
    }
    fundamental_matrix <- function(P, transient_states) {
        Q <- Q_absorb(P, transient_states)
        return (solve(diag(length(transient_states)) - Q))
    }

    transient_states <- 1:6
    absorbing_states <- 7:8

    Q <- Q_absorb(P, transient_states)
    Q

    R <- R_absorb(P, transient_states, absorbing_states)
    R

    (fund_matrix <- fundamental_matrix(P, transient_states))
    rowSums(fund_matrix)
    fund_matrix %*% R
    long_term

    #long_term %>% View()
    long_term <- long_term[, c('(conversion)', '(null)')]
    long_term
    long_term["(start)", "(conversion)"]
    sum(campaign_paths$num_conversions) / nrow(campaign_paths)

    as.data.frame(long_term) %>%
        mutate(channel=rownames(.)) %>%
        filter(!str_detect(channel, "\\(")) %>%
        ggplot(aes(channel, `(conversion)`)) +
        geom_point()



    temp <- campaign_data %>%
        rt__mock__attribution_to_clickstream()
    temp <- bind_rows(temp %>%
                          select(id) %>%
                          distinct() %>%
                          mutate(step="(start)",
                                 index = 0),
                      temp %>%
                          group_by(id) %>%
                          mutate(index = row_number()) %>%
                          ungroup() %>%
                          mutate(step = ifelse(num_conversions > 0, '(conversion)', step)) %>%
                          select(id, step, index)) %>%
        bind_rows(temp %>%
                      group_by(id) %>%
                      summarise(had_conversion = any(num_conversions > 0)) %>%
                      filter(!had_conversion) %>%
                      select(-had_conversion) %>%
                      mutate(step="(null)",
                             index = 10000000)) %>%
        arrange(id, index)

    temp <- temp %>%
        group_by(id) %>%
        mutate(next_step = lead(step)) %>%
        ungroup() %>%
        mutate(next_step = case_when(
            is.na(next_step) & step == "(conversion)" ~ "(conversion)",
            is.na(next_step) & step == "(null)" ~ "(null)",
            TRUE ~ next_step
        ))
    #mutate(next_step = ifelse(is.na(next_step), "(conversion)", next_step))

    temp_table <- table(Current=temp$step, Next=temp$next_step)
    temp_table <- temp_table / rowSums(temp_table)
    temp_table <- temp_table[colnames(temp_table), ]
    temp_table %>% View()



    matrixpower(temp_table, 1000)[, c(1,2)]
    long_term[rownames(temp_table),]

    matrixpower(temp_table, 100) %>% View()
    P %>% View()


    initial_paths <- str_split(campaign_paths$path_sequence, ' > ', simplify = TRUE)[, 1]

    initial_counts <- table(initial_paths)
    # put in correct order
    initial_counts <- initial_counts[colnames(P) %>% rt_remove_val(c("(start)", "(conversion)", "(null)"))]

    initial_state <- c(0, as.numeric(initial_counts), 0, 0)

    initial_state %*% matrixpower(P, 100)
    markov_model_results$removal_effects

    c(sum(initial_state), rep(0, 7)) %*% matrixpower(P, 100) %>% View()

    sum(initial_state)
    254.8636 + 3415.136

    total_conversions
    total_touches


    sum(total_conversions)
    sum(total_touches)
})

test_that("TODO", {
    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions() %>%
        # only care about the first conversion since it is percent converted
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE)

    touch_type <- "any touch"
    if(touch_type == "first touch") {

        path_matrix <- campaign_data %>%
            select(.path_id, timestamp, step) %>%
            distinct() %>%
            mutate(visit=1) %>%
            group_by(.path_id) %>%
            filter(row_number(timestamp) == 1) %>%
            ungroup()

    } else if(touch_type == "last touch") {
        path_matrix <- campaign_data %>%
            select(.path_id, timestamp, step) %>%
            distinct() %>%
            mutate(visit=1) %>%
            group_by(.path_id) %>%
            filter(row_number(timestamp) == max(row_number(timestamp))) %>%
            ungroup()

    } else if(touch_type == "any touch") {

        path_matrix <- campaign_data %>%
            select(.path_id, step) %>%
            distinct() %>%
            mutate(visit=1)

    } else {
        stopifnot(FALSE)
    }

    path_matrix <- path_matrix %>%
        pivot_wider(names_from = step,
                    values_from=visit,
                    values_fill = list(visit = 0)) %>%
        inner_join(campaign_data %>%
                       group_by(.path_id) %>%
                       summarise(converted = any(num_conversions > 0)),
                   by = '.path_id') #%>%
    #select(-.path_id)

    rt_stopif(any(duplicated(path_matrix$.path_id)))
    if(touch_type == "any touch") {

        path_matrix <- path_matrix %>% select(-.path_id)
    } else {

        path_matrix <- path_matrix %>% select(-.path_id, -timestamp)
    }

    show_conversion_rate_vs_touches <- function(total_touches,
                                                total_conversions,
                                                markov_model_results=NULL) {
        percent_conversions <- total_conversions / total_touches
        conversion_df <- data.frame(channel_name=names(percent_conversions),
                                    percent_conversions=percent_conversions,
                                    total_touches=total_touches)

        if(is.null(markov_model_results)) {

            expand_scale_multiplier <- 0.1

        } else {
            expand_scale_multiplier <- 0.2

            conversion_df <- conversion_df %>%
                inner_join(markov_model_results$removal_effects, by='channel_name')
        }

        plot_object <- conversion_df %>%
            filter(channel_name != 'converted') %>%
            ggplot(aes(x=total_touches, y =percent_conversions)) +
            geom_text(aes(label = channel_name), vjust=-1) +
            scale_y_continuous(expand=expand_scale(mult=expand_scale_multiplier),
                               breaks = pretty_breaks(10), labels = percent_format()) +
            scale_x_continuous(expand=expand_scale(mult=expand_scale_multiplier),
                               breaks = pretty_breaks(10), labels = rt_pretty_numbers_short) +
            #expand_limits(y=c(0, 1), x=0) +
            theme_light()

        if(!is.null(markov_model_results)) {
            plot_object <- plot_object +
                geom_text(aes(label = paste0("(", round(removal_effects_conversion, 3), ")")),
                          vjust=2,
                          size=3.3) +
                geom_point(aes(size=removal_effects_conversion)) +
                labs(size='Markov Removal Effects')
        } else {
            plot_object <- plot_object +
                geom_point()
        }

        return (plot_object)
    }

    total_touches <- colSums(path_matrix)
    total_conversions <- colSums(path_matrix %>% filter(converted))

    show_conversion_rate_vs_touches(total_touches, total_conversions)

    campaign_paths <- campaign_data %>%
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE) %>%
        rt_campaign_to_markov_paths(.separate_paths_ids=TRUE)
    markov_model_results <- rt_markov_model(campaign_paths)

    markov_model_results$removal_effects
    show_conversion_rate_vs_touches(total_touches, total_conversions, markov_model_results)

})

test_that("TODO", {
    ########################################
    # All
    ########################################

    # campaign_data %>%
    #     group_by(id) %>%
    #     summarise(converted = any(num_conversions > 0)) %>%
    #     pull(converted) %>% sum()

    campaign_data <- readRDS('data/campaign_data__small.RDS') %>%
        test_helper__campaign_add_conversions() %>%
        # only care about the first conversion since it is percent converted
        rt_campaign_add_path_id(.use_first_conversion=TRUE, .sort=TRUE)

    library(networkD3)
    campaign_data_2 <- campaign_data %>%
        group_by(id) %>%
        mutate(converted = any(num_conversions > 0)) %>%
        mutate(first_converted = ifelse(converted, min(timestamp[num_conversions > 0], na.rm = TRUE), NA)) %>%
        ungroup() %>%
        # this is getting unique channel, but perhaps have an option not to get unique
        # need to do this after conversion logic above because if the person converts on the nth time
        # for a particular channel, this will have filtered out their conversion event
        group_by(id, step) %>%
        filter(row_number(timestamp) == 1) %>%
        ungroup() %>%
        filter(is.na(first_converted) | timestamp <= first_converted) %>%
        select(-first_converted) %>%
        arrange(id, timestamp)

    # campaign_data_2 %>%
    #     group_by(id) %>%
    #     summarise(converted = any(converted)) %>%
    #     pull(converted) %>% sum()

    bounced_cookies <- campaign_data_2 %>%
        filter(!converted) %>%
        group_by(id) %>%
        summarise(timestamp = max(timestamp),
                  step = 'Bounced',
                  converted=FALSE)
    bounced_cookies$timestamp <- bounced_cookies$timestamp + seconds(1)

    converted_cookies <- campaign_data_2 %>%
        filter(converted) %>%
        group_by(id) %>%
        summarise(timestamp = max(timestamp),
                  step = 'Converted',
                  converted=TRUE)
    converted_cookies$timestamp <- converted_cookies$timestamp + seconds(1)


    campaign_data_t <- campaign_data_2 %>%
        select(id, timestamp, step, converted) %>%
        bind_rows(bounced_cookies) %>%
        bind_rows(converted_cookies) %>%
        arrange(id, timestamp) %>%
        group_by(id) %>%
        mutate(visit_index = row_number(timestamp),
               visit_index_rev = rev(visit_index)) %>%
        ungroup() %>%
        select(id, step, visit_index, visit_index_rev, converted)

    # comment this out to get bounced
    campaign_data_t <- campaign_data_t %>% filter(converted)

    campaign_data_t <- campaign_data_t %>%
        unite(channel_source, c(step, visit_index)) %>%
        group_by(id) %>%
        mutate(channel_target = lead(channel_source)) %>%
        ungroup() %>%
        filter(!is.na(channel_target)) %>%
        mutate(channel_target = case_when(
            str_detect(channel_target, 'Bounced') ~ 'Bounced',
            str_detect(channel_target, 'Converted') ~ 'Converted',
            TRUE ~ channel_target
        ))

    campaign_data_t <- campaign_data_t %>%
        unite(step, c(channel_source, channel_target), remove = FALSE) %>%
        group_by(step, channel_source, channel_target) %>%
        summarise(n=n(),
                  n_d=n_distinct(id)) %>%
        ungroup()


    #campaign_data_t %>% View()

    unique_nodes <- bind_rows(campaign_data_t %>% count(channel_source, wt=n) %>% arrange(n) %>% select(channel_source, n) %>% rename(channel_name=channel_source),
                              campaign_data_t %>% count(channel_target, wt=n) %>% arrange(n) %>% select(channel_target, n) %>% rename(channel_name=channel_target)) %>%
        count(channel_name, wt=n) %>%
        arrange(desc(n)) %>%
        pull(channel_name)

    #campaign_data_t %>% count(channel_source, wt=n) %>% arrange(n) %>% pull(channel_source)
    #campaign_data_t %>% count(channel_source, wt=n) %>% arrange(n) %>% pull(channel_source)
    #unique_nodes <- unique(c(campaign_data_t$channel_source, campaign_data_t$channel_target))
    # target_nodes <- unique(campaign_data_t$channel_target)

    source_indexes <- match(campaign_data_t$channel_source, unique_nodes) - 1
    target_indexes <- match(campaign_data_t$channel_target, unique_nodes) - 1

    campaign_data_t$source <- source_indexes
    campaign_data_t$target <- target_indexes

    unique_nodes <- str_remove(string=unique_nodes, pattern = "_.*")
    sankey_nodes_df <- data.frame(name=c(unique_nodes))

    color_string <- rt_str_collapse(rt_colors(),.surround = '"', .separate = ", ")
    ColourScal <- paste0('d3.scaleOrdinal().range([', color_string,'])')
    #campaign_data_t %>% View()
    #sankey_nodes_df %>% View()
    sankeyNetwork(Links = campaign_data_t,
                  Nodes = sankey_nodes_df,
                  Source = 'source',
                  Target = 'target',
                  Value = 'n',
                  NodeID = 'name',
                  colourScale = ColourScal,
                  #units = 'TWh',
                  fontSize = 12, nodeWidth = 30)

})

test_that("TODO", {

    skip("sandbox")

    table(ifelse(path_matrix$Facebook == 1, 'Yes', 'No'),
          ifelse(path_matrix$converted == 1, 'Converted', 'Not Converted'))
    regression_result <- glm(converted ~ .,
                             data = path_matrix,
                             family = binomial(link='logit'),
                             maxit = 100)
    summary(regression_result)

    regression_result <- glm(converted ~ .,
                             data = path_matrix,
                             family = "binomial",
                             maxit = 100)
    summary(regression_result)

    # regression_result <- lm(converted ~ ., data = path_matrix)
    # summary(regression_result)

    #install.packages('arm')
    #library(arm)

    rt_explore_plot_correlations(path_matrix %>% select(converted, everything()))
    rt_explore_correlations(path_matrix)

    regression_result <- arm::bayesglm(converted ~ .,
                                       data = path_matrix,
                                       family = "binomial",
                                       maxit = 100)

    summary(regression_result)




    campaign_data %>%
        head(400) %>%
        group_by(cookie) %>%
        arrange(time) %>%
        mutate(cumsum_conv=cumsum(conversion),
               lag_cumsum_conv=lag(cumsum(conversion)),
               path_no = ifelse(is.na(lag_cumsum_conv), 0, lag_cumsum_conv) + 1) %>%
        ungroup() %>%
        select(-lag_cumsum_conv) %>%
        as.data.frame() %>%
        arrange(cookie, time) %>%
        rt_peak()

    ########################################
    # First & Last Touchf
    ########################################
    campaign_data_first_last <- campaign_data %>%
        group_by(id) %>%
        mutate(visit_index = row_number(timestamp),
               visit_index_rev = row_number(desc(timestamp))) %>%
        ungroup() %>%
        filter(visit_index == 1 | visit_index_rev == 1)


    first_last_channel <- campaign_data_first_last %>%
        group_by(id) %>%
        summarise(first_channel = step[visit_index == 1],
                  last_channel = step[visit_index_rev == 1])

    sankey_dataframe <- first_last_channel %>%
        mutate(path = paste(first_channel, '-', last_channel)) %>%
        count(path, name='num_paths') %>%
        arrange(desc(num_paths)) %>%
        filter(num_paths > 20) %>%
        separate(path, into = c('first_channel', 'last_channel'), sep = ' - ')


    first_nodes <- unique(sankey_dataframe$first_channel)
    last_nodes <- unique(sankey_dataframe$last_channel)


    source_indexes <- match(sankey_dataframe$first_channel, first_nodes) - 1
    target_indexes <- match(sankey_dataframe$last_channel, last_nodes) + length(first_nodes) - 1

    sankey_dataframe$source <- source_indexes
    sankey_dataframe$target <- target_indexes
    sankey_nodes_df <- data.frame(name=c(first_nodes, last_nodes))

    sankeyNetwork(Links = sankey_dataframe,
                  Nodes = sankey_nodes_df,
                  Source = 'source',
                  Target = 'target',
                  Value = 'num_paths',
                  NodeID = 'name',
                  #units = 'TWh',
                  fontSize = 12, nodeWidth = 30)


    ?sankeyNetwork
    sankeyNetwork(Links = energy$links,
                  Nodes = energy$nodes,
                  Source = 'source',
                  Target = 'target', Value = 'value', NodeID = 'name',
                  units = 'TWh', fontSize = 12, nodeWidth = 30)






























    first_nodes <- unique(sankey_dataframe$first_channel)
    last_nodes <- unique(sankey_dataframe$last_channel)


    source_indexes <- match(sankey_dataframe$first_channel, first_nodes) - 1
    target_indexes <- match(sankey_dataframe$last_channel, last_nodes) + length(first_nodes) - 1

    sankey_dataframe$source <- source_indexes
    sankey_dataframe$target <- target_indexes
    sankey_nodes_df <- data.frame(name=c(first_nodes, last_nodes))

    sankeyNetwork(Links = sankey_dataframe,
                  Nodes = sankey_nodes_df,
                  Source = 'source',
                  Target = 'target',
                  Value = 'num_paths',
                  NodeID = 'name',
                  #units = 'TWh',
                  fontSize = 12, nodeWidth = 30)








    str_detect()

    ?unite



    energy$links
    energy$nodes






    campaign_data_2 %>%
        group_by(cookie) %>%
        summarise(n = n()) %>%
        pull(n) %>% histogram()

    campaign_data_2 %>%
        group_by(cookie) %>%
        mutate(path = paste0(channel, collapse = ' > ')) %>%
        ungroup() %>%
        count(path, name='num_paths') %>%
        arrange(desc(num_paths)) %>%
        View()


    energy$links
    energy$nodes









    # simple example
    i <- c(1,3:8);
    j <- c(2,9,6:10);
    x <- 7 * (1:7)
    library(Matrix)
    (A <- sparseMatrix(i, j, x = x))                    ##  8 x 10 "dgCMatrix"
    summary(A)

    campaign_data_2 %>% rt_peak()
    campaign_data_2 %>% View()
    # loop through each possible value
    possible_channels <- unique(campaign_data_2$channel)
    # for each channel, count how many people go to another channel (or convert; or drop off ())
    for(channel in possible_channels) {

    }




    campaign_data_all %>%
        group_by(path) %>%
        count(path, name='num_paths') %>%
        arrange(desc(num_paths)) %>%
        separate(path, into=NA, sep = ' > ')

    ?separate




    sankey_dataframe <- first_last_channel %>%
        mutate(path = paste(first_channel, '-', last_channel)) %>%
        count(path, name='num_paths') %>%
        arrange(desc(num_paths)) %>%
        filter(num_paths > 20) %>%
        separate(path, into = c('first_channel', 'last_channel'), sep = ' - ')


    first_nodes <- unique(sankey_dataframe$first_channel)
    last_nodes <- unique(sankey_dataframe$last_channel)


    source_indexes <- match(sankey_dataframe$first_channel, first_nodes) - 1
    target_indexes <- match(sankey_dataframe$last_channel, last_nodes) + length(first_nodes) - 1

    sankey_dataframe$source <- source_indexes
    sankey_dataframe$target <- target_indexes
    sankey_nodes_df <- data.frame(name=c(first_nodes, last_nodes))

    sankeyNetwork(Links = sankey_dataframe,
                  Nodes = sankey_nodes_df,
                  Source = 'source',
                  Target = 'target',
                  Value = 'num_paths',
                  NodeID = 'name',
                  #units = 'TWh',
                  fontSize = 12, nodeWidth = 30)


    ?sankeyNetwork
    sankeyNetwork(Links = energy$links,
                  Nodes = energy$nodes,
                  Source = 'source',
                  Target = 'target', Value = 'value', NodeID = 'name',
                  units = 'TWh', fontSize = 12, nodeWidth = 30)





    ##### sankey
    # Libraries
    library(tidyverse)
    library(viridis)
    #install.packages('patchwork')
    library(patchwork)
    #install.packages('hrbrthemes')
    library(hrbrthemes)
    #install.packages('circlize')
    library(circlize)

    # Load dataset from github
    data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
    # Package
    #install.packages('networkD3')
    library(networkD3)

    # I need a long format
    colnames(data) <- str_replace_all(colnames(data), "\\.", " ")
    data_long <- data %>%
        rownames_to_column %>%
        gather(key = 'key', value = 'value', -rowname) %>%
        filter(value > 0)
    colnames(data_long) <- c("source", "target", "value")

    nodes <- nodes %>% mutate(name = str_replace_all(name, "\\.", " "))

    data_long$target <- paste(data_long$target, " ", sep="")

    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    data_long$IDsource=match(data_long$source, nodes$name)-1
    data_long$IDtarget=match(data_long$target, nodes$name)-1

    color_string <- rt_str_collapse(rt_colors(),.surround = '"', .separate = ", ")
    ColourScal <- paste0('d3.scaleOrdinal().range([', color_string,'])')
    # prepare colour scale
    #ColourScal ='d3.scaleOrdinal().range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

    # Make the Network
    ?sankeyNetwork
    sankeyNetwork(Links = data_long, Nodes = nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "name",
                  sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)











    ## Not run:
    # Recreate Bostock Sankey diagram: http://bost.ocks.org/mike/sankey/
    # Load energy projection data
    URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/',
                  'master/JSONdata/energy.json')
    energy <- jsonlite::fromJSON(URL)

    # Plot
    duplicated(energy$nodes)
    sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
                  Target = 'target', Value = 'value', NodeID = 'name',
                  units = 'TWh', fontSize = 12, nodeWidth = 30)

    # Colour links
    energy$links$energy_type <- sub(' .*', '',
                                    energy$nodes[energy$links$source + 1, 'name'])

    sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
                  Target = 'target', Value = 'value', NodeID = 'name',
                  LinkGroup = 'energy_type', NodeGroup = NULL)

})

