context('Stats')
library(testthat)
library(dplyr)
library(ggplot2)
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
    clickstream_data <- rt__mock__attribution_to_clickstream(campaign_data)
    campaign_data_new <- rt_clickstream_to_attribution(clickstream_data) %>%
        arrange(id, timestamp, step)
    expect_true(rt_are_dataframes_equal(campaign_data_original %>% arrange(id, timestamp, step),
                                        campaign_data_new %>% arrange(id, timestamp, step)))
})

test_that("rt_campaign_add_columns", {

    campaign_data <- readRDS('data/campaign_data__small.RDS')
    # make 1st and 2nd events have >0 conversions
    campaign_data[1, 'num_conversions'] <- 1
    campaign_data[2, 'num_conversions'] <- 2
    # make 1st and 2nd events have >0 conversions
    campaign_data[5, 'num_conversions'] <- 2
    campaign_data[6, 'num_conversions'] <- 2
    # make 2nd and 3rd events have >0 conversions
    campaign_data[12, 'num_conversions'] <- 2
    campaign_data[13, 'num_conversions'] <- 2
    campaign_data[14, 'num_conversions'] <- 1

    set.seed(42)
    new_indexes <- sample(nrow(campaign_data), replace = FALSE)

    ######
    # .use_first_conversion=TRUE
    # .reset_upon_conversion=TRUE
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data[new_indexes, ],
                                                         .use_first_conversion=TRUE,
                                                         .reset_upon_conversion=TRUE,
                                                         .sort=TRUE)

    expected_df <- test_helper__campaign_filter_first_conversions(campaign_data)

    expect_true(rt_are_dataframes_equal(expected_df,
                                        campaign_data_transformed %>%
                                            select(-.path_id) %>%
                                            arrange(id, timestamp, conversion_value, step)))
    expect_identical(campaign_data_transformed$.path_id, campaign_data_transformed$id)
    expect_identical(campaign_data_transformed$.path_id, expected_df$id)

    ######
    # .use_first_conversion=TRUE
    # .reset_upon_conversion=FALSE
    ######
    # this should be the same as above (TRUE/TRUE) because if .use_first_conversion is TRUE then
    # .reset_upon_conversion doesn't apply
    campaign_data_transformed_2 <- rt_campaign_add_path_id(campaign_data[new_indexes, ],
                                                          .use_first_conversion=TRUE,
                                                          .reset_upon_conversion=FALSE,
                                                          .sort=TRUE)
    expect_true(rt_are_dataframes_equal(campaign_data_transformed, campaign_data_transformed_2))

    ######
    # .use_first_conversion=FALSE
    # .reset_upon_conversion=FALSE
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data[new_indexes, ],
                                                         .use_first_conversion=FALSE,
                                                         .reset_upon_conversion=FALSE,
                                                         .sort=TRUE)
    expected_df <- campaign_data %>% arrange(id, timestamp, conversion_value, step)

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
                                                         .reset_upon_conversion=TRUE,
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

    campaign_data <- readRDS('data/campaign_data__small.RDS')

    # make 1st and 2nd events have >0 conversions
    campaign_data[1, 'num_conversions'] <- 1
    campaign_data[2, 'num_conversions'] <- 2
    # make 1st and 2nd events have >0 conversions
    campaign_data[5, 'num_conversions'] <- 2
    campaign_data[6, 'num_conversions'] <- 2
    # make 2nd and 3rd events have >0 conversions
    campaign_data[12, 'num_conversions'] <- 2
    campaign_data[13, 'num_conversions'] <- 2
    campaign_data[14, 'num_conversions'] <- 1

    ######
    # .use_first_conversion=TRUE
    # .reset_upon_conversion=TRUE/FALSE results in the same thing
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data,
                                                         .use_first_conversion=TRUE,
                                                         .reset_upon_conversion=TRUE,
                                                         .sort=TRUE)

    campaign_data_paths <- rt_campaign_to_markov_paths(campaign_data_transformed)

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
    expect_identical(campaign_data_paths$null_conversion, ifelse(campaign_data__first_conversions__last_step$num_conversions > 0, 0, 1))

    # the number of times a step appears in the path sequence should match the number of times the event
    # appears in the (first time) campaign data
    path_split <- str_split(campaign_data_paths$path_sequence, pattern = ' > ', simplify =  TRUE)
    step_counts_found <- table(path_split)
    step_counts_found <- step_counts_found[-1]
    step_counts_expected <- campaign_data__first_conversions %>% count(step)
    expect_identical(names(step_counts_found), step_counts_expected$step)
    expect_equal(as.numeric(step_counts_found), step_counts_expected$n)

    ######
    # .use_first_conversion=FALSE
    # .reset_upon_conversion=TRUE
    ######
    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data,
                                                         .use_first_conversion=FALSE,
                                                         .reset_upon_conversion=TRUE,
                                                         .sort=TRUE)

    rt_campaign_to_markov_paths(campaign_data_transformed)


    ######
    # .use_first_conversion=FALSE
    # .reset_upon_conversion=FALSE
    ######


    campaign_data_transformed <- rt_campaign_add_path_id(campaign_data,
                                                         .use_first_conversion=FALSE,
                                                         .reset_upon_conversion=FALSE,
                                                         .sort=TRUE)

    rt_campaign_to_markov_paths(campaign_data_transformed)

})

test_that("rt_campaign_add_columns", {

})





markov_attribution <- ChannelAttribution::markov_model(campaign_data_paths,
                                                       var_path = ".path_sequence",
                                                       var_conv = ".num_conversions",
                                                       var_value = NULL,
                                                       order = 1, # higher order markov chain
                                                       var_null = ".null_conversion",
                                                       out_more = TRUE,
                                                       sep=">")

library(ggplot2)
library(forcats)
markov_attribution$removal_effects %>%
    mutate(channel_name = fct_reorder(channel_name, removal_effects)) %>%
    ggplot(aes(x=channel_name, y=removal_effects)) +
    geom_col() +
    coord_flip() +
    theme_light() +
    expand_limits(y=1)







    # what happens when the there are multiple types of conversions that are triggered from a single e.g. page
    # so
    # pricing -> (Lead Form-Fill & Lead Sign-up)

    .clickstream_data <- click_stream_data
    .clickstream_data %>% rt_peak(40)

    non_conversion_clickstream

    campaign_data %>%
        filter(num_conversions > 0) %>%



        campaign_data %>% rt_peak(40)







    library(ChannelAttribution)
    data(PathData)
    Data
    markov_model(Data, "path", "total_conversions")
    markov_model(Data, "path", "total_conversions", var_value="total_conversion_value")
    markov_model(Data,"path","total_conversions", var_value="total_conversion_value",
                 var_null="total_null")
    markov_model(Data, "path", "total_conversions", var_value="total_conversion_value",
                 var_null="total_null", out_more=TRUE)


    library(readr)
    campaign_data <- suppressMessages(read_csv("data/campaign_data__small.csv"))



    ?ChannelAttribution::markov_model


    .campaign_data <- campaign_data
    .id <- 'cookie'
    .timestamp = 'time'
    .conversion = 'conversion'
    .conversion_value = 'conversion_value'

    campaign_data_trans <- rt_campaign_add_columns(.campaign_data = .campaign_data,
                                                   .id = .id,
                                                   .timestamp = .timestamp,
                                                   .conversion = .conversion,
                                                   .use_first_conversion = TRUE)


    .campaign_data <- campaign_data_trans
    campaign_paths <- rt_campaign_to_markov_paths(.campaign_data = .campaign_data,
                                                  .timestamp = .timestamp)


    #unique(campaign_paths$.path_sequence)
    ?ChannelAttribution::markov_model
    markov_attribution <- ChannelAttribution::markov_model(campaign_paths,
                                       var_path = ".path_sequence",
                                       var_conv = ".total_conversions",
                                       var_value = NULL,
                                       order = 1, # higher order markov chain
                                       var_null = ".null_conversion",
                                       out_more = TRUE,
                                       sep=">")

    library(ggplot2)
    library(forcats)
    markov_attribution$removal_effects %>%
        mutate(channel_name = fct_reorder(channel_name, removal_effects)) %>%
        ggplot(aes(x=channel_name, y=removal_effects)) +
        geom_col() +
        coord_flip() +
        theme_light() +
        expand_limits(y=1)

    sum(markov_attribution$removal_effects$removal_effects)

    #?heuristic_models
    # todo: add U-shape
    heuristic_attribution <- ChannelAttribution::heuristic_models(campaign_paths,
                                              var_path = ".path_sequence",
                                              var_conv = ".total_conversions")

    all_models <- markov_attribution$result %>%
        rename(markov_weighting = total_conversions) %>%
        inner_join(heuristic_attribution, by='channel_name')

    all_models %>%
        gather(attribution_type, value, -channel_name) %>%
        mutate(attribution_type = rt_pretty_text(attribution_type)) %>%
        ggplot(aes(x=channel_name, y=value, fill=attribution_type)) +
        geom_col(position = position_dodge(width = 0.9)) +
        geom_text(aes(label=round(value)),
                  position = position_dodge(width = 0.9),
                  vjust=-0.3) +
        scale_fill_manual(values=rt_colors()) +
        theme_light() +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        labs(y='Conversions',
             x='Channel Name',
             fill="Attribution Model")


    all_models %>% select_if(is.numeric) %>% colSums()


    sum(campaign_paths$.total_conversions)
    sum(campaign_data$conversion)

    campaign_data

    markov_attribution$result$total_conversions / sum(markov_attribution$result$total_conversions)



    t <- campaign_data_trans %>%
        select(.path_id, channel, conversion) %>%
        group_by(.path_id) %>%
        mutate(had_conversion = max(conversion)) %>%
        ungroup() %>%
        select(-conversion)


    path_conversion_matrix <- campaign_data_trans %>%
        select(.path_id, channel, conversion) %>%
        group_by(.path_id) %>%
        mutate(had_conversion = max(conversion)) %>%
        ungroup() %>%
        filter(had_conversion == 1) %>%
        select(-conversion) %>%
        distinct() %>%
        pivot_wider(names_from = channel,
                    values_from=had_conversion,
                    values_fill = list(had_conversion = 0)) %>%
        select(-.path_id)
    stopifnot(all(rowSums(path_conversion_matrix) > 0))

    total_any_touch <- colSums(path_conversion_matrix)
    total_any_touch <- total_any_touch / sum(total_any_touch)

    all_models %>%
        mutate_if(is.numeric,~ . / 250) %>%
        inner_join(data.frame(channel_name=names(total_any_touch), any_touch=as.numeric(total_any_touch)),
                   by='channel_name') %>%

        gather(attribution_type, value, -channel_name) %>%
        mutate(attribution_type = rt_pretty_text(attribution_type)) %>%
        ggplot(aes(x=channel_name, y=value, fill=attribution_type)) +
        geom_col(position = position_dodge(width = 0.9)) +
        geom_text(aes(label=round(value, 3)),
                  position = position_dodge(width = 0.9),
                  vjust=-0.3) +
        scale_fill_manual(values=rt_colors()) +
        theme_light() +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        labs(y='Conversions',
             x='Channel Name',
             fill="Attribution Model")




    path_matrix <- campaign_data_trans %>%
        select(.path_id, channel) %>%
        distinct() %>%
        mutate(visit=1) %>%
        pivot_wider(names_from = channel,
                                    values_from=visit,
                                    values_fill = list(visit = 0)) %>%
        inner_join(campaign_data_trans %>%
                       group_by(.path_id) %>%
                       summarise(num_touches = n(),
                                 converted = max(conversion)),
                   by = '.path_id') %>%
        select(-.path_id)

    table(ifelse(path_matrix$Facebook == 1, 'Yes', 'No'),
          ifelse(path_matrix$converted == 1, 'Converted', 'Not Converted'))

    path_matrix %>% rt_peak(1000)

    total_touches <- colSums(path_matrix %>% select(-converted, -num_touches))
    total_conversions <- colSums(path_conversion_matrix %>% select(-converted, -num_touches))
    percent_conversions <- total_conversions / total_touches

    data.frame(channel_name=names(percent_conversions),
                  percent_conversions=percent_conversions,
                  total_touches=total_touches) %>%
        ggplot(aes(x=total_touches, y =percent_conversions)) +
        geom_point() +
        geom_text(aes(label = channel_name), vjust=-1) +
        expand_limits(y=c(0, 1), x=0) +
        theme_light()


    markov_attribution$removal_effects

    data.frame(channel_name=names(percent_conversions),
               percent_conversions=percent_conversions,
               total_touches=total_touches) %>%
        inner_join(markov_attribution$removal_effects, by='channel_name') %>%
        ggplot(aes(x=total_touches, y =percent_conversions)) +
        geom_point(aes(size=removal_effects)) +
        geom_text(aes(label = channel_name), vjust=-1) +
        geom_text(aes(label = paste0("(", round(removal_effects, 3), ")")), vjust=2) +
        theme_light()


    colnames(path_matrix)
    summary(path_matrix)

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










    markov_attribution$result



    campaign_data %>% rt_peak()

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
        group_by(cookie) %>%
        mutate(visit_index = row_number(time),
               visit_index_rev = row_number(desc(time))) %>%
        ungroup() %>%
        filter(visit_index == 1 | visit_index_rev == 1)


    first_last_channel <- campaign_data_first_last %>%
        group_by(cookie) %>%
        summarise(first_channel = channel[visit_index == 1],
                  last_channel = channel[visit_index_rev == 1])

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






    ########################################
    # All
    ########################################

    campaign_data %>%
        group_by(cookie) %>%
        summarise(converted = any(conversion > 0)) %>%
        pull(converted) %>% sum()

    campaign_data_2 <- campaign_data %>%
        group_by(cookie) %>%
        mutate(converted = any(conversion > 0)) %>%
        mutate(first_converted = min(time[conversion == 1], na.rm = TRUE)) %>%
        ungroup() %>%
        # this is getting unique channel, but perhaps have an option not to get unique
        # need to do this after conversion logic above because if the person converts on the nth time
        # for a particular channel, this will have filtered out their conversion event
        group_by(cookie, channel) %>%
        filter(row_number(time) == 1) %>%
        ungroup() %>%
        filter(is.na(first_converted) | time <= first_converted) %>%
        select(-first_converted)

    campaign_data_2 %>%
        group_by(cookie) %>%
        summarise(converted = any(converted)) %>%
        pull(converted) %>% sum()

    bounced_cookies <- campaign_data_2 %>%
        filter(!converted) %>%
        group_by(cookie) %>%
        summarise(time = max(time),
                  channel = 'Bounced',
                  converted=FALSE)
    bounced_cookies$time <- bounced_cookies$time + seconds(1)

    converted_cookies <- campaign_data_2 %>%
        filter(converted) %>%
        group_by(cookie) %>%
        summarise(time = max(time),
                  channel = 'Converted',
                  converted=TRUE)
    converted_cookies$time <- converted_cookies$time + seconds(1)


    campaign_data_t <- campaign_data_2 %>%
        select(cookie, time, channel, converted) %>%
        bind_rows(bounced_cookies) %>%
        bind_rows(converted_cookies) %>%
        arrange(cookie, time) %>%
        group_by(cookie) %>%
        mutate(visit_index = row_number(time),
               visit_index_rev = rev(visit_index)) %>%
        ungroup() %>%
        select(cookie, channel, visit_index, visit_index_rev, converted)

    campaign_data_t <- campaign_data_t %>% filter(converted)

    campaign_data_t <- campaign_data_t %>%
        unite(channel_source, c(channel, visit_index)) %>%
        group_by(cookie) %>%
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
                  n_d=n_distinct(cookie)) %>%
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
