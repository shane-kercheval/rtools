context('Stats')
library(testthat)
library(dplyr)
library(ggplot2)
source('test_helpers.R')


test_that("rt_xxxxx", {

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

    # only count up until the first conversion (e.g. only count web pages visited until the "goal" i.e. conversion is reached e.g. talk-to-sales form submit)
    # reset upon each conversion (e.g. amazon style checkout where someone might convert multiple times, once they convert, it "resets" the experience and the next interactions/conversions are treated as a separate path/entity)
    # cumulative

    #' @param .df that is pre-arranged by id and time_stamp
    #'              column has following columns
    #'              optionally has a column representing the value of the conversions
    #' @param .use_first_conversion if true, only use the first conversion
    rt_campaign_add_columns <- function(.campaign_data,
                                        .id,
                                        .timestamp,
                                        .conversion,
                                        .conversion_value,


                                        .use_first_conversion=TRUE,
                                        .reset_upon_conversion=TRUE,
                                        .sort=TRUE) {

        if(.sort) {
            .campaign_data <- .campaign_data %>% arrange(!!sym(.id), !!sym(.timestamp))
        }

        if(.use_first_conversion) {
            .campaign_data <- suppressWarnings(.campaign_data %>%
                group_by(cookie) %>%
                # arrange(time) %>%
                mutate(.___datetimes___ = !!sym(.timestamp)) %>%
                mutate(.___first_conversion___=min(.___datetimes___[!!sym(.conversion) > 0], na.rm = TRUE)) %>%
                ungroup() %>%
                filter(!!sym(.timestamp) <= .___first_conversion___) %>%
                select(-.___datetimes___, -.___first_conversion___))
        }

        if(.reset_upon_conversion) {
            # treat events after conversion as new path
            .campaign_data <- .campaign_data %>%
                group_by(cookie) %>%
                # arrange(time) %>%
                mutate(.___path_no___ = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
                ungroup() %>%
                mutate(.path_id = paste0(cookie, '-', .___path_no___)) %>%
                select(-.___path_no___)
        } else {

            # treat single person as 1 path regardless how many conversions
            .campaign_data <- .campaign_data %>% mutate(.path_id = !!sym(.id))
        }

        return (.campaign_data)
    }

    rt_campaign_to_markov_paths <- function(.campaign_data, .timestamp, .sort=TRUE, .symbol=" > ") {

        if(.sort) {
            .campaign_data <- .campaign_data %>% arrange(.path_id, !!sym(.timestamp))
        }

        .campaign_data %>%
            group_by(.path_id) %>%
            #arrange(time) %>%
            summarise(.path_sequence = paste(channel, collapse = .symbol),
                      .total_conversions = sum(!!sym(.conversion))) %>%
            ungroup() %>%
            mutate(.null_conversion = ifelse(.total_conversions > 0 , 0, 1)) # adding information about path that have not led to conversion
    }


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


})
