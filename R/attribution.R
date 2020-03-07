#' xxxx
#'
#' @param xxx xxx
#'
#' @importFrom xxx xxx
#' @export
rt_markov_chains_example_data <- function(seed=42){


    return (xxx)
}


#' transforms .campaign_data into structure that is based on events, meaning the conversion would be
#' a specific event (e.g. clicking a submit button on a website)
#' this mimics what we would typically see from a clickstream
#' randomly assigns "Button Submit Event" & "Button Submit Event 2"
#' randomly gives the conversion timestamp either the same timestamp of the corresponding step or 1 second after
#' @param .campaign_data dataframe with id|timestamp|step|step_type|num_conversions|conversino_value columns
#'      This dataframe has a value num_conversions on the step association with the conversion.
#'      So for example, if a user signs up on the pricing page, there is no specific signup step/event,
#'      but rather a step for the pricing page indicating number of conversions (probably always 1 for a signup event)
#'      and total value of conversions
#'
#' @importFrom dplyr filter bind_rows mutate if_else arrange
#' @importFrom stringr str_ends
#' @importFrom lubridate seconds
#'
#' @export
rt__mock__attribution_to_clickstream <- function(.campaign_data) {

    conversion_clickstream <- .campaign_data %>%
        filter(num_conversions > 0)
    # need to make the current step worth 0 (because it isn't the conversion event, just where the conversion happened)
    # then create a conversion event with a timestamp that is the same

    conversion_clickstream <- bind_rows(conversion_clickstream %>%
                                            mutate(num_conversions = 0, conversion_value = 0),
                                        conversion_clickstream %>%
                                            mutate(step = if_else(str_ends(id, 'f'), 'Button Submit Event 2', 'Button Submit Event'),
                                                   step_type = 'Conversion',
                                                   # actually, i should make some of the time-stamps the same
                                                   # and some 1 second after to mimic what might
                                                   # happen in the click-stream data
                                                   # the dataset i'm working with, this id has another event 1
                                                   # second after, so ensure the conversion event has the same timestamp for this particular id
                                                   timestamp = if_else(str_ends(id, 'f') | id == '00F7EkFhfiA7fFA73E0niiBfn', timestamp, timestamp + seconds(1)))
    )

    click_stream_data <- bind_rows(.campaign_data %>% filter(num_conversions == 0),
                                   conversion_clickstream) %>%
        arrange(id, timestamp)

    return (click_stream_data)
}

#' transforms .clickstream_data into the expected format for attribution calculations
#' different types of conversion events will be ignored, so the user is expected to filter out any
#' conversion events they are not interested in.
#'
#' This function does not handle the case when the there are multiple types of conversions that are
#' triggered from a single step
#' For example: Someone lands on the homepage and from that page does 2 conversions `Lead Form-Fill` & `Lead Sign-up`)
#' The user is expected to filter the events of interest accordingly.
#'
#' @param .clickstream_data dataframe with id|timestamp|step|step_type|num_conversions|conversion_value columns
#'     This dataframe has "clickstream" data, which means that it has a list of steps/events that might
#'          correspond to, for example, page visits on a website.
#'     num_conversions should indicate which steps are conversion events.
#'     A conversion event should be its own step, that has a timestamp equal to or after the step that
#'     should get the conversion event.
#'         For example, if someone visits the pricing page, and then signs up for the product (which is the conversion),
#'         there should be a single row (i.e. step) for the visit to the pricing page, and a single row for the conversion step.
#'         The timestamp of the conversion event, in this case, would be immdediately after (seconds or minutes) the pricing step.
#'         The step that is before the conversion event (regardless of how much before) gets credit (last-touch) for the conversion.
#'
#' @importFrom dplyr arrange group_by ungroup mutate lag lead row_number n select filter contains
#'
#' @export
rt_clickstream_to_attribution <- function(.clickstream_data) {

    temp <- .clickstream_data %>%
        arrange(id, timestamp, step) %>%
        # need to arrange by num_conversions so that if the conversion step and corresponding step that
        # should get credit have the same timestamp, arranging by num_conversions should ensure
        # that the conversion step comes after
        arrange(id, timestamp, num_conversions) %>%
        group_by(id) %>%
        # need to create a unique index per unique id per each path up until a conversion event
        # people can have multiple conversions
        # this will allow us to identify the event immediately before (or with the same timestamp as) the
        # conversion event (arranging by num_conversions ensures that if the corresponding step and
        # conversion event have the same timestamp, the conversion event will be ordered after)
        mutate(temp___lag_cumulative_conv=lag(cumsum(num_conversions)),
               temp___path_no = ifelse(is.na(temp___lag_cumulative_conv), 0, temp___lag_cumulative_conv) + 1) %>%
        ungroup() %>%
        group_by(id, temp___path_no) %>%
        mutate(temp___path_index = row_number(timestamp),
               temp___path_num_steps = n(),
               temp___path_has_conversions = any(num_conversions > 0),
               temp___check = ifelse(any(num_conversions > 0),
                              # temp___path_index of the conversion event should be the max path index
                              max(temp___path_index[num_conversions > 0]) == max(temp___path_index),
                              # if there aren't any conversions this check doesn't apply so return TRUE
                              TRUE),
               num_conversions = ifelse(temp___path_has_conversions, lead(num_conversions), num_conversions),
               conversion_value = ifelse(temp___path_has_conversions, lead(conversion_value), conversion_value)
               ) %>%
        ungroup() %>%
        select(-temp___lag_cumulative_conv)

    # if the path has conversions, there should be more than 1 steps
    # so either the path doesn't have conversinos, or it has 2 or more steps
    stopifnot(all(!temp$temp___path_has_conversions | temp$temp___path_num_steps >= 2))

    # this makes sure that the conversion event always has the max indexstep number
    stopifnot(all(temp$temp___check))

    return(temp %>%
        filter(!is.na(num_conversions)) %>%
        select(-contains('temp___')))
}

#' adds `.path_id` column to dataframe. 
#'
#' @param .campaign_data that is pre-arranged by id and time_stamp
#'              column has following columns
#'              optionally has a column representing the value of the conversions
#' @param .id string identifying the id column
#' @param .timestamp string identifying the timestamp column
#' @param .num_conversions string identifying the num_conversions column 
#' @param .conversion_values string identifying the conversion_values column 
#' @param .use_first_conversion if true, only use the first conversion
#' @param .reset_upon_conversion if true, treat steps after each conversion as a different path, if false, the entire path of .id is treated as one path. (.path_id is simply the id)
#' @param .sort if true, the df is sorted by id, timestamp, conversion_values, & num_conversions; the dataframe needs to be sorted in this way to work, but this option let's the user avoid this action if it has already been done
#'
#' @importFrom dplyr arrange group_by ungroup mutate filter select lag
#'
#' @export
rt_campaign_add_path_id <- function(.campaign_data,
                                    .id='id',
                                    .timestamp='timestamp',
                                    .num_conversions='num_conversions',
                                    .conversion_values='conversion_value',
                                    .use_first_conversion=TRUE,
                                    .reset_upon_conversion=TRUE,
                                    .sort=TRUE) {

    if(.sort) {
        .campaign_data <- .campaign_data %>%
            arrange(!!sym(.id), !!sym(.timestamp), !!sym(.conversion_values), !!sym(.num_conversions))
    }

    if(.use_first_conversion) {
        .campaign_data <- suppressWarnings(.campaign_data %>%
            group_by(!!sym(.id)) %>%
            # arrange(time) %>%
            mutate(.___datetimes___ = !!sym(.timestamp)) %>%
            mutate(.___first_conversion___=min(.___datetimes___[!!sym(.num_conversions) > 0], na.rm = TRUE)) %>%
            ungroup() %>%
            filter(!!sym(.timestamp) <= .___first_conversion___) %>%
            select(-.___datetimes___, -.___first_conversion___))
    }

    # .reset_upon_conversion doesn't make sense if we only are using the first conversion
    if(.reset_upon_conversion & !.use_first_conversion) {
        # treat events after conversion as new path
        .campaign_data <- .campaign_data %>%
            group_by(!!sym(.id)) %>%
            # arrange(time) %>%
            mutate(.___path_no___ = ifelse(is.na(lag(cumsum(!!sym(.num_conversions)))), 0, lag(cumsum(!!sym(.num_conversions)))) + 1) %>%
            ungroup() %>%
            mutate(.path_id = paste0(!!sym(.id), '-', .___path_no___)) %>%
            select(-.___path_no___)
    } else {

        # treat single person as 1 path regardless how many conversions
        .campaign_data <- .campaign_data %>% mutate(.path_id = !!sym(.id))
    }

    return (.campaign_data)
}

#' adds converts campaign data to path data in the format expected by ChannelAttribution::markov_model & ChannelAttribution::heuristic_models
#'
#' @param .df that is pre-arranged by id and time_stamp
#'              column has following columns
#'              optionally has a column representing the value of the conversions
#' @param .id string identifying the id column
#' @param .timestamp string identifying the timestamp column
#' @param .num_conversions string identifying the num_conversions column 
#' @param .conversion_values string identifying the conversion_values column 
#' @param .use_first_conversion if true, only use the first conversion
#' @param .reset_upon_conversion if true, treat steps after each conversion as a different path, if false, the entire path of .id is treated as one path. (.path_id is simply the id)
#'
#' @importFrom dplyr arrange group_by ungroup mutate filter select lag
#'
#' @export
rt_campaign_to_markov_paths <- function(.campaign_data, .timestamp, .sort=TRUE, .symbol=" > ") {

    if(.sort) {
        .campaign_data <- .campaign_data %>% arrange(.path_id, !!sym(.timestamp))
    }

    .campaign_data %>%
        group_by(.path_id) %>%
        #arrange(time) %>%
        summarise(.path_sequence = paste(channel, collapse = .symbol),
                  .total_conversions = sum(!!sym(.num_conversions))) %>%
        ungroup() %>%
        mutate(.null_conversion = ifelse(.total_conversions > 0 , 0, 1)) # adding information about path that have not led to conversion
}
