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
                                                   # actually, i should make some of the time-stamps the same and some 1 second after to mimic what might
                                                   # happen in the click-stream data
                                                   timestamp = if_else(str_ends(id, 'f'), timestamp, timestamp + seconds(1)))
    )

    click_stream_data <- bind_rows(.campaign_data %>% filter(num_conversions == 0),
                                   conversion_clickstream) %>%
        arrange(id, timestamp)

    return (click_stream_data)
}
