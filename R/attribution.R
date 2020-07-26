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
#' @importFrom magrittr "%>%"
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
                                                   timestamp = if_else(str_ends(id, 'f') | id == 'fbd2f972542f5b6a9dfa602f4cac4d5c', timestamp, timestamp + seconds(1)))
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
#' @importFrom magrittr "%>%"
#' @importFrom dplyr arrange group_by ungroup mutate lead row_number n select filter contains
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
        mutate(temp___lag_cumulative_conv=dplyr::lag(cumsum(num_conversions)),
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

#' adds `.path_id` column to dataframe. Each conversion step triggers a new path-id for subsequent steps
#'
#' @param .campaign_data dataframe with columns: `id | timestamp | step | num_conversions | conversion_value`
#' @param .id string identifying the id column
#' @param .timestamp string identifying the timestamp column
#' @param .num_conversions string identifying the num_conversions column
#' @param .conversion_value string identifying the conversion_values column
#' @param .use_first_conversion if true, only use the first conversion - the path id will be identical to the .id (by definition there will never be more than 1 path)
#' @param .sort if true, the df is sorted by id, timestamp, conversion_values, & num_conversions; the dataframe needs to be sorted in this way to work, but this option let's the user avoid this action if it has already been done
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr arrange group_by ungroup mutate filter select
#'
#' @export
rt_campaign_add_path_id <- function(.campaign_data,
                                    .id='id',
                                    .timestamp='timestamp',
                                    .num_conversions='num_conversions',
                                    .conversion_value='conversion_value',
                                    .use_first_conversion=TRUE,
                                    .sort=TRUE) {

    if(.sort) {
        .campaign_data <- .campaign_data %>%
            arrange(!!sym(.id), !!sym(.timestamp), !!sym(.conversion_value), !!sym(.num_conversions))
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

        # if we only use the first conversion, by definition, there will never be multiple path ids
        # so we don't have to do the complex logic
        # treat single person as 1 path regardless how many conversions
        .campaign_data <- .campaign_data %>% mutate(.path_id = !!sym(.id))

    } else {

        # treat events after conversion as new path
        .campaign_data <- .campaign_data %>%
            group_by(!!sym(.id)) %>%
            # arrange(time) %>%
            mutate(.___path_no___ = ifelse(is.na(dplyr::lag(cumsum(!!sym(.num_conversions)))), 0, dplyr::lag(cumsum(!!sym(.num_conversions)))) + 1) %>%
            ungroup() %>%
            mutate(.path_id = paste0(!!sym(.id), '-', .___path_no___)) %>%
            select(-.___path_no___)
    }

    return (.campaign_data)
}

#' adds converts campaign data to path data in the format expected by ChannelAttribution::markov_model & ChannelAttribution::heuristic_models
#'
#' @param .campaign_data dataframe with columns `id | timestamp | step | num_conversions | conversion_value`
#' @param .id string identifying the id column
#' @param .path_id string identifying the path_id column
#' @param .step string identifying the step column
#' @param .timestamp string identifying the timestamp column
#' @param .num_conversions string identifying the num_conversions column
#' @param .conversion_value string identifying the conversion_values column
#' @param .separate_paths_ids if TRUE, each .path_id gets its own row & path_sequence. Each .id will be represented >= 1 times
#'    if FALSE, each person will only be counted once, with their entire path, and cumulative conversions/conversion-value/null-conversions
#' @param .sort if true, the df is sorted by id, timestamp, conversion_values, & num_conversions; the dataframe needs to be sorted in this way to work, but this option let's the user avoid this action if it has already been done
#' @param .symbol the symbol to separate the steps
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr arrange group_by ungroup mutate filter select
#'
#' @export
rt_campaign_to_markov_paths <- function(.campaign_data,
                                        .id='id',
                                        .path_id='.path_id',
                                        .step='step',
                                        .timestamp='timestamp',
                                        .num_conversions='num_conversions',
                                        .conversion_value='conversion_value',
                                        .separate_paths_ids=TRUE,
                                        .sort=TRUE,
                                        .symbol=" > ") {

    if(.sort) {
        .campaign_data <- .campaign_data %>%
            arrange(!!sym(.path_id), !!sym(.timestamp), !!sym(.conversion_value), !!sym(.num_conversions))
    }

    if(.separate_paths_ids) {
        .campaign_data %>%
            group_by(!!sym(.path_id)) %>%
            #arrange(time) %>%
            summarise(path_sequence = paste(!!sym(.step), collapse = .symbol),
                      num_conversions = sum(!!sym(.num_conversions)),
                      conversion_value = sum(!!sym(.conversion_value))) %>%
            ungroup() %>%
            rename(path_id = !!sym(.path_id)) %>%
            mutate(null_conversions = ifelse(num_conversions > 0 | conversion_value > 0, 0, 1)) # adding information about path that have not led to conversion
    } else {
        inner_join(
            .campaign_data %>%
                group_by(!!sym(.id)) %>%
                #arrange(time) %>%
                summarise(path_sequence = paste(!!sym(.step), collapse = .symbol),
                          num_conversions = sum(!!sym(.num_conversions)),
                          conversion_value = sum(!!sym(.conversion_value))) %>%
                ungroup() %>%
                rename(path_id = !!sym(.id)),
            .campaign_data %>%
                group_by(!!sym(.id), !!sym(.path_id)) %>%
                summarise(path_converted = any(!!sym(.num_conversions) > 0) | any(!!sym(.conversion_value) > 0)) %>%
                ungroup() %>%
                group_by(!!sym(.id)) %>%
                summarise(null_conversions = sum(!path_converted)) %>%
                rename(path_id = !!sym(.id)),
            by = "path_id")
    }
}

#' wrapper around ChannelAttribution::markov_model
#'
#' @param .path_data dataframe expected by ChannelAttribution::markov_model
#' @param .path_sequence var_path
#' @param .num_conversions var_conv
#' @param .conversion_value var_value
#' @param .null_conversions var_null
#' @param .order order
#' @param .symbol sep
#' @param .seed seed
#'
#' @importFrom ChannelAttribution markov_model
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename
#'
#' @export
rt_markov_model <- function(.path_data,
                            .path_sequence='path_sequence',
                            .num_conversions='num_conversions',
                            .conversion_value='conversion_value',
                            .null_conversions='null_conversions',
                            .order=1,
                            .symbol='>',
                            .seed=42) {

    set.seed(.seed)
    markov_attribution <- markov_model(Data = .path_data,
                                       var_path = .path_sequence,
                                       var_conv = .num_conversions,
                                       var_value = .conversion_value,
                                       var_null = .null_conversions,
                                       order = .order, # higher order markov chain
                                       out_more = TRUE,
                                       verbose = FALSE,
                                       sep=.symbol,
                                       seed = .seed)

    # inconsistent naming if var_value is NULL
    if('removal_effects' %in% names(markov_attribution$removal_effects)) {

        markov_attribution$removal_effects <- markov_attribution$removal_effects %>%
            rename(removal_effects_conversion = removal_effects)
    }

    return (markov_attribution)
}

#' graphs removal effects returned by rt_markov_model (i.e. ChannelAttribution::markov_model)
#'
#' @param .markov_attribution results from rt_markov_model
#' @param .channel_categories if provided, colors removal effects by channel categories; named vector with categories as value and channel names as vector names
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate left_join
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_col position_dodge geom_text coord_flip scale_fill_manual scale_y_continuous theme_light guides guide_legend labs theme facet_wrap
#' @importFrom scales pretty_breaks
#'
#' @export
rt_plot_markov_removal_effects <- function(.markov_attribution, .channel_categories=NULL) {

    markov_long <- .markov_attribution$removal_effects %>%
        pivot_longer(names(.markov_attribution$removal_effects) %>% rt_remove_val('channel_name'),
                     names_to = 'removal_type',
                     values_to = 'removal_value') %>%
        mutate(channel_name = as.character(channel_name),
               removal_type = rt_pretty_text(removal_type),
               removal_type = str_replace(removal_type, "Removal Effects ", ""))

    if(is.null(.channel_categories)) {

        if(length(unique(markov_long$removal_type)) == 1) {

            fill_colors <- rt_colors()[1]

        } else {

            fill_colors <- rev(rt_colors()[1:2])
        }

        markov_plot <- markov_long %>%
            mutate(removal_type = factor(removal_type, levels=c("Conversion Value", "Conversion"))) %>%
            mutate(channel_name = fct_reorder(channel_name, removal_value)) %>%
            ggplot(aes(x=channel_name, y=removal_value, fill=removal_type)) +
            geom_col(position = position_dodge(0.9),
                     alpha=0.75) +
            geom_text(aes(label=rt_pretty_axes_percent(removal_value, increase_precision_delta = 0)),
                      position = position_dodge(0.9),
                      size=3.3) +
            coord_flip() +
            scale_fill_manual(values=fill_colors) +
            scale_y_continuous(breaks=pretty_breaks(10),
                               labels=rt_pretty_axes_percent) +

            theme_light() +
            #expand_limits(y=1) +
            guides(fill = guide_legend(reverse = TRUE)) +
            labs(title="Markov Removal Effects",
                 subtitle = "Shows the estimated percent decrease in conversions from removing specific channels.",
                 x='Channel',
                 y='Estimated Removal Effect')

        if(length(unique(markov_long$removal_type)) == 1) {

            markov_plot <- markov_plot + theme(legend.position = 'none')
        }

    } else {
        .channel_categories <- data.frame(channel_name = names(.channel_categories),
                                         category = as.character(.channel_categories),
                                         stringsAsFactors = FALSE)

        markov_plot <- markov_long %>%
            left_join(.channel_categories, by = "channel_name") %>%
            mutate(category = ifelse(is.na(category), 'Uncategorized', category)) %>%
            mutate(channel_name = fct_reorder(channel_name, removal_value)) %>%
            ggplot(aes(x=channel_name, y=removal_value, fill=category)) +
            geom_col(position = position_dodge(0.9),
                     alpha=0.75) +
            geom_text(aes(label=rt_pretty_axes_percent(removal_value, increase_precision_delta = 0)),
                      position = position_dodge(0.9),
                      size=3.3) +
            coord_flip() +
            scale_fill_manual(values=rt_colors()) +
            scale_y_continuous(breaks=pretty_breaks(10),
                               labels=rt_pretty_axes_percent) +

            theme_light() +
            labs(title="Markov Removal Effects",
                 subtitle = "Shows the estimated percent decrease in conversions from removing specific channels (i.e. the removal effect).",
                 x='Channel',
                 y='Estimated Removal Effect')

        if(length(unique(markov_long$removal_type)) > 1) {

            markov_plot <- markov_plot + facet_wrap(~removal_type)
        }
    }

    return (markov_plot)
}

#' wrapper around ChannelAttribution::markov_model & ChannelAttribution::heuristic_models
#'
#' returns a dataframe with the combined results
#'
#' @param .path_data dataframe expected by ChannelAttribution::markov_model & & ChannelAttribution::heuristic_models
#' @param .path_sequence var_path
#' @param .num_conversions var_conv
#' @param .conversion_value var_value
#' @param .null_conversions var_null
#' @param .order order
#' @param .symbol sep
#' @param .seed seed
#'
#' @importFrom ChannelAttribution heuristic_models
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename inner_join mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace str_ends str_remove
#'
#' @export
rt_get_channel_attribution <- function(.path_data,
                                       .path_sequence='path_sequence',
                                       .num_conversions='num_conversions',
                                       .conversion_value='conversion_value',
                                       .null_conversions='null_conversions',
                                       .order=1,
                                       .symbol='>',
                                       .seed=42) {

    markov_results <- rt_markov_model(.path_data=.path_data,
                                      .path_sequence=.path_sequence,
                                      .num_conversions=.num_conversions,
                                      .conversion_value=.conversion_value,
                                      .null_conversions=.null_conversions,
                                      .order=.order,
                                      .symbol=.symbol,
                                      .seed=.seed)
    markov_attribution <- markov_results$result
    heuristic_attribution <- ChannelAttribution::heuristic_models(Data = .path_data,
                                                                  var_path = .path_sequence,
                                                                  var_conv = .num_conversions,
                                                                  var_value = .conversion_value,
                                                                  #out_more = TRUE,
                                                                  sep=.symbol
                                                                  )
    # if var_value is null, heuristic_models returns column names like "first_touch"
    # if var_value is not null, heuristic_models returns column names like "first_touch_conversions"
    # let's make consistent regardless
    if(is.null(.conversion_value)) {

        colnames(heuristic_attribution) <- str_replace(colnames(heuristic_attribution), 'touch', 'touch_conversions')

        markov_attribution <- markov_attribution %>%
            rename(markov_conversions=total_conversions)
    } else {

        markov_attribution <- markov_attribution %>%
            rename(markov_conversions=total_conversions,
                   markov_value=total_conversion_value)
    }

    all_models <- inner_join(heuristic_attribution, markov_attribution, by = 'channel_name')
    all_models <- rt_attribution_pivot_longer(all_models)

    return (all_models)
}

#' wrapper around ChannelAttribution::markov_model & ChannelAttribution::heuristic_models
#'
#' returns a dataframe with the combined results
#'
#' @param .channel_attribution dataframe returned by rt_get_channel_attribution()
#' @param .channel_categories if provided, colors removal effects by channel categories; named vector with categories as value and channel names as vector names
#' @param .show_values show the attribution values
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate left_join
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_col position_dodge scale_fill_manual theme_light theme labs geom_text aes facet_wrap
#'
#' @export
rt_plot_channel_attribution <- function(.channel_attribution, .channel_categories=NULL, .show_values=TRUE) {

    base_attribution_plot <- function(channel_plot, .show_values) {

        channel_plot <- channel_plot +
            geom_col(position = position_dodge(width = 0.9),
                     alpha=0.75) +
            scale_fill_manual(values=rt_colors()) +
            theme_light() +
            theme(axis.text.x = element_text(angle=45, hjust=1)) +
            labs(y='Conversions',
                 x='Channel Name',
                 fill="Attribution Model")

        if(.show_values) {

            round_values_by <- 0
            channel_plot <- channel_plot +
                geom_text(aes(label=ifelse(attribution_value < 1,
                                           rt_pretty_axes_percent(attribution_value, increase_precision_delta = 0),
                                           round(attribution_value))),
                          position = position_dodge(width = 0.9),
                          angle=90,
                          hjust=1)
        }

        return (channel_plot)
    }

    if(length(unique(.channel_attribution$attribution_type)) == 1) {

        if(is.null(.channel_categories)) {

            channel_plot <- .channel_attribution %>%
                mutate(channel_name = fct_reorder(channel_name, attribution_value, .fun = max, .desc = TRUE)) %>%
                ggplot(aes(x=channel_name, y=attribution_value, fill=attribution_name)) %>%
                base_attribution_plot(.show_values)

        } else {

            .channel_categories <- data.frame(channel_name = names(.channel_categories),
                                              category = as.character(.channel_categories),
                                              stringsAsFactors = FALSE)

            channel_plot <- .channel_attribution %>%
                mutate(channel_name = as.character(channel_name)) %>%
                left_join(.channel_categories, by = "channel_name") %>%
                mutate(category = ifelse(is.na(category), 'Uncategorized', category)) %>%
                mutate(channel_name = fct_reorder(channel_name, attribution_value, .fun = max, .desc = TRUE)) %>%
                ggplot(aes(x=channel_name, y=attribution_value, fill=category)) %>%
                base_attribution_plot(.show_values) +
                facet_wrap( ~ attribution_name)
        }
    } else {

        channel_plot <- .channel_attribution %>%
            mutate(channel_name = fct_reorder(channel_name, attribution_value, .fun = max, .desc = TRUE)) %>%
            ggplot(aes(x=channel_name, y=attribution_value, fill=attribution_name)) %>%
            base_attribution_plot(.show_values) +
            facet_wrap(~ attribution_type, scales = 'free_y')
    }

    return (channel_plot)
}

#' gives each step credit for the number of conversions that resulted from the corresponding path conversions
#'
#' if the path is Facebook -> Facebook -> Facebook -> Instagram -> 2 Conversions, then both Facebook & Instagram get credit for two conversions, regardless how many times they are in the path
#'
#' returns a dataframe with the combined results
#'
#' @param .campaign_data dataframe with columns `id | timestamp | step | num_conversions | conversion_value`
#' @param .conversion_column e.g. num_conversions or conversion_value
#' @param .path_id string identifying the path_id column
#' @param .step string identifying the step column
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by ungroup mutate filter distinct
#' @importFrom tidyr pivot_wider
#'
#' @export
rt_get_any_touch_attribution <- function(.campaign_data,
                                         .conversion_column,
                                         .path_id='.path_id',
                                         .step='step') {

    path_conversions <- .campaign_data %>%
        select(!!sym(.path_id), !!sym(.step), !!sym(.conversion_column)) %>%
        group_by(!!sym(.path_id)) %>%
        mutate(temp___path_conversion = sum(!!sym(.conversion_column))) %>%
        ungroup() %>%
        filter(temp___path_conversion > 0)

    path_conversion_matrix <- path_conversions %>%
        select(-!!sym(.conversion_column)) %>%
        distinct() %>%
        pivot_wider(names_from = !!sym(.step),
                    values_from = temp___path_conversion,
                    values_fill = list(temp___path_conversion = 0)) %>%
        select(-!!sym(.path_id))

    path_conversions <- path_conversions %>%
        select(!!sym(.path_id), temp___path_conversion) %>%
        distinct()

    rt_stopif(any(duplicated(path_conversions$.path_id)))

    stopifnot(all(rowSums(path_conversion_matrix) > 0))
    stopifnot(all.equal(apply(path_conversion_matrix, 1, max), path_conversions$temp___path_conversion))
    # they should equal 2, unless there is a path that had all channels, which isn't the case
    stopifnot(all(apply(path_conversion_matrix, 1, function(x) length(unique(x))) == 2))

    any_touch <- colSums(path_conversion_matrix)
    any_touch <- any_touch / sum(any_touch)

    stopifnot(sum(any_touch) == 1)

    any_touch_df <- data.frame(channel_name=names(any_touch), any_touch=as.numeric(any_touch))

    return (any_touch_df)
}

#' transforms attribution dataframe to long format
#'
#' @param attribution_models dataframe with columns `channel_name | xxx_conversions | xxx_value`
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate select case_when
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_ends str_remove
#'
#' @export
rt_attribution_pivot_longer <- function(attribution_models) {

    attribution_models <- attribution_models %>%
        pivot_longer(colnames(attribution_models) %>% rt_remove_val('channel_name'),
                     names_to = 'attribution_column_name',
                     values_to = 'attribution_value') %>%
        mutate(attribution_type = case_when(
                str_ends(attribution_column_name, '_conversions') ~ 'Conversion',
                str_ends(attribution_column_name, '_value') ~ 'Conversion Value',
                TRUE ~ 'unknown'
            ),
            attribution_name = str_remove(attribution_column_name, "_conversions"),
            attribution_name = str_remove(attribution_name, "_value"),
            attribution_name = rt_pretty_text(attribution_name)) %>%
        select(-attribution_column_name) %>%
        select(channel_name, attribution_name, attribution_type, attribution_value)

    return (attribution_models)
}

#' transforms attribution dataframe to long format
#'
#' @param attribution_models dataframe with columns `channel_name | xxx_conversions | xxx_value`
#'
#' @importFrom magrittr "%>%"
#' @importFrom forcats fct_lump
#' @importFrom networkD3 sankeyNetwork
#'
#'
#'
#' @importFrom dplyr select group_by filter ungroup distinct bind_rows arrange unite mutate summarise desc count rename pull
#' @importFrom stringr str_remove
#'
#' @export
create_sankey <- function(.path_data,
                          .id='entity_id',
                          .path_column='touch_category',
                          .visit_index='touch_index',
                          .global_path_values=NULL,
                          .ending_events=NULL,
                          .ending_event_fill_name='End of Data',
                          .order_by=c('size', 'optimize', 'both'),
                          .depth_threshold=4,
                          .proportion_threshold_other_category=0.01) {

    ############################################################
    # we want to lump categories together if they meet the threshold
    # except we want to retain all .ending_events
    ############################################################
    if(!is.null(.ending_events)) {
        original_path_values <- .path_data[[.path_column]]
    }
    .path_data[[.path_column]] <- as.character(fct_lump(.path_data[[.path_column]],
                                                        prop = .proportion_threshold_other_category,
                                                        ties.method = 'first', ))
    if(!is.null(.ending_events)) {
        # however, want to keep success/ending events
        # so if the original value was an ending event, replace with that event, otherwise keep the
        # post-lumped value
        .path_data[[.path_column]] <- ifelse(original_path_values %in% .ending_events,
                                             original_path_values,
                                             .path_data[[.path_column]])

    }
    ############################################################

    if(!is.null(.ending_events)) {

        # if this isn't null, then
        # 1) anyone who doesn't have a success event has "bounced"
        # 2) anyone who doesn't have anything other than a success event needs an initial filler event
        # 3) if we don't have this value, then the final event will show up multiple
        # these 2 things will make sure everyone is represent from beginning to end.


        # 1) bounced
        bounced_path_data <- .path_data %>%
            group_by(!!sym(.id)) %>%
            filter(!any(!!sym(.path_column) %in% .ending_events)) %>%
            ungroup() %>%
            select(entity_id) %>%
            distinct()
        bounced_path_data[[.path_column]] <- .ending_event_fill_name
        bounced_path_data[[.visit_index]] <- Inf
        # nothing other than success event

        only_success_data <- .path_data %>%
            group_by(!!sym(.id)) %>%
            filter(n() == 1 & all(!!sym(.path_column) %in% .ending_events)) %>%
            ungroup() %>%
            select(entity_id) %>%
            distinct()
        only_success_data[[.path_column]] <- "No Prior Data"
        only_success_data[[.visit_index]] <- -Inf

        .path_data <- .path_data %>%
            bind_rows(bounced_path_data) %>%
            bind_rows(only_success_data) %>%
            arrange(!!sym(.id), !!sym(.visit_index))
    }

    # convert dataset so that it has `source->target` pairs (e.g. visit1 -> visit2; visit2 -> visit3)
    source_target_data <- .path_data %>%
        unite(channel_source, c(!!sym(.path_column), !!sym(.visit_index)), sep = "~~") %>%
        group_by(!!sym(.id)) %>%
        mutate(channel_target = lead(channel_source)) %>%
        ungroup() %>%
        filter(!is.na(channel_target))

    if(!is.null(.ending_events)) {

        original_event_name <- str_remove(source_target_data$channel_target, pattern = "~~.*")
        source_target_data$channel_target <- ifelse(original_event_name %in% .ending_events,
                                                    original_event_name,
                                                    source_target_data$channel_target)

        # we should only check if the user provides us .ending_events value(s), otherwise, all bets are off
        # e.g. might happen if the person only has 1 touch-point (e.g. bounced or converted without any prior touch-points)
        stopifnot(setequal(source_target_data[[.id]], .path_data[[.id]]))
    }

    total_rows <- nrow(source_target_data)

    source_target_data <- source_target_data %>%
        unite(step, c(channel_source, channel_target), remove = FALSE, sep = " -> ") %>%
        group_by(step, channel_source, channel_target) %>%
        summarise(num_touch_points=n(),
                  num_touch_points_distinct=n_distinct(!!sym(.id))) %>%
        ungroup() %>%
        arrange(desc(num_touch_points))
    stop_if_not_identical(source_target_data$num_touch_points, source_target_data$num_touch_points_distinct)
    stop_if_any_duplicated(source_target_data$step)
    stop_if_any_duplicated(source_target_data %>% select(channel_source, channel_target))
    stop_if_not_identical(total_rows, sum(source_target_data$num_touch_points))

    source_target_data <- source_target_data %>% select(-num_touch_points_distinct)

    # TODO: CAN WE CHECK TO MAKE SURE THE COUNT FOR THE ENTRY POINT IS THE SAME AS THE COUNT FOR THE EXIT POINT?
    first_touch <- .path_data %>% filter(touch_index == 1)
    stop_if_any_duplicated(first_touch[[.id]])
    last_touch <- .path_data %>%
        group_by(!!sym(.id)) %>%
        filter(!!sym(.visit_index) == max(!!sym(.visit_index))) %>%
        ungroup()
    stop_if_any_duplicated(last_touch[[.id]])
    stopifnot(nrow(first_touch) == nrow(last_touch))
    # first_touch %>% count(!!sym(.path_column), sort = TRUE)
    # last_touch %>% count(!!sym(.path_column), sort = TRUE)

    if(!is.null(.ending_events)) {
        stopifnot(all(last_touch[[.path_column]] %in% c(.ending_event_fill_name, .ending_events)))
    }

    rt_stopif(nrow(source_target_data) > 200)
    unique_nodes <- bind_rows(source_target_data %>%
                                  count(channel_source, wt=num_touch_points, name = 'num_touch_points') %>%
                                  arrange(num_touch_points) %>%
                                  select(channel_source, num_touch_points) %>%
                                  rename(channel_name=channel_source),
                              source_target_data %>%
                                  count(channel_target, wt=num_touch_points, name = 'num_touch_points') %>%
                                  arrange(num_touch_points) %>%
                                  select(channel_target, num_touch_points) %>%
                                  rename(channel_name=channel_target)) %>%
        count(channel_name, wt=num_touch_points, name = 'num_touch_points') %>%
        arrange(desc(num_touch_points)) %>%
        pull(channel_name)

    source_indexes <- match(source_target_data$channel_source, unique_nodes) - 1
    target_indexes <- match(source_target_data$channel_target, unique_nodes) - 1

    source_target_data$source <- source_indexes
    source_target_data$target <- target_indexes

    unique_nodes <- str_remove(string=unique_nodes, pattern = "~~.*")
    sankey_nodes_df <- data.frame(name=c(unique_nodes))

    #stopifnot(all(unique_nodes %in% names(color_dictionary)))
    # color_dictionary <- c(color_dictionary,
    #                       c("Joined Experiment"=rt_colors_good_bad()[1], "No Further Visits"=rt_colors_good_bad()[2],
    #                         "Other"=rt_colors(color_names = 'dove_gray')))
    color_dictionary <- rt_colors()[1:length(.global_path_values)]
    names(color_dictionary) <- .global_path_values
    rt_stopif(any(duplicated(names(color_dictionary))))
    rt_stopif(any(duplicated(as.character(color_dictionary))))

    selected_colors <- as.character(color_dictionary[unique_nodes])

    color_string <- rt_str_collapse(unique(selected_colors),.surround = '"', .separate = ", ")
    ColourScal <- paste0('d3.scaleOrdinal().range([', color_string,'])')
    rt_stopif(nrow(source_target_data) > 200)


    stopifnot(all(.order_by %in% c('size', 'optimize', 'both')))
    .order_by <- .order_by[1]
    sankey_plots <- list()
    if(.order_by %in% c('size', 'both')) {

        sankey_plot <- sankeyNetwork(Links = source_target_data %>% as.data.frame(),
                                                Nodes = sankey_nodes_df %>% as.data.frame(),
                                                Source = 'source',
                                                Target = 'target',
                                                Value = 'num_touch_points',
                                                NodeID = 'name',
                                                iterations=0,  # forces the cells in the diagram to appear in order of size
                                                colourScale = ColourScal,
                                                #units = 'TWh',
                                                fontSize = 12, nodeWidth = 30)
        sankey_plots <- append_list(sankey_plots, sankey_plot)
    }

    if(.order_by %in% c('optimize', 'both')) {

        # save an alternative image where we do not force the order of the items
        sankey_plot <- sankeyNetwork(Links = source_target_data %>% as.data.frame(),
                                                Nodes = sankey_nodes_df %>% as.data.frame(),
                                                Source = 'source',
                                                Target = 'target',
                                                Value = 'num_touch_points',
                                                NodeID = 'name',
                                                #iterations=0,
                                                colourScale = ColourScal,
                                                #units = 'TWh',
                                                fontSize = 12, nodeWidth = 30)
        sankey_plots <- append_list(sankey_plots, sankey_plot)
    }

    return (sankey_plots[!is.na(sankey_plots)])
}
