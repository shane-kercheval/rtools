test_helper__column_names <- function(.df) {
    return (paste(rt_pretty_text(colnames(.df)), 'Col'))
}

test_helper_transform_campaign_data <- function(.campaign_data) {
    .campaign_data %>%
        select(-interaction) %>%
        mutate(step_type = ifelse(channel == 'Facebook' | channel == 'Instagram', "Social", "Other")) %>%
        rename(id=cookie,
               timestamp=time,
               step=channel,
               num_conversions = conversion) %>%
        select(id, timestamp, step, step_type, num_conversions, conversion_value) %>%
        arrange(id, timestamp)
}

test_helper__campaign_filter_first_conversions <- function(.campaign_data) {
    suppressWarnings(.campaign_data %>%
                         #campaign_data %>% arrange(id, timestamp, conversion_value) %>%
                         group_by(id) %>%
                         filter(all(num_conversions == 0) | timestamp <= min(timestamp[num_conversions > 0])) %>%
                         ungroup() %>%
                         arrange(id, timestamp, conversion_value, step))
}

