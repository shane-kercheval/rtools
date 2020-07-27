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

test_helper__campaign_add_conversions <- function(.campaign_data) {
    # make 1st and 2nd events have >0 conversions
    .campaign_data[1, c('num_conversions', 'conversion_value')] <- 1
    .campaign_data[2, c('num_conversions', 'conversion_value')] <- 2
    # make 1st and 2nd events have >0 conversions
    .campaign_data[5, c('num_conversions', 'conversion_value')] <- 2
    .campaign_data[6, c('num_conversions', 'conversion_value')] <- 2
    # make 2nd and 3rd events have >0 conversions
    .campaign_data[12, c('num_conversions', 'conversion_value')] <- 2
    .campaign_data[13, c('num_conversions', 'conversion_value')] <- 2
    .campaign_data[14, c('num_conversions', 'conversion_value')] <- 1

    return (.campaign_data)
}

test_helper__save_sankey_plot <- function(.sankey_plot, .file_name) {

    networkD3::saveNetwork(.sankey_plot, paste0(.file_name, ".html"))
    # note you have to install the package and run "install_phantomjs()"
    #install.packages('webshot')
    #webshot::install_phantomjs()
    #library(webshot)
    webshot::webshot(url = paste0(.file_name, ".html"), file=paste0(.file_name, ".png"))
}
