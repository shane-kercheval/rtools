#' compares two dataframes and returns TRUE if they are both equal
#'
#' @param dataframe1 a dataframe to compare
#' @param dataframe2 a dataframe to compare
#'
#' @export
rt_are_dataframes_equal <- function(dataframe1, dataframe2) {
    return (all(rownames(dataframe1) == rownames(dataframe2)) &&
                all(colnames(dataframe1) == colnames(dataframe2)) &&
                # if either df1 or df2 is NA, then both should be NA
                all(ifelse(is.na(dataframe1) | is.na(dataframe2),
                       is.na(dataframe1) & is.na(dataframe2),
                       dataframe1 == dataframe2)))
}

#' compares two dataframes, one of them will be loaded from the `file`
#'
#' @param dataframe1 a dataframe to compare
#' @param rds_file a file c
#'
#' @export
rt_are_dataframes_equal_from_file <- function(dataframe1, rds_file) {
    # saveRDS(object = dataframe1, file = rds_file)
    other = readRDS(file=rds_file)
    return (rt_are_dataframes_equal(dataframe1=dataframe1, dataframe2=other))
}

#' deletes the file/plot if it exists, saves the plot, and ensures it exists
#'
#' @param file_name path to save plot
#' @param plot plot to save
#' @param size_inches c(height, width) in inches
#'
#' @importFrom ggplot2 ggsave
#' @export
test_save_plot <- function(file_name, plot, size_inches=c(5, 8)) {

    if (file.exists(file_name)) file.remove(file_name)
    ggsave(filename=file_name, plot=plot, height=size_inches[1], width=size_inches[2], units='in')
    expect_true(file.exists(file_name))
}
