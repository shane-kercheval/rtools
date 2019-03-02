
#' deletes the file/plot if it exists, saves the plot, and ensures it exists
#'
#' @param file_name path to save plot
#' @param plot plot to save
#' @param size_inches c(height, width) in inches
#'
#' @importFrom ggplot2 ggsave
#' @export
test_save_plot <- function(file_name, plot, size_inches=c(5, 8)) {

    stopifnot(!is.null(plot))

    if (file.exists(file_name)) file.remove(file_name)

    ggsave(filename=file_name, plot=plot, height=size_inches[1], width=size_inches[2], units='in')
    expect_true(file.exists(file_name))
}
