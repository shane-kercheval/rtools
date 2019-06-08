#' returns whether or not a value is NULL, NA, or NAN
#'
#' @param x a single value to check
#'
#' @examples
#'
#' rt_is_null_na_nan(NA)
#'
#' @export
rt_is_null_na_nan <- function(x) {
    return (is.null(x) || is.na(x) || is.nan(x))
}

#' compares two dataframes and returns TRUE if they are both equal
#'
#' @param dataframe1 a dataframe to compare
#' @param dataframe2 a dataframe to compare
#'
#' @export
rt_are_dataframes_equal <- function(dataframe1, dataframe2) {

    nrows_df1 <- nrow(dataframe1)
    nrows_df2 <- nrow(dataframe2)

    if((is.null(nrows_df1) && !is.null(nrows_df2)) ||
       (!is.null(nrows_df1) && is.null(nrows_df2)) ||
       nrows_df1 != nrows_df2) {
        return (FALSE)
    }

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

#' returns TRUE if `set1` is a subset of `set2`. Returns `FALSE` if either `set1` or `set2` is `NULL` (even
#' if they are both NULL). The reason for this is because it is used to test sets, and more often then not,
#' NULL values when testing sets is indicative of a bug.
#'
#' @param set1 the set that is a subset
#' @param set2 the set that is the superset
#'
#' @export
rt_setsubset <- function(set1, set2) {
    if(is.null(set1) || is.null(set2)) {

        return (FALSE)
    }

    return (all(set1 %in% set2))
}
