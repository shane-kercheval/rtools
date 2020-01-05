#' Changes strings to "pretty" strings. Ignores other datatypes else.
#'
#' @param values a string vector
#'
#' @examples
#'
#' library(ggplot2)
#' rt_pretty_text(c('abc', 'ABC', 'abc/xyz'))
#'
#' @importFrom stringr str_replace_all str_split
#' @importFrom purrr map_chr
#' @export
rt_pretty_text <- function(values) {

    if(all(is.na(values)) || (!is.character(values) && !is.factor(values))) {

        return (values)

    } else {
        factor_levels <- NULL
        if(is.factor(values)) {

            factor_levels <- levels(values)
            factor_levels <- rt_pretty_text(factor_levels)
            factor_ordered <- is.ordered(values)
        }

        values <- str_replace_all(values, '_', ' ')
        values <- str_replace_all(values, '(?<=\\S)(?=\\S)\\/(?=\\S)', ' / ')  # `x/y`` -> `x / y``
        #values <- str_replace_all(values, '(?=\\S)\\/(?!\\s)'    , ' / ')  # `x/y`` -> `x / y``

        words <- str_split(values, ' ')
        # change first character to upper case, leave the rest of the characters
        # this is so when words are intentially all caps (e.g. acronyms), they don't get converted to camel
        values <- map_chr(words, ~ {
            word <- NA
            if(!all(is.na(.))) {

                word <- paste(toupper(substring(., 1,1)), substring(., 2), sep='', collapse=' ')
            }
            return (word)
        })

        if(!is.null(factor_levels)) {

            values <- factor(values, levels=factor_levels, ordered=factor_ordered)
        }

        return (values)
    }
}

#' Changes string column values and column names to "pretty" strings. Ignores other datatypes else.
#'
#' @param dataset a dataframe to make pretty
#'
#' @examples
#'
#' library(ggplot2)
#' rt_pretty_text(c('abc', 'ABC', 'abc/xyz'))
#'
#' @importFrom stringr str_replace_all str_split
#' @importFrom purrr map_chr
#' @export
rt_pretty_dataset <- function(dataset) {


    for(column in colnames(dataset)) {

        dataset[[column]] <- rt_pretty_text(dataset[[column]])
    }

    colnames(dataset) <- rt_pretty_text(colnames(dataset))

    return (dataset)
}


#' wrapper for base `prettyNum`
#'
#' @param values a numeric vector
#' @param use_na if TRUE keep NAs, else return "NA"
#'
#' @importFrom stringr str_trim
#' @export
rt_prettyNum <- function(values, use_na=TRUE) {

    values <- str_trim(prettyNum(values, big.mark = ",", drop0trailing = TRUE))

    if(use_na) {

        values <- ifelse(values == "NA", NA, values)
    }

    return (values)
}

#' Formats numeric values
#'
#' @param values a numeric vector
#'
#' @examples
#'
#' rt_pretty_numbers_short(rnorm(n=10, mean=0, sd=0.001))
#' rt_pretty_numbers_short(rnorm(n=10, mean=1000000, sd=100000))
#'
#' @export
rt_pretty_numbers_short <- function(values) {

    if(max(abs(values), na.rm = TRUE) > 1000000000) {

        values <- paste0(rt_prettyNum(round(values / 1000000000, 2)), 'B')

    } else if(max(abs(values), na.rm = TRUE) > 1000000) {

        values <- paste0(rt_prettyNum(round(values / 1000000, 2)), 'M')

    } else if(max(abs(values), na.rm = TRUE) > 100000) {

        values <- paste0(rt_prettyNum(round(values / 1000, 1)), 'K')

    } else if(max(abs(values), na.rm = TRUE) > 10000) {

        values <- paste0(rt_prettyNum(round(values / 1000, 1)), 'K')

    } else if(max(abs(values), na.rm = TRUE) > 1000) {

        values <- paste0(rt_prettyNum(round(values / 1000, 2)), 'K')

    } else if(max(abs(values), na.rm = TRUE) > 100) {

        values <- rt_prettyNum(round(values, 0))

    } else if(max(abs(values), na.rm = TRUE) > 1) {

        any_has_decimal <- any(values%%1 != 0, na.rm = TRUE)

        if(any_has_decimal) {

            values <- rt_prettyNum(round(values, 1))

        } else {

            values <- as.character(values)
        }
    } else if(max(abs(values), na.rm = TRUE) > 0.1) {

        values <- rt_prettyNum(round(values, 2))

    } else if(max(abs(values), na.rm = TRUE) > 0.01) {

        values <- rt_prettyNum(round(values, 3))

    } else if(max(abs(values), na.rm = TRUE) > 0.001) {

        values <- rt_prettyNum(round(values, 4))

    } else if(all(values == 0, na.rm = TRUE)) {

        values <- ifelse(is.na(values), NA, "0")

    } else {

        values <- formatC(values, format = "e", digits = 2)
    }

    return (values)
}

#' Formats numeric values
#'
#' @param values a numeric vector
#'
#' @importFrom scales label_comma
#' @export
rt_pretty_numbers_long <- function(values) {

    if(max(abs(values), na.rm = TRUE) > 100) {

        values <- rt_prettyNum(round(values, 0))

    } else {

        values <- rt_pretty_numbers_short(values)
    }

    return (values)
}

#' formats a percent
#'
#' @param values numeric values
#' @param use_na if TRUE keep NAs, else return "NA"
#'
#' @importFrom scales label_percent
#' @export
rt_pretty_percent <- function(values, use_na=TRUE) {

    values <- paste0(rt_pretty_numbers_long(values * 100), "%")

    if(use_na) {

        values <- ifelse(values == "NA%", NA, values)
    } else {
        values <- ifelse(values == "NA%", "NA", values)
    }

    return (values)
}
