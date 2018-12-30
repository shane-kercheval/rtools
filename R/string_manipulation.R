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

        dataset[, column] <- rt_pretty_text(dataset[, column])
    }

    colnames(dataset) <- rt_pretty_text(colnames(dataset))

    return (dataset)
}

#' Formats numeric values
#'
#' @param values a numeric vector
#'
#' @examples
#'
#' rt_pretty_numerics(rnorm(n=10, mean=0, sd=0.001))
#' rt_pretty_numerics(rnorm(n=10, mean=1000000, sd=100000))
#'
#' @export
rt_pretty_numerics <- function(values) {

    if(max(values) > 1000000) {

        values <- paste0(round(values / 1000000, 2), 'M')

    } else if(max(values) > 100000) {

        values <- paste0(round(values / 1000, 1), 'K')

    } else if(max(values) > 10000) {

        values <- paste0(round(values / 1000, 1), 'K')

    } else if(max(values) > 1000) {

        values <- paste0(round(values / 1000, 2), 'K')

    } else if(max(values) > 100) {

        values <- round(values, 0)

    } else if(max(values) > 1) {

        values <- round(values, 1)

    } else if(max(values) > 0.1) {

        values <- round(values, 2)

    } else {

        values <- formatC(values, format = "e", digits = 2)
    }

    return (values)
}
