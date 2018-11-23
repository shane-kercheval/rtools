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
