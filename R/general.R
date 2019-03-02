#' returns whether or not a value is NULL, NA, or NAN
#'
#' @param date_vector a vector of dates
#' @param reference_date the reference/base date used for fields such as (is_current_xxx). If NULL (default), then today's date is used.
#'
#' @examples
#'
#' date_vector <- lubridate::as_date('2018-01-01') + seq(0, 400)
#' rt_get_date_fields(date_vector)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate select
#' @importFrom lubridate year quarter month week day yday as_date
#' @importFrom purrr map_int map
#' @importFrom stringr str_pad
#' @importFrom timeDate holiday USNewYearsDay USMLKingsBirthday USWashingtonsBirthday USMemorialDay USIndependenceDay USLaborDay USColumbusDay USElectionDay USVeteransDay USThanksgivingDay USChristmasDay
#' @export
rt_get_date_fields <- function(date_vector, reference_date=NULL) {

    date_vector <- as_date(date_vector)
    if(is.null(reference_date)) {

        reference_date <- Sys.Date()

    } else {

        reference_date <- lubridate::as_date(reference_date)
    }

    min_year <- min(year(date_vector), na.rm = TRUE)
    max_year <- max(year(date_vector), na.rm = TRUE)

    day_lookup <- c(1, 2, 3, 4, 5, 6, 7)
    month_names <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
                      'October', 'November', 'December')
    names_days_of_the_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
    names(day_lookup) <- names_days_of_the_week

    #library(timeDate)
    us_federal_holiday_names <- c('USNewYearsDay', 'USMLKingsBirthday', 'USWashingtonsBirthday', 'USMemorialDay',
                                  'USIndependenceDay', 'USLaborDay', 'USColumbusDay', 'USElectionDay',
                                  'USVeteransDay', 'USThanksgivingDay', 'USChristmasDay')
    us_federal_holiday_dates <- timeDate::holiday(min_year:max_year, us_federal_holiday_names)

    return (
        data.frame(
        year = year(date_vector),
        quarter = quarter(date_vector),
        month = month(date_vector),
        week_of_year = week(date_vector),
        day_of_month = day(date_vector),
        month_name = factor(months(date_vector), levels=month_names, ordered=TRUE),
        day_name = factor(weekdays(date_vector), levels=names_days_of_the_week, ordered=TRUE),
        day_of_year = yday(date_vector)) %>%
            mutate(week_of_month = ceiling(day_of_month / 7),
                   is_current_year = year == year(reference_date),
                   is_current_quarter = ifelse(is.na(date_vector),
                                               NA,
                                               paste(year, quarter) == paste(year(reference_date),
                                                                             quarter(reference_date))),
                   is_current_month = ifelse(is.na(date_vector),
                                             NA,
                                             paste(year, month) == paste(year(reference_date),
                                                                         month(reference_date))),
                   is_month_quarter_start = month %% 3 == 1,
                   is_month_quarter_end = month %% 3 == 0,
                   week_day_number = map_int(day_name, ~ as.integer(day_lookup[as.character(.)])),
                   is_weekend = week_day_number - 6 >= 0,
                   is_holiday = ifelse(is.na(date_vector),
                                       NA,
                                       date_vector %in% as_date(us_federal_holiday_dates)),
                   is_first_of_month=day(date_vector) == 1,
                   is_end_of_month=day((date_vector + 1)) == 1,
                   is_end_of_quarter = is_end_of_month & is_month_quarter_end,
                   is_year_start = ifelse(is.na(month(date_vector)) | is.na(day(date_vector)),
                                          NA,
                                          month(date_vector) == 1 & day(date_vector) == 1),
                   is_year_end = ifelse(is.na(month(date_vector)) | is.na(day(date_vector)),
                                          NA,
                                          month(date_vector) == 12 & day(date_vector) == 31),
                   cohort_week = factor(ifelse(is.na(year) | is.na(week_of_year),
                                               NA,
                                               paste0(year, '-W', str_pad(week_of_year,
                                                                          width= 2,
                                                                          side='left',
                                                                          pad='0'))),
                                        ordered=TRUE),
                   cohort_month = factor(paste0(year, '-', substr(month_name, start = 1, stop = 3)),
                                         levels=unlist(map(min_year:max_year,
                                                           ~ paste0(., '-', substr(month_names, 1, 3)))),
                                         ordered=TRUE),
                   cohort_quarter = factor(ifelse(is.na(year) | is.na(quarter),
                                                  NA,
                                                  paste0(year, '-Q', quarter)),
                                           ordered=TRUE)
                   ) %>%
            select(year, quarter, month, day_of_month, week_day_number, day_of_year, week_of_year,
                   week_of_month, month_name, day_name, is_current_year, is_current_quarter, is_current_month,
                   is_weekend, is_holiday, is_month_quarter_start, is_month_quarter_end, is_first_of_month,
                   is_end_of_month, is_end_of_quarter, is_year_start, is_year_end, cohort_week, cohort_month,
                   cohort_quarter)
    )
}

#' returns a dataframe's column as a vector
#'
#' @param df a data.frame
#' @param column the column to return as a vector
#' @param return_unique if TRUE, return unique values
#'
#' @export
rt_get_vector <- function(df, column, return_unique=FALSE) {

    if(return_unique) {

        return (unique(df[[column]]))
    
    } else {

        return (df[[column]])
    }
}

#' returns the vector (`vec`) without the specified value (`val`). If `val` doesn't exist in `vec`, `vec` is
#' returned unchanged.
#'
#' @param vec the vector
#' @param val the value to remove
#'
#' @export
rt_remove_val <- function(vec, val) {

    return (vec[!vec %in% val])
}


#' Returnes the ceiling of the **absolute value** of `y`, rounded to the nearest_x.
#'
#' @param y the value
#' @param nearest_x the decimal value to round the ceiling to
#'
#' @export
rt_ceiling_nearest_x <- function(y, nearest_x) {

    y_trans <- ceiling(abs(y)/nearest_x)*nearest_x
    if(y < 0) {
        y_trans <- y_trans * -1
    }
    # round to nearest 10 because computers can't handle decimals
    return (round(y_trans, 10))
}

#' Like `stopifnot`, but stop `stopifnot` stops if the expression is not true, and `rt_stopif` stops if the
#' expression is true. Avoids the unintuitive double-negative e.g. (`stopifnot(!espression)`) which becomes
#' `rt_stopif(espression)`.
#'
#' @param exprs the expression
#'
#' @export
rt_stopif <- function(exprs) {

    stopifnot(!exprs)
}
