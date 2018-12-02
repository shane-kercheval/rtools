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
#' @importFrom purrr map_int
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

    weekday_lookup <- c(1, 2, 3, 4, 5, 6, 7)
    names_days_of_the_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
    names(weekday_lookup) <- names_days_of_the_week

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
        day_of_week = factor(weekdays(date_vector), levels=names_days_of_the_week),
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
                   week_day_number = map_int(day_of_week, ~ as.integer(weekday_lookup[as.character(.)])),
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
                                          month(date_vector) == 12 & day(date_vector) == 31)
                   ) %>%
            select(year, quarter, month, week_of_year, week_of_month, day_of_month, day_of_week, week_day_number, day_of_year,
                   is_current_year, is_current_quarter, is_current_month, is_weekend, is_holiday, is_month_quarter_start,
                   is_month_quarter_end, is_first_of_month, is_end_of_month, is_end_of_quarter, is_year_start,
                   is_year_end)
    )
}
