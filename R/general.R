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

#' returns numeric value of difftime (date_last - date_first)
#'
#' @param date_last the date to subtract from
#' @param date_first the date to subtract
#' @param units same values of difftime, defaults to `days`
#'
#' @export
rt_difftime_numeric <- function(date_last, date_first, units='days') {
    rt_stopif(length(units) > 1)
    rt_stopif(units == 'auto')  # since we are returning a numeric, we wouldn't know the units if using auto
    return (as.numeric(difftime(date_last, date_first, units=units)))
}

#' returns an ordered factor of the date vector
#'
#' date_floor: `week` gives `YYYY-MM-DD` with the `Monday` as the date when week_start is `1`
#' date_floor: `month` gives `YYYY-MM`
#' date_floor: `quarter` gives `YYYY-QX`
#' date_floor: `year` gives `YYYY`
#'
#' @param date_vector the date vector
#' @param date_floor `week`, `month`, `quarter`, `year`
#' @param week_start same values of floor_date, defaults to `1` which starts the week on Monday
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate floor_date
#' @importFrom stringr str_replace
#'
#' @export
rt_floor_date_factor <- function(date_vector, date_floor='week', week_start=1) {

    date_vector <- as.character(floor_date(x=date_vector, unit=date_floor, week_start=week_start))

    if(date_floor == 'month') {

        date_vector <- substr(date_vector, 1, 7)

    } else if (date_floor == 'quarter') {

        date_vector <- date_vector %>%
            str_replace('-01-01', '-Q1') %>%
            str_replace('-04-01', '-Q2') %>%
            str_replace('-07-01', '-Q3') %>%
            str_replace('-10-01', '-Q4')

    } else if(date_floor == 'year') {

        date_vector <- substr(date_vector, 1, 4)

    }

    date_vector <- factor(date_vector, levels = sort(unique(date_vector)), ordered = TRUE)

    return (date_vector)
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

#' Hex colors vector
#'
#' @param color_names filter by the names of the colors
#' @param sets filter by the set index
#' @param return_named_vector returns the colors as a named vector
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter
#' @importFrom tibble tribble

#' @export
rt_colors <- function(color_names=NULL, sets=NULL, return_named_vector=FALSE) {
    color_df <- tribble(
        ~set, ~name, ~hex,
        1, "pastel_blue", '#7AA9CF',
        1, "tuplip_tree", '#EBB13E',
        1, "custom_green" , '#41B3A3',
        1, "crail", '#A65B50',
        1, "dove_gray", '#7A7A7A',
        1, "custom_purple", '#C38D9E',
        1, "cadmium_orange", '#F28E2B',

        2, "mariner", '#4E79A7',
        2, "sandstone", '#F4CC70',
        2, "avocado", '#258039',
        2, "tomato", '#CF3721',
        2, "mist", '#90AFC5',
        2, "lavender", '#C396E8',
        2, "tree_poppy", '#FD9126',

        3, "cerulean", '#1EB1ED',
        3, "yellow_pepper", '#F5BE41',
        3, "pigment_green", '#1AAF54',
        3, "poppy", '#FF420E',
        3, "loblolly_gray", '#B4B7B9',
        3, "lavendar2", '#6C648B',
        3, "flamingo", '#FC641F',

        4, "forest", '#1E434C',
        4, "gold", '#C99E10',
        4, "sunflower", '#3F681C',
        4, "crimson", '#8D230F',
        4, "granite", '#B7B8B6',
        4, "vivid_violet", '#932791',
        4, "petal", '#F98866',

        5, "black_shadow", '#2A3132',
        5, "sky", '#375E97',
        5, "medium_sea_green", '#37B57F',
        5, "red_clay", '#A43820',

        #6, "mint", '#3EB480',
        6, "mandy", '#DF585C'
        # "coffee", '#B38867',
        # "turquoise", '#5BC8AC',
        #
        #
        # "custom_red", '#E27D60',
        #
        # "sunset", '#FB6542',
        # "stem", '#80BD9E',
        # "spring_green", '#89DA59',
        # "automn_foliage", '#763626',
        # "stone", '#336B87',
        #
        # "pink_tulip", '#F18D9E',
        # "blue_sky", '#4CB5F5',
        # "aqua_blue", '#31A9B8',
        #
        # "red_valencia", '#E15759',
        # "summer_sky", '#40C6EE',
        # "yale_blue", '#11499C',
        # "blue_de_france", '#4286E8',
        #
        # "shamrock", '#5FECA6',
        # "astronaut", '#283676',
        # "well_read", '#BB3A34',
        # "blue_chill", '#159192',
        #
        # "custom_blue", '#085DCB',
        # "custom_orange", '#E8A87C'
    )

    rt_stopif(any(duplicated(color_df$name)))
    rt_stopif(any(duplicated(color_df$hex)))

    # blue, yellow, green, red, gray, purple, orange,
    # color_df <- color_df[1:7, ]

    if(!is.null(color_names)) {
        color_df <- color_df %>% filter(name %in% color_names)
        color_df <- color_df[match(color_names, color_df$name),]
    }

    if(!is.null(sets)) {
        color_df <- color_df %>% filter(set %in% sets)
    }

    colors <- color_df$hex

    if(return_named_vector) {
        names(colors) <- color_df$name
    }
    return (colors)
}

#' Hex colors vector representing "good" (green) and "bad" (red)
#'
#' @param good_first if `TRUE` then index 1 is the hex color corresponding to `good` and
#'  index 2 corresponds to bad, otherwise the reverse.
#'
#' @export
rt_colors_good_bad <- function(good_first=TRUE) {
    good <- 'medium_sea_green'
    bad <- 'mandy'
    if(good_first) {
        custom_color_names <- c(good, bad)
    } else {
        custom_color_names <- c(bad, good)
    }
    return (rt_colors(color_names=custom_color_names))
}

#' Plots the colors from `rt_colors()`
#'
#' @param color_names filter by the names of the colors
#' @param sets filter by the set index
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual theme_light theme element_blank coord_flip

#' @export
rt_plot_colors <- function(color_names=NULL, sets=NULL) {
    custom_colors <- rt_colors(color_names=color_names, sets=sets, return_named_vector=TRUE)

    factor_names <- names(custom_colors)
    factor_names <- rev(names(custom_colors))
    colors_df <- data.frame(name=names(custom_colors),
                   hex=custom_colors,
                   value=1,
                   stringsAsFactors = FALSE) %>%
            mutate(name = factor(name, levels=factor_names))

    colors_df %>%
        ggplot(aes(x=name, y=value, fill=name)) +
        geom_col() +
        scale_fill_manual(values=rev(custom_colors)) +
        theme_light() +
        theme(legend.position = 'none',
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        coord_flip()
}

#' Returns the colors based on the alphabetical order of the values. E.g. if you plot a graph alphabetically
#' and then plot it based on it's factor order the colors should be retained for each category.
#'
#' @param x a character or factor vector
#'
#' @export
rt_get_colors_from_values <- function(x) {

    custom_colors <- c(rt_colors(), rt_colors(), rt_colors())
    sorted_values <- sort(as.character(unique(x)))

    stopifnot(length(custom_colors) >= length(sorted_values))

    if(is.character(x) || is.logical(x)) {

        return (custom_colors[1:length(sorted_values)])

    } else if (is.factor(x)) {

        found_values <- as.character(sort(unique(x)))
        color_index <- match(found_values, sorted_values)
        return (custom_colors[color_index])
    } else {

        stopifnot(FALSE)
    }
}
