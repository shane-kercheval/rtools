#' determines if a time-series dataset contains a single variable (as opposed to multiple variables)
#'
#' @param x a time series dataset
#'
#' library(fpp2)
#' rt_ts_is_single_variable(a10)
#'
#' @export
rt_ts_is_single_variable <- function(x) {

    classes <- class(x)
    return (length(classes) == 1 && classes == 'ts')
}

#' determines if a time-series dataset contains a single variable (as opposed to multiple variables)
#'
#' @param x a time series dataset
#'
#' library(fpp2)
#' rt_ts_is_multi_variable(melsyd)
#'
#' @export
rt_ts_is_multi_variable <- function(x) {

    classes <- class(x)
    return (any(classes == 'mts') || any(classes == 'msts'))
}

#' rather than e.g. 2007.333 for May of 2007 that `time()` returns, this returns `2007.01`

#'
#' @param ts_values a time series dataset
#'
#' library(fpp2)
#' get_time_period(a10)
#'
#' @export
rt_ts_get_time_period <- function(ts_values) {

    values <- time(ts_values) %>% as.character()
    splits <- str_split(values, pattern='\\.', simplify = TRUE)

    if(ncol(splits) == 2) {
        value_periods <- as.numeric(as.factor(splits[,2]))
        max_freq_width <- max(nchar(as.character(value_periods)))

        values <- paste0(splits[, 1],
                         '.',
                         str_pad(string=as.numeric(as.factor(splits[,2])), width=max_freq_width, side='left', pad='0'))
    }

    return (as.numeric(values))
}

#' returns a forumla string to pass `tslm()``
#'
#' @param dependent_variable name of the depedent variable in the dataset
#' @param independent_variables name of the indepedent variables in the dataset (including "trend" & "season" if applicable)
#' @param interaction_variables not supported yet
#' @param ex_ante_forecast_horizon is used when the regression model will be used to forecast future values, `ex_ante_forecast_horizon` specifies the number periods (i.e. horizon) that will be forecasted.
#'
#' Ex ante means the forecast will be a "true" forecast and, in this case, will only use lagged values that are lagged far enough back, that we can use them to predict into the future. (and/or trend/season variables)
#' For example, if we want to lag 4 periods (e.g. quarters) ahead, we must use values that are lagged 4 periods behind.
#'
#' Lagged variables must be in the for of `x_lag_y`, where `x` is the original variable name and `y` is the lag number.
#'
#' If `ex_ante_forecast_horizon` is set, only variables that are in the form of `x_lag_y` will be included, and additionally, only lagged variables where `y` >= `ex_ante_forecast_horizon` will be included.
#'
#' All independent variables are included of `ex_ante_forecast_horizon` is `NULL`
#' @examples
#'
#' rt_ts_lm_build_formula(dependent_variable = 'dataset',
#'                        independent_variables = c('trend', 'season'))
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_split
#' @export
rt_ts_lm_build_formula <- function(dependent_variable,
                                   independent_variables=NULL,
                                   interaction_variables=NULL,
                                   ex_ante_forecast_horizon=NULL) {
    # ex_ante_forecast_horizon specifies the number periods (i.e. horizon) we will be forecasting
    # this ex_ante (i.e. true forecast) implementation requires the lags included in the regression model
    # to be >= the ex_ante_forecast_horizon.... e.g. cannot have lag 5 included if we are predicting 10 periods ahead, or lag 9
    # all independent variables are included of ex_ante_forecast_horizon is NULL

    if(!is.null(ex_ante_forecast_horizon)) {

        splits <- str_split(independent_variables, '_lag_', simplify=TRUE)
        if(ncol(splits) == 1) { # no matches/lags found

            valid_lags <- rep(FALSE, length(independent_variables))  # all invalid

        } else {

            lag_number <- as.numeric(splits[, 2])
            lag_number[is.na(lag_number)] <- 0  # NA means not a lag
            valid_lags <- lag_number >= ex_ante_forecast_horizon
        }

        keep_vars <- independent_variables %in% c('trend', 'season') | valid_lags
        independent_variables <- independent_variables[keep_vars]
    }

    if(is.null(interaction_variables)) {

        interaction_variables_formula <- ''

    } else {

        interaction_variables_formula <- paste(map_chr(interaction_variables, ~ paste(., collapse =' * ')),
                                               collapse = ' + ')
    }

    if(is.null(independent_variables) || length(independent_variables) == 0) {

        independent_variables_formula <- interaction_variables_formula

    } else if(is.null(interaction_variables) || length(interaction_variables) == 0) {

        independent_variables_formula <- paste(independent_variables, collapse =' + ')

    } else {

        independent_variables_formula <- paste(interaction_variables_formula,
                                               '+',
                                               paste(independent_variables, collapse =' + '))
    }

    return (paste(dependent_variable, '~', independent_variables_formula))
}

#' returns a new dataset containing the lagged values for specified variables
#'
#' @param dataset a time-series dataset (single- or multi-variable)
#' @param num_lags the number of lags to include (e.g. `3` will include 3 lagged variables for each applicable variable, `x_lag_1`, `x_lag_2`, `x_lag_3`)
#' @param lag_variables the variables (i.e. columns) to include lagged values for (the original column will be retained). A value of `NULL` is used for single-variable datasets (i.e. ts datasets that have no columns) or, for multi-variable datasets, will create lagged variables for *all* variables in the dataset.
#'
#' All variables that are not specified in `lag_variables` are removed.
#'
#' @param keep_variables the variables to retain in the dataset which are not specified in `lag_variables`. A value of `NULL` is used for single-variable datasets.
#'
#' @importFrom stats lag
#' @export
rt_ts_create_lagged_dataset <- function(dataset, num_lags=1, lag_variables=NULL, keep_variables=NULL) {

    lagged_dataset <- NULL
    new_columns <- NULL

    if(rt_ts_is_single_variable(dataset)) {

        lagged_dataset <- dataset
        new_columns <- 'original_data'

        for(lag_index in 1:num_lags) {

            lagged_dataset <- cbind(lagged_dataset, stats::lag(dataset, lag_index * -1))
            new_columns <- c(new_columns, paste0('data_lag_', lag_index))
        }

    } else {
        if(is.null(lag_variables)) {
            lag_variables <- colnames(dataset)
        }

        new_columns <- NULL

        if(!is.null(keep_variables)) {

            # add variables/columns we want to keep
            for(column in keep_variables) {

                lagged_dataset <- cbind(lagged_dataset, dataset[, column])
                new_columns <- c(new_columns, column)
            }
        }

        for(column in lag_variables) {

            # add original column
            lagged_dataset <- cbind(lagged_dataset, dataset[, column])
            new_columns <- c(new_columns, column)

            # add lags
            for(lag_index in 1:num_lags) {

                lagged_dataset <- cbind(lagged_dataset, stats::lag(dataset[, column], lag_index * -1))
                new_columns <- c(new_columns, paste0(column, '_lag_', lag_index))
            }
        }
    }

    colnames(lagged_dataset) <- new_columns

    return (lagged_dataset)
}

#' adds lag variables, performs regrssion & forecasting, produces a plot
#'
#' @param dataset a time-series dataset (single- or multi-variable)
#' @param dependent_variable the dependent variable; can be null for single-var datasets
#' @param independent_variables the independent variables;
#'
#' `independent_variables` can be null for single-var datasets; for multi-variable datasets, a value of `NULL` will include all variables
#'
#' single-variable & multi-variable datasets can also include `trend` and/or `season` values
#'
#' @param lambda (simply passes value to `tslm`); according to their docs: "Box-Cox transformation parameter. If lambda="auto", then a transformation is automatically selected using BoxCox.lambda. The transformation is ignored if NULL. Otherwise, data transformed before model is estimated."
#' @param num_lags if specified, adds lag variables for all the independent_variables
#' @param include_dependent_variable_lag if num_lags >= 1 *and* a multi-variable dataset, then this indicates whether or not we should lag the dependent_variable and include it in the model (if the data is single-variable, then this is ignored, because the only thing to lag is the dependent_variable)
#' @param ex_ante_forecast_horizon if specified, indicates how far into the future we should forecast. All original variables (except for `trend`/`season`) and all lag values that have a lag less than `ex_ante_forecast_horizon` will be removed.
#' @param build_graphs if TRUE, will build various graphs and return those ggplot objects in the returned list
#' @param show_dataset_labels whether or not to show the values of the original dataaset
#' @param show_forecast_labels whether or not to show the forecast values
#'
#' @importFrom stringr str_split
#' @importFrom ggplot2 aes geom_point geom_text labs geom_boxplot
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @export
rt_ts_auto_regression <- function(dataset,
                                  dependent_variable=NULL,
                                  independent_variables=NULL,
                                  lambda=NULL,
                                  num_lags=NULL,
                                  include_dependent_variable_lag=TRUE,
                                  ex_ante_forecast_horizon=NULL,
                                  build_graphs=TRUE,
                                  show_dataset_labels=FALSE,
                                  show_forecast_labels=TRUE) {

    if(!is.null(num_lags)) {

        stopifnot(num_lags >= 1)
    }

    if(rt_ts_is_single_variable(dataset)) {
        # single variable dataset has to have independent variables to regress on,
        # meaning that it has to have either lags-vars or trend/season vars
        stopifnot(!is.null(num_lags) ||
                    (!is.null(independent_variables) && all(independent_variables %in% c('trend', 'season'))))
    } else {
        # dependent_variable cannot be null for multi-variable dataset
        # if independent_variables is null for multi-variable dataset, set it to all columns except dependent_variable
        stopifnot(!is.null(dependent_variable))

        if(is.null(independent_variables)) {

            column_names <- colnames(dataset)
            independent_variables <- column_names[! column_names %in% dependent_variable]
        }
    }

    # ex_ante_forecast_horizon means we are going to do a *true* forecast, and
    # *either* we need lags (at least enough lags for the corresponding horizon), or we need `trend` and/or `season`
    if(!is.null(ex_ante_forecast_horizon) && !any(independent_variables %in% c('trend', 'season'))) {

        stopifnot(num_lags >= ex_ante_forecast_horizon)
    }

    original_dependent_variable <- dependent_variable
    if(is.null(dependent_variable)) {

        dependent_variable <- 'data'
    }


    # add lags to dataset
    if(!is.null(num_lags)) {

        # if the *original* dataset is single-var, then we'll need to rename the dependent_variable to the name that rt_ts_create_lagged_dataset gives
        if(rt_ts_is_single_variable(dataset)) {

            dependent_variable = 'original_data'  # this is what rt_ts_create_lagged_dataset names the column for single-var
        }

        keep_variables <- original_dependent_variable
        lag_variables <- independent_variables[! independent_variables %in% c('trend', 'season')]

        if(include_dependent_variable_lag && rt_ts_is_multi_variable(dataset)) {

            # if we are going to lag on the dependent vairable as well, then we have to include that in
            # lag_variables and remove it from keep_variables (keep_variables keeps the specified variables
            # but doesn't lag them)
            lag_variables <- c(dependent_variable, lag_variables)
            keep_variables <- NULL
        }

        dataset <- rt_ts_create_lagged_dataset(dataset,
                                               num_lags=num_lags,
                                               lag_variables=lag_variables,
                                               keep_variables=keep_variables)  # for single-var we will have changed it from NULL

        # now, the column names of the new dataset (other than the dependent_variable) contains all of the
        # original and new independent_variables
        # but, it won't have trend/season, so if that was in the independent variables, we need to add it back
        trend_season <- independent_variables[independent_variables %in% c('trend', 'season')]  # get trend and/or season if they are in the list of independent variables
        column_names <- colnames(dataset)
        independent_variables <- c(trend_season, column_names[! column_names %in% dependent_variable])
    }

    reg_formula <- rt_ts_lm_build_formula(dependent_variable=dependent_variable,
                                          independent_variables=independent_variables,
                                          ex_ante_forecast_horizon=ex_ante_forecast_horizon)


    # extract all variables used in the regression formula (and thus the regression model)
    independent_vars_used <- str_split(str_split(reg_formula, pattern=' ~ ', simplify=TRUE )[2], ' \\+ ')[[1]]
    stopifnot(!is.null(independent_vars_used) && length(independent_vars_used) != 0 && !any(independent_vars_used == ""))
    # now figure out which vars were used in the regression model, aside from trend/season (i.e. which variables we need to subset)
    independent_vars_used_from_dataset <- independent_vars_used[! independent_vars_used %in% c('trend', 'season')]

    # if any variables are used beside trend/season, we want the dataset containing only those values
    # because, for example, if we do an na.omit on variables that have a lot of missing data but aren't even
    # used in the model, we'll be needlessly restricting the data avaialable to the model
    if(length(independent_vars_used_from_dataset) > 0) {
        # we just want the dataset that is actually used by the regression model
        dataset <- dataset[, c(dependent_variable, independent_vars_used_from_dataset)]
    }

    # for LAGGED DATA na.omit will remove the NAs at the beginning of the dataset (because a column that lags x
    # periods doesn't have values for the first x periods)
    # but will also remove the NAs at the end of the dataset which is needed to we restrict the training to the
    # original time horizon i.e. ending period
    training_data <- na.omit(dataset)
    ts_model <- tslm(formula=reg_formula, data=training_data, lambda=lambda)

    ts_forecast <- NULL
    # we can only forecast for ex_ante regressions, otherwise we don't have the required data
    if(!is.null(ex_ante_forecast_horizon)) {

        # if we only forecast trend and/or season (i.e. 0 variables from the original dataset)
        # then we don't need to extract other data, just forecast however many periods
        if(length(independent_vars_used_from_dataset) == 0) {

            ts_forecast <- forecast(ts_model, h=ex_ante_forecast_horizon)

        } else {  # `dataset` has 1 or more variables (including the dependent_variable)

            # now we want the dataset of just the independent variables with the NAs removed, because the last
            # `ex_ante_forecast_horizon` number of periods of this dataset will be the first
            # `ex_ante_forecast_horizon` number of periods after the last available period in the original training set
            # i.e. will be the `ex_ante_forecast_horizon` periods we can predict

            # it is possible that we stripped out all but one variable i.e. it is now single-var dataset
            temp <- na.omit(dataset[, independent_vars_used_from_dataset])


            # i want to verify that the end() of temp is exactly `ex_ante_forecast_horizon` periods after the
            # last period we trained on
            period_add_duration <- function(period, data_frequency, periods_to_add) {

                years_to_add <- floor((period[2] + periods_to_add - 1) / data_frequency)
                ending_period <- (period[2] + periods_to_add) %% data_frequency

                if(ending_period == 0) {
                    ending_period <- data_frequency
                }

                period[1] <- period[1] + years_to_add
                period[2] <- ending_period

                return (period)
            }

            # period_add_duration(period=c(2008, 6), data_frequency=12, periods_to_add=5)
            # period_add_duration(period=c(2008, 6), data_frequency=12, periods_to_add=6)
            # period_add_duration(period=c(2008, 6), data_frequency=12, periods_to_add=7)
            # period_add_duration(period=c(2008, 6), data_frequency=12, periods_to_add=24)
            # period_add_duration(period=c(2008, 6), data_frequency=12, periods_to_add=24 + 5)
            # period_add_duration(period=c(2008, 6), data_frequency=12, periods_to_add=24 + 6)
            # period_add_duration(period=c(2008, 6), data_frequency=12, periods_to_add=24 + 7)

            # need to "add" a period plus a "duration"
            expected_end <- period_add_duration(period=end(training_data),
                                                data_frequency=frequency(training_data),
                                                periods_to_add=ex_ante_forecast_horizon)
            stopifnot(all(expected_end == end(temp)))

            if(rt_ts_is_single_variable(temp)) {

                # rt_ts_is_single_variable == TRUE should mean that we only have one independent variable, confirm logic
                stopifnot(length(independent_vars_used_from_dataset) == 1)

                num_periods <- length(temp)
                new_data <- subset(temp, start=num_periods - ex_ante_forecast_horizon + 1)

                # even though it is a single-var dataset, the tmls was given a data.frame, with a particular column
                new_data <- as.data.frame(new_data)
                colnames(new_data) <- independent_vars_used_from_dataset

            } else {

                num_periods <- nrow(temp)
                # new_data should now contain a ts dataset of the periods we want to predict and the (lagged)
                # values necessary to predict them
                start_index <- num_periods - ex_ante_forecast_horizon + 1
                new_data <- as.data.frame(subset(temp, start=start_index))

                # if ex_ante_forecast_horizon is 1 (i.e. we're only subsetting one period), then subsetting the
                # data when there are multiple variables and then turning that data into a data.frame results
                # in a data frame that has the columns as rows...
                if(ex_ante_forecast_horizon == 1 && length(independent_vars_used_from_dataset) > 1) {

                    new_data <- as.data.frame(t(new_data))
                }
            }

            # the number of rows in new_data should match the number of periods we are forecasting
            stopifnot(nrow(new_data) == ex_ante_forecast_horizon)

            ts_forecast <- forecast(ts_model, newdata=new_data)
        }
    }

    ggplot_fit <- NULL
    ggplot_actual_vs_fit <- NULL
    ggplot_residual_vs_fit <- NULL
    ggplot_residual_vs_predictors <- NULL
    ggplot_residual_vs_season <- NULL
    ggplot_residual_vs_period <- NULL
    if(build_graphs) {

        ######################################################################################################
        # datasets the graphs will share
        ######################################################################################################
        if(rt_ts_is_single_variable(dataset)) {

            dependent_values <- dataset

        } else {

            dependent_values <- dataset[, dependent_variable]
        }
        fitted_values <- fitted(ts_model)
        residual_values <- residuals(ts_model)

        # df_fit_data contains original dataset with fitted/residuals/etc.
        df_fit_data <- NULL
        if(length(independent_vars_used_from_dataset) > 0) {

            df_fit_data <- cbind(dataset[, independent_vars_used_from_dataset],
                                 Actual=dependent_values,
                                 Fitted=fitted_values,
                                 Season=cycle(dependent_values),
                                 Time=rt_ts_get_time_period(dependent_values),
                                 Residuals=residual_values) %>%
                na.omit() %>%
                as.data.frame() %>%
                mutate(Season=as.factor(Season))
            colnames(df_fit_data) <- c(independent_vars_used_from_dataset,
                                       'Actual', 'Fitted', 'Season', 'Time', 'Residuals')

        } else {

            df_fit_data <- cbind(Actual=dependent_values,
                                 Fitted=fitted_values,
                                 Season=cycle(dependent_values),
                                 Time=rt_ts_get_time_period(dependent_values),
                                 Residuals=residual_values) %>%
                na.omit() %>%
                as.data.frame() %>%
                mutate(Season=as.factor(Season))

        }

        ######################################################################################################
        # FIT GRAPH (AND FORECAST IF APPLICABLE)
        ######################################################################################################
        # build plot; use data before it was striped of NAs (`dataset`) so when we plot the regression points,
        # we can see what the model used or did not use
        ggplot_fit <- autoplot(dependent_values) +
            autolayer(fitted(ts_model), series='Regression')

        if(!is.null(ex_ante_forecast_horizon)) {

            ggplot_fit <- ggplot_fit +
                autolayer(ts_forecast, series = 'Regression')

            if(show_forecast_labels) {

                forecast_start <- start(ts_forecast$mean)
                forecast_end <- end(ts_forecast$mean)
                forecast_freq <- frequency(ts_forecast$mean)

                ts_forecast_data <- ts(as.data.frame(ts_forecast)$`Point Forecast`,
                                       start = forecast_start,
                                       end=forecast_end,
                                       frequency = forecast_freq)
                ggplot_fit <- ggplot_fit +
                    geom_point(data=ts_forecast_data) +
                    geom_text(data=ts_forecast_data,
                              aes(label=rt_pretty_numerics(as.numeric(ts_forecast_data))),
                              size=3.1,
                              check_overlap=TRUE,
                              vjust=-0.2,
                              hjust=-0.2)
            }
        }

        ggplot_fit <- ggplot_fit + labs(y=dependent_variable)

        ######################################################################################################
        # Actual vs Fitted
        ######################################################################################################
        data_freq <- frequency(dependent_values)
        if(data_freq == 1 || data_freq > 12) {

            ggplot_actual_vs_fit <- ggplot(df_fit_data, aes(x=Fitted, y=Actual))

        } else {

            ggplot_actual_vs_fit <- ggplot(df_fit_data, aes(x=Fitted, y=Actual, color=Season))
        }

        label_threshold <- as.numeric(quantile(abs(residual_values), .97))  # get value at 97th percentile of data

        ggplot_actual_vs_fit <- ggplot_actual_vs_fit +
            geom_point() +
            geom_abline(intercept=0, slope=1) +
            geom_text(aes(label=ifelse(abs(Residuals) > label_threshold, Time, '')),  # show extreme values
                      size=3,
                      vjust=-0.2,
                      hjust=-0.2) +
            geom_smooth(method='loess', group=1, se=FALSE, size=0.5, colour='red') +
            labs(title='Actual vs. Fitted Values',
                 x='Fitted',
                 y='Actual Values',
                 caption='\nBlack line shows perfect alignment between `fitted` and `actual` values.\nRed line shows smoothed trend between `fitted` and `actual`.\nData points with large residuals are labed.')

        ######################################################################################################
        # Residuals vs Fitted
        ######################################################################################################
        data_freq <- frequency(dependent_values)
        if(data_freq == 1 || data_freq > 12) {

            ggplot_residual_vs_fit <- ggplot(df_fit_data, aes(x=Fitted, y=Residuals))

        } else {

            ggplot_residual_vs_fit <- ggplot(df_fit_data, aes(x=Fitted, y=Residuals, color=Season))
        }

        label_threshold <- as.numeric(quantile(abs(residual_values), .97))  # get value at 97th percentile of data

        ggplot_residual_vs_fit <- ggplot_residual_vs_fit +
            geom_point() +
            geom_abline(intercept=0, slope=0) +
            geom_text(aes(label=ifelse(abs(Residuals) > label_threshold, Time, '')),  # show extreme values
                      size=3,
                      vjust=-0.2,
                      hjust=-0.2) +
            geom_smooth(method='loess', group=1, se=FALSE, size=0.5, colour='red') +
            labs(title='Residuals vs. Fitted Values',
                 x='Fitted',
                 y='Residuals Values',
                 caption='\nBlack line is reference for 0 residual/error.\nRed line shows smoothed trend between `residuals` and `fitted` values.\nData points with large residuals are labed.')

        ######################################################################################################
        # Residuals vs Predictors
        ######################################################################################################
        if(length(independent_vars_used_from_dataset) > 0) {

            ggplot_residual_vs_predictors <- df_fit_data[, c('Residuals', independent_vars_used_from_dataset)] %>%
                gather(key, value, -Residuals) %>%
                mutate(key=factor(key, levels = independent_vars_used_from_dataset)) %>%
            ggplot(aes(x=value, y=Residuals, group=key)) +
                geom_point() +
                geom_smooth(method='loess') +
                facet_wrap( ~ key,
                            scales='free_x',
                            ncol=min(2, length(independent_vars_used_from_dataset))) +
                labs(title='Residuals vs. Predictors',
                     x='Predictor Values')
        }

        ######################################################################################################
        # Residuals vs Period
        ######################################################################################################
        floor_times <- floor(df_fit_data$Time)
        if(length(unique(floor_times)) <= 50) {

            ggplot_residual_vs_period <- ggplot(df_fit_data %>% mutate(Floor_Times=as.factor(floor_times)),
                                                aes(x=Floor_Times, y=Residuals)) +
                geom_boxplot() +
                labs(title = 'Residuals vs. Period',
                     x='Period')
        }

        ######################################################################################################
        # Residuals vs Season
        ######################################################################################################
        if(frequency(dataset) > 1 && frequency(dataset) <= 52) {

            ggplot_residual_vs_season <- ggplot(df_fit_data, aes(x=Season, y=Residuals)) +
                geom_boxplot() +
                labs(title = 'Residuals vs. Season')
        }
    }

    return (list(formula=reg_formula,
                 model=ts_model,
                 forecast=ts_forecast,
                 plot_fit=ggplot_fit,
                 plot_actual_vs_fitted=ggplot_actual_vs_fit,
                 plot_residuals_vs_fitted=ggplot_residual_vs_fit,
                 plot_residuals_vs_predictors=ggplot_residual_vs_predictors,
                 plot_residuals_vs_period=ggplot_residual_vs_period,
                 plot_residuals_vs_season=ggplot_residual_vs_season))
}

#' Returns friendly axis ticks. e.g. for monthly it returns a factor with values c("2019-01", "2019-02").
#' If the frequency of the dataset is anything other than 52, 12, 4, it returns the raw time() values.
#'
#' @param dataset ts dataset
#' @importForm stats time
#' @importFrom timeDate frequency
#' @importFrom stringr str_pad
#' @export
rt_ts_get_friendly_time_ticks <- function(dataset) {

    time_ticks <- as.numeric(time(dataset))
    num_periods <- frequency(dataset)

    if(num_periods == 52 || num_periods == 12 || num_periods == 4) {

        years <- floor(time_ticks)
        time_minor <- time_ticks - years
        friendly_minor <- (num_periods * time_minor) + 1
        num_zeros_to_pad <- nchar(trunc(max(friendly_minor)))
        friendly_ticks <- paste0(years,
                                 '-',
                                 str_pad(string=round(friendly_minor),
                                         width=num_zeros_to_pad, pad="0", side='left'))
        friendly_ticks <- factor(friendly_ticks, levels=friendly_ticks, ordered=TRUE)
        return (friendly_ticks)

    } else {

        return (time_ticks)
    }
}

#' plots a time-series dataset
#'
#' @param dataset ts dataset
#' @param show_values show text value above each point in time
#' @param show_points show points
#' @param show_dates show dates above each point in time
#' @param y_zoom_min adjust (i.e. zoom in) to the y-axis; sets the minimum y-value for the adjustment
#' @param y_zoom_max adjust (i.e. zoom in) to the y-axis; sets the maximum y-value for the adjustment
#' @param facet_multi_variables if TRUE each variable gets it's own section
#' @param text_size sets the text size
#' @param base_size sets the base size
#'
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
#' @importFrom purrr map_chr
#' @importFrom ggplot2 ggplot aes geom_line expand_limits scale_y_continuous scale_color_manual theme_light labs geom_text geom_point theme element_text coord_cartesian facet_wrap
#' @importFrom scales pretty_breaks format_format
#' @importForm stats time
#' @importFrom timeDate frequency
#' @export
rt_ts_plot_time_series <- function(dataset,
                                   show_values=FALSE,
                                   show_points=FALSE,
                                   show_dates=FALSE,
                                   y_zoom_min=NA,
                                   y_zoom_max=NA,
                                   facet_multi_variables=FALSE,
                                   text_size=4,
                                   base_size=11) {

    df_dataset <- as.data.frame(dataset)
    num_periods <- frequency(dataset)

    friendly_ticks <- rt_ts_get_friendly_time_ticks(dataset)

    # 36 to allow for 3 years of monthly
    if(is.factor(friendly_ticks) && length(friendly_ticks) > 36) {

        df_dataset$ticks <- as.numeric(time(dataset))

    } else {

        df_dataset$ticks <- friendly_ticks
    }
    df_dataset$friendly_ticks <- friendly_ticks

    df_dataset <- df_dataset %>%
        gather(key, value, -c(ticks, friendly_ticks))
    df_dataset$pretty_value <- map_chr(df_dataset$value,
                                       ~ prettyNum(., big.mark=",", preserve.width="none", digits=4, scientific=FALSE))

    ggplot_object <- df_dataset %>%
        ggplot(aes(x=ticks, y=value, color=key, group=key)) +
            geom_line() +
            expand_limits(y=0) +
            scale_y_continuous(breaks=pretty_breaks(10),
                               labels = format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)) +
            scale_color_manual(values=c(rt_colors(), rt_colors()), na.value = '#2A3132') +
            theme_light(base_size = base_size) +
            labs(y=NULL,
                 x=NULL)

    if(show_values) {
        ggplot_object <- ggplot_object +
            geom_text(aes(label = pretty_value),
                      check_overlap = TRUE,
                      vjust=-0.5, size=text_size, na.rm = TRUE)
    }

    if(show_dates) {

        if(show_values) {

            show_values_hjust <- -0.5

        } else {

            show_values_hjust <- -0.1
        }

        ggplot_object <- ggplot_object +
            geom_text(aes(label = friendly_ticks),
                      check_overlap = TRUE,
                      vjust=--0.5, hjust=show_values_hjust, size=text_size, na.rm = TRUE, angle=90)
    }

    if(show_points) {

        ggplot_object <- ggplot_object + geom_point(size=1)
    }

    if(is.factor(df_dataset$ticks)) {

        ggplot_object <- ggplot_object + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }

    if(length(unique(df_dataset$key)) == 1) {

        ggplot_object <- ggplot_object + theme(legend.position = "none")
    }

    if(!is.na(y_zoom_min) || !is.na(y_zoom_max)) {

        # if one of the zooms is specified then we hae to provide both, so get corresponding min/max
        if(is.na(y_zoom_min)) {

            y_zoom_min <- min(dataset, na.rm = TRUE)
        }

        if(is.na(y_zoom_max)) {

            y_zoom_max <- max(dataset, na.rm = TRUE)
        }

        ggplot_object <- ggplot_object + coord_cartesian(ylim = c(y_zoom_min, y_zoom_max))
    }

    if(facet_multi_variables && rt_ts_is_multi_variable(dataset)) {

        ggplot_object <- ggplot_object + facet_wrap(~ key, ncol=1, scales = 'free_y')
    }

    return (ggplot_object)
}
