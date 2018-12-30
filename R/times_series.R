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
#' @param num_lags if specified, adds lag variables for all the independent_variables
#' @param ex_ante_forecast_horizon if specified, indicates how far into the future we should forecast. All original variables (except for `trend`/`season`) and all lag values that have a lag less than `ex_ante_forecast_horizon` will be removed.
#' @param build_graphs if TRUE, will build various graphs and return those ggplot objects in the returned list
#' @param show_dataset_labels whether or not to show the values of the original dataaset
#' @param show_forecast_labels whether or not to show the forecast values
#'
#' @importFrom stringr str_split
#' @importFrom ggplot2 aes geom_point geom_text labs
#' @importFrom dplyr mutate
#' @export
rt_ts_auto_regression <- function(dataset,
                                  dependent_variable=NULL,
                                  independent_variables=NULL,
                                  num_lags=NULL,
                                  ex_ante_forecast_horizon=NULL,
                                  build_graphs=TRUE,
                                  show_dataset_labels=FALSE,
                                  show_forecast_labels=TRUE) {

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

        dependent_variable <- 'dataset'
    }


    # add lags to dataset
    if(!is.null(num_lags)) {

        # if the *original* dataset is single-var, then we'll need to rename the dependent_variable to the name that rt_ts_create_lagged_dataset gives
        if(rt_ts_is_single_variable(dataset)) {

            dependent_variable = 'original_data'  # this is what rt_ts_create_lagged_dataset names the column for single-var
        }

        dataset <- rt_ts_create_lagged_dataset(dataset,
                                               num_lags=num_lags,
                                               lag_variables=independent_variables[! independent_variables %in% c('trend', 'season')],
                                               keep_variables=original_dependent_variable)  # for single-var we will have changed it from NULL

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
    ts_model <- tslm(formula=reg_formula, data=training_data)

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
                new_data <- as.data.frame(subset(temp, start=num_periods - ex_ante_forecast_horizon + 1))
            }

            # the number of rows in new_data should match the number of periods we are forecasting
            stopifnot(nrow(new_data) == ex_ante_forecast_horizon)

            ts_forecast <- forecast(ts_model, newdata=new_data)
        }
    }

    ggplot_fit <- NULL
    ggplot_actual_vs_fit <- NULL
    ggplot_residual_vs_fit <- NULL
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

        df_fit_data <- cbind(Actual=dependent_values,
              Fitted=fitted_values,
              Season=cycle(dependent_values),
              Time=rt_ts_get_time_period(dependent_values),
              Residuals=residual_values) %>%
            na.omit() %>%
            as.data.frame() %>%
            mutate(Season=as.factor(Season))

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

        ggplot_fit <- ggplot_fit +
            labs(y=dependent_variable)

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
    }




    ###### Sandbox


    ###############################################


    return (list(formula=reg_formula,
                 model=ts_model,
                 forecast=ts_forecast,
                 plot_fit=ggplot_fit,
                 plot_actual_vs_fitted=ggplot_actual_vs_fit,
                 plot_residuals_vs_fitted=ggplot_residual_vs_fit))

}
