context('Time Series')
library(testthat)
library(fpp2)

test_that('rt_ts_is_single_variable', {

    expect_true(rt_ts_is_single_variable(a10))  # class: ts
    expect_false(rt_ts_is_single_variable(melsyd))  # classes: mst ts
    expect_false(rt_ts_is_single_variable(visnights))  # classes: mst ts matrix
    expect_false(rt_ts_is_single_variable(elecdemand))   # classes: msts ts

})

test_that('rt_ts_is_multi_variable', {

    expect_false(rt_ts_is_multi_variable(a10))  # class: ts
    expect_true(rt_ts_is_multi_variable(melsyd))  # classes: mst ts
    expect_true(rt_ts_is_multi_variable(visnights))  # classes: mst ts matrix
    expect_true(rt_ts_is_multi_variable(elecdemand))   # classes: msts ts
})

test_that('rt_ts_lm_build_formula', {

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('trend', 'season'),
                                          ex_ante_forecast_horizon=NULL)
    expect_equal(reg_formula, 'dataset ~ trend + season')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('trend', 'season'),
                                          ex_ante_forecast_horizon=3)
    expect_equal(reg_formula, 'dataset ~ trend + season')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('trend'),
                                          ex_ante_forecast_horizon=NULL)
    expect_equal(reg_formula, 'dataset ~ trend')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('trend'),
                                          ex_ante_forecast_horizon=3)
    expect_equal(reg_formula, 'dataset ~ trend')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('season'),
                                          ex_ante_forecast_horizon=NULL)
    expect_equal(reg_formula, 'dataset ~ season')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('season'),
                                          ex_ante_forecast_horizon=3)
    expect_equal(reg_formula, 'dataset ~ season')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'season', 'NSWNthCo'))
    expect_equal(reg_formula, 'NSWMetro ~ trend + season + NSWNthCo')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'season', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2'),
                                          ex_ante_forecast_horizon = NULL)
    expect_equal(reg_formula, 'NSWMetro ~ trend + season + NSWNthCo + NSWNthCo_lag_1 + NSWNthCo_lag_2')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'season', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2'),
                                          ex_ante_forecast_horizon = 1)
    expect_equal(reg_formula, 'NSWMetro ~ trend + season + NSWNthCo_lag_1 + NSWNthCo_lag_2')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'season', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2'),
                                          ex_ante_forecast_horizon = 2)
    expect_equal(reg_formula, 'NSWMetro ~ trend + season + NSWNthCo_lag_2')


    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2',
                                                                    'XYZ', 'XYZ_lag_1', 'XYZ_lag_2',
                                                                    'XYZ_lag_3', 'XYZ_lagg_3'),
                                          ex_ante_forecast_horizon = 2)
    expect_equal(reg_formula, 'NSWMetro ~ trend + NSWNthCo_lag_2 + XYZ_lag_2 + XYZ_lag_3')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2',
                                                                    'XYZ', 'XYZ_lag_1', 'XYZ_lag_2',
                                                                    'XYZ_lag_3', 'XYZ_lagg_3'),
                                          ex_ante_forecast_horizon = NULL)
    expect_equal(reg_formula, 'NSWMetro ~ trend + NSWNthCo + NSWNthCo_lag_1 + NSWNthCo_lag_2 + XYZ + XYZ_lag_1 + XYZ_lag_2 + XYZ_lag_3 + XYZ_lagg_3')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('season', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2'),
                                          ex_ante_forecast_horizon = 2)
    expect_equal(reg_formula, 'NSWMetro ~ season + NSWNthCo_lag_2')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('NSWNthCo', 'NSWNthCo_lag_1',
                                                                    'NSWNthCo_lag_2'),
                                          ex_ante_forecast_horizon = 2)
    expect_equal(reg_formula, 'NSWMetro ~ NSWNthCo_lag_2')

})

test_that('rt_ts_create_lagged_dataset', {

    # single-var time-series
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=rt_ts_create_lagged_dataset(a10, num_lags=3),
                                                  rds_file='data/rt_ts_create_lagged_dataset_a10.RDS'))

    # multi-var time-series
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=rt_ts_create_lagged_dataset(visnights,
                                                                                         lag_variables = c('NSWNthCo', 'SAUMetro'),
                                                                                         num_lags=3),
                                                  rds_file='data/rt_ts_create_lagged_dataset_visnights_1.RDS'))
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=rt_ts_create_lagged_dataset(visnights,
                                                                                         lag_variables = c('NSWNthCo', 'SAUMetro'),
                                                                                         keep_variables=c('NSWMetro'),
                                                                                         num_lags=3),
                                                  rds_file='data/rt_ts_create_lagged_dataset_visnights_2.RDS'))
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=rt_ts_create_lagged_dataset(visnights,
                                                                                         lag_variables = c('NSWNthCo', 'SAUMetro'),
                                                                                         keep_variables=c('NSWMetro', 'QLDMetro'),
                                                                                         num_lags=3),
                                                  rds_file='data/rt_ts_create_lagged_dataset_visnights._3RDS'))
    # lag all columns
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=rt_ts_create_lagged_dataset(visnights,
                                                                                         lag_variables = NULL,
                                                                                         num_lags=3),
                                                  rds_file='data/rt_ts_create_lagged_dataset_visnights_all_columns.RDS'))
})

test_that('rt_ts_create_lagged_dataset - single variable', {

    ##########################################################################################################
    # WITHOUT LAG
    ##########################################################################################################
    # single-var dataset has to have either lags or trend/season, otherwise there will be no regressors
    expect_error(rt_ts_auto_regression(a10, ex_ante_forecast_horizon=10))
    expect_error(rt_ts_auto_regression(a10))


    results <- rt_ts_auto_regression(dataset=a10,
                                     independent_variables = 'trend',
                                     num_lags=NULL,
                                     ex_ante_forecast_horizon=5)
    expected_formula <- 'dataset ~ trend'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectation
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ',  simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.8549738, tolerance=1e-7)
    # check that we forecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("Jul 2008", "Aug 2008", "Sep 2008", "Oct 2008", "Nov 2008")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_trend.png', plot = results$plot)

    results <- rt_ts_auto_regression(a10,
                                     independent_variables = c('trend', 'season'),
                                     num_lags=NULL,
                                     ex_ante_forecast_horizon=5)
    expected_formula <- 'dataset ~ trend + season'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.9205468, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("Jul 2008", "Aug 2008", "Sep 2008", "Oct 2008", "Nov 2008")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_trend_season.png', plot = results$plot)

    ##########################################################################################################
    # WITH LAG
    ##########################################################################################################
    results <- rt_ts_auto_regression(dataset=a10,
                                     num_lags=10,
                                     ex_ante_forecast_horizon=10)
    expected_formula <- 'original_data ~ data_lag_10'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.8061207, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("Jul 2008", "Aug 2008", "Sep 2008", "Oct 2008", "Nov 2008", "Dec 2008", "Jan 2009", "Feb 2009", "Mar 2009", "Apr 2009")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_forecast_10.png', plot = results$plot)

    results <- rt_ts_auto_regression(dataset=a10,
                                     num_lags=10,
                                     ex_ante_forecast_horizon=5)
    expected_formula <- 'original_data ~ data_lag_5 + data_lag_6 + data_lag_7 + data_lag_8 + data_lag_9 + data_lag_10'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.8486632, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("Jul 2008", "Aug 2008", "Sep 2008", "Oct 2008", "Nov 2008")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_forecast_lag_5.png', plot = results$plot)

    results <- rt_ts_auto_regression(a10,
                                     independent_variables = 'trend',
                                     num_lags=10,
                                     ex_ante_forecast_horizon=5)
    expected_formula <- 'original_data ~ trend + data_lag_5 + data_lag_6 + data_lag_7 + data_lag_8 + data_lag_9 + data_lag_10'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.8674065, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("Jul 2008", "Aug 2008", "Sep 2008", "Oct 2008", "Nov 2008")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_forecast_trend_lag_5.png', plot = results$plot)

    results <- rt_ts_auto_regression(a10,
                                     independent_variables = c('trend', 'season'),
                                     num_lags=10,
                                     ex_ante_forecast_horizon=5)
    expected_formula <- 'original_data ~ trend + season + data_lag_5 + data_lag_6 + data_lag_7 + data_lag_8 + data_lag_9 + data_lag_10'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.9575629, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("Jul 2008", "Aug 2008", "Sep 2008", "Oct 2008", "Nov 2008")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_forecast_trend_season_lag_5.png', plot = results$plot)

    # lag, but no ex-ante (so can keep all regressors)
    results <- rt_ts_auto_regression(a10,
                                     num_lags=10,
                                     ex_ante_forecast_horizon=NULL)
    expected_formula <- 'original_data ~ data_lag_1 + data_lag_2 + data_lag_3 + data_lag_4 + data_lag_5 + data_lag_6 + data_lag_7 + data_lag_8 + data_lag_9 + data_lag_10'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.8945594, tolerance=1e-7)
    expect_null(results$forecast)
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_lag_10.png', plot = results$plot)

    results <- rt_ts_auto_regression(a10,
                                     independent_variables = 'trend',
                                     num_lags=10,
                                     ex_ante_forecast_horizon=NULL)
    expected_formula <- 'original_data ~ trend + data_lag_1 + data_lag_2 + data_lag_3 + data_lag_4 + data_lag_5 + data_lag_6 + data_lag_7 + data_lag_8 + data_lag_9 + data_lag_10'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.8975611, tolerance=1e-7)
    expect_null(results$forecast)
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_trend_lag_10.png', plot = results$plot)

    results <- rt_ts_auto_regression(a10,
                                     independent_variables = c('trend', 'season'),
                                     num_lags=10,
                                     ex_ante_forecast_horizon=NULL)
    expected_formula <- 'original_data ~ trend + season + data_lag_1 + data_lag_2 + data_lag_3 + data_lag_4 + data_lag_5 + data_lag_6 + data_lag_7 + data_lag_8 + data_lag_9 + data_lag_10'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.9680469, tolerance=1e-7)
    expect_null(results$forecast)
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_a10_trend_season_lag_10.png', plot = results$plot)

})


test_that('rt_ts_create_lagged_dataset - multi variable', {

    # multi-var has to have dependent variable
    expect_error(rt_ts_auto_regression(melsyd))

    results <- rt_ts_auto_regression(melsyd,
                                     dependent_variable = 'First.Class',
                                     num_lags=3,
                                     ex_ante_forecast_horizon=NULL)
    expected_formula <- 'First.Class ~ Business.Class + Business.Class_lag_1 + Business.Class_lag_2 + Business.Class_lag_3 + Economy.Class + Economy.Class_lag_1 + Economy.Class_lag_2 + Economy.Class_lag_3'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.5809303, tolerance=1e-7)
    expect_null(results$forecast)
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_melsyd_lag_3.png', plot = results$plot)

    results <- rt_ts_auto_regression(melsyd,
                                     dependent_variable = 'First.Class',
                                     num_lags=3,
                                     ex_ante_forecast_horizon=3)
    expected_formula <- 'First.Class ~ Business.Class_lag_3 + Economy.Class_lag_3'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.3422511, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("1992.923", "1992.942", "1992.962")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_melsyd_forecast_lag_3.png', plot = results$plot)


    # error, removes all regressors
    expect_error(rt_ts_auto_regression(melsyd,
                                       dependent_variable = 'First.Class',
                                       num_lags=3,
                                       ex_ante_forecast_horizon=4))

    results <- rt_ts_auto_regression(dataset=melsyd,
                                     dependent_variable = 'First.Class',
                                     independent_variables = c('trend', 'season', 'Business.Class', 'Economy.Class'),
                                     num_lags=3,
                                     ex_ante_forecast_horizon=4)  # removes all lags, but works because it retains trend/season
    expected_formula <- 'First.Class ~ trend + season'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.4888078, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("1992.923", "1992.942", "1992.962", "1992.981")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_melsyd_forecast_trend_season_lag_3.png', plot = results$plot)


    ##############
    # Now, we are not using Business.Class, which has many NAs, so the regression should use most of the dataset, confirm with plot
    ##############
    # We do have to subset melysd, because now that we aren't removing all NAs from Business.Class,
    # it actually creates a problem because the resulting dataset has NAs in the middle of the dataset, which is not allowed
    # i.e. na.omit only removes NAs at the beginning/end of a dataset
    dataset <- window(melsyd, start=1988)
    results <- rt_ts_auto_regression(dataset=dataset,
                                     dependent_variable = 'First.Class',
                                     independent_variables = c('trend', 'season', 'Economy.Class'),
                                     num_lags=3,
                                     ex_ante_forecast_horizon=3)
    expected_formula <- 'First.Class ~ trend + season + Economy.Class_lag_3'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.6437302, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("1992.923", "1992.942", "1992.962")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_melsyd_forecast_Economyclass.png', plot = results$plot)


    # no ex-ante
    results <- rt_ts_auto_regression(dataset=melsyd,
                                     dependent_variable = 'First.Class',
                                     independent_variables = c('trend', 'season', 'Business.Class', 'Economy.Class'),
                                     num_lags=3,
                                     ex_ante_forecast_horizon=NULL)  # removes all lags, but works because it retains trend/season
    expected_formula <- 'First.Class ~ trend + season + Business.Class + Business.Class_lag_1 + Business.Class_lag_2 + Business.Class_lag_3 + Economy.Class + Economy.Class_lag_1 + Economy.Class_lag_2 + Economy.Class_lag_3'
    expect_equal(results$formula, expected_formula)
    # ensure the formula used matches our expectations
    expect_equal(as.character(formula(results$model))[3], str_split(expected_formula, ' ~ ', simplify = TRUE)[, 2])
    # ensure the model's R-Squared is expected
    expect_equal(summary(results$model)$r.squared, 0.8238516, tolerance=1e-7)
    # check that we corecasted the correct periods
    expect_true(all(rownames(as.data.frame(results$forecast)) == c("1992.923", "1992.942", "1992.962")))
    # save plot
    test_save_plot(file_name = 'data/ts_regression/regression_melsyd_no_forecast.png', plot = results$plot)

})




























































