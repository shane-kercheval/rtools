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
                                          ex_ante_horizon=NULL)
    expect_equal(reg_formula, 'dataset ~ trend + season')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('trend', 'season'),
                                          ex_ante_horizon=3)
    expect_equal(reg_formula, 'dataset ~ trend + season')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('trend'),
                                          ex_ante_horizon=NULL)
    expect_equal(reg_formula, 'dataset ~ trend')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('trend'),
                                          ex_ante_horizon=3)
    expect_equal(reg_formula, 'dataset ~ trend')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('season'),
                                          ex_ante_horizon=NULL)
    expect_equal(reg_formula, 'dataset ~ season')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'dataset',
                                          independent_variables = c('season'),
                                          ex_ante_horizon=3)
    expect_equal(reg_formula, 'dataset ~ season')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'season', 'NSWNthCo'))
    expect_equal(reg_formula, 'NSWMetro ~ trend + season + NSWNthCo')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'season', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2'),
                                          ex_ante_horizon = NULL)
    expect_equal(reg_formula, 'NSWMetro ~ trend + season + NSWNthCo + NSWNthCo_lag_1 + NSWNthCo_lag_2')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'season', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2'),
                                          ex_ante_horizon = 1)
    expect_equal(reg_formula, 'NSWMetro ~ trend + season + NSWNthCo_lag_1 + NSWNthCo_lag_2')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'season', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2'),
                                          ex_ante_horizon = 2)
    expect_equal(reg_formula, 'NSWMetro ~ trend + season + NSWNthCo_lag_2')


    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2',
                                                                    'XYZ', 'XYZ_lag_1', 'XYZ_lag_2',
                                                                    'XYZ_lag_3', 'XYZ_lagg_3'),
                                          ex_ante_horizon = 2)
    expect_equal(reg_formula, 'NSWMetro ~ trend + NSWNthCo_lag_2 + XYZ_lag_2 + XYZ_lag_3')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('trend', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2',
                                                                    'XYZ', 'XYZ_lag_1', 'XYZ_lag_2',
                                                                    'XYZ_lag_3', 'XYZ_lagg_3'),
                                          ex_ante_horizon = NULL)
    expect_equal(reg_formula, 'NSWMetro ~ trend + NSWNthCo + NSWNthCo_lag_1 + NSWNthCo_lag_2 + XYZ + XYZ_lag_1 + XYZ_lag_2 + XYZ_lag_3 + XYZ_lagg_3')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('season', 'NSWNthCo',
                                                                    'NSWNthCo_lag_1', 'NSWNthCo_lag_2'),
                                          ex_ante_horizon = 2)
    expect_equal(reg_formula, 'NSWMetro ~ season + NSWNthCo_lag_2')

    reg_formula <- rt_ts_lm_build_formula(dependent_variable = 'NSWMetro',
                                          independent_variables = c('NSWNthCo', 'NSWNthCo_lag_1',
                                                                    'NSWNthCo_lag_2'),
                                          ex_ante_horizon = 2)
    expect_equal(reg_formula, 'NSWMetro ~ NSWNthCo_lag_2')

})
