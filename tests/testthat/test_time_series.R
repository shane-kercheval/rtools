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

