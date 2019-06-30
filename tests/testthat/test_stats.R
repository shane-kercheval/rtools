context('Stats')
library(testthat)
library(dplyr)

test_that("rt_geometric_mean", {

    values <- c(3, 8, 10, 17, 24, 27)
    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0.0001),
                 11.75802, tolerance=1e-6)

    expect_equal(rt_geometric_mean(values=values, na.rm=FALSE, add_subtract=0),
                 11.75906, tolerance=1e-6)

    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0),
                 11.75906, tolerance=1e-6)

    values <- c(3, 8, 10, 17, 24, 27, 100, 1000)
    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0.0001),
                 26.77564, tolerance=1e-6)

    expect_equal(rt_geometric_mean(values=values, na.rm=FALSE, add_subtract=0.0001),
                 26.77564, tolerance=1e-6)

    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0),
                 26.77808, tolerance=1e-6)

    values <- c(3, 8, 10, 17, 24, 27, 100, 1000, 0)
    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0.0001),
                 6.678098, tolerance=1e-6)

    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0),
                 -Inf, tolerance=1e-6)

    values <- c(3, 8, 10, 17, 24, 27, 100, 1000, 0.0000001)
    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0),
                 3.099984, tolerance=1e-6)

    values <- c(3, 8, 10, 17, 24, 27, 100, 1000, 0, NA)
    expect_true(is.na(rt_geometric_mean(values=values, na.rm=FALSE, add_subtract=0.0001)))

    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0.0001),
                 6.678098, tolerance=1e-6)

    expect_equal(rt_geometric_mean(values=values, na.rm=TRUE, add_subtract=0),
                 -Inf, tolerance=1e-6)
})


test_that('rt_regression_build_formula', {

    reg_formula <- rt_regression_build_formula(dependent_variable = 'dependent_var',
                                               independent_variables = c('A'))
    expect_equal(reg_formula, 'dependent_var ~ A')

    reg_formula <- rt_regression_build_formula(dependent_variable = 'dependent_var',
                                               independent_variables = c('A', 'B'))
    expect_equal(reg_formula, 'dependent_var ~ A + B')

    reg_formula <- rt_regression_build_formula(dependent_variable = 'dependent_var',
                                               independent_variables = c('A', 'B', 'C'))
    expect_equal(reg_formula, 'dependent_var ~ A + B + C')

    reg_formula <- rt_regression_build_formula(dependent_variable = 'dependent_var',
                                               independent_variables = c('A', 'B', 'C'),
                                               interaction_variables = list(c('C', 'D')))
    expect_equal(reg_formula, 'dependent_var ~ C*D + A + B + C')

    reg_formula <- rt_regression_build_formula(dependent_variable = 'dependent_var',
                                               independent_variables = c('A', 'B', 'C'),
                                               interaction_variables = list(c('C', 'D'), c('E', 'F')))
    expect_equal(reg_formula, 'dependent_var ~ C*D + E*F + A + B + C')

    reg_formula <- rt_regression_build_formula(dependent_variable = 'dependent_var',
                                               #independent_variables = c('A', 'B', 'C'),
                                               interaction_variables = list(c('C', 'D'), c('E', 'F')))
    expect_equal(reg_formula, 'dependent_var ~ C*D + E*F')
})

compare_models <- function(actual_model, expected_model){
    expect_true(all(actual_model$coefficients == expected_model$coefficients))

    expect_true(all(actual_model$residuals == expected_model$residuals))
    expect_true(all(actual_model$effects == expected_model$effects))
    expect_true(all(actual_model$rank == expected_model$rank))
    expect_true(all(actual_model$fitted.values == expected_model$fitted.values))
    expect_true(all(actual_model$terms == expected_model$terms))
    expect_true(rt_are_dataframes_equal(actual_model$model, expected_model$model))
}

save_lm_summary <- function(model, file_name) {
    sink(file_name)
    print(summary(model))
    sink()
}

test_that('rt_regression', {
    data('mtcars')
    reg_data <- mtcars
    dependent_variable = 'mpg'
    independent_variables = c('cyl', 'hp', 'wt')

    reg_data_orignal <- reg_data %>% select(dependent_variable, independent_variables)
    expected_formula <- rt_regression_build_formula(dependent_variable = dependent_variable,
                                                    independent_variables = independent_variables)

    result <- rt_regression(dataset = reg_data,
                            dependent_variable = dependent_variable,
                            independent_variables = independent_variables)
    expect_equal(length(result$rows_excluded), 0)
    compare_models(actual_model=result$model,
                   expected_model=lm(mpg ~ cyl + hp + wt, mtcars))
    expect_equal(result$formula, expected_formula)
    expect_equal(result$type, "Linear Regression")
    expect_true(setequal(independent_variables,
                         rt_regression_get_ind_var_options(result$model,
                                                           original_dataset=reg_data_orignal,
                                                           dependent_variable=dependent_variable)))
    result$model %>% save_lm_summary("data/rt_regression__mtcars__summary_1.txt")

    test_save_plot(file_name='data/rt_regression_plot_actual_vs_predicted__mtcars.png',
                   plot=rt_regression_plot_actual_vs_predicted(result$model))
    test_save_plot(file_name='data/rt_regression_plot_residual_vs_predicted__mtcars.png',
                   plot=rt_regression_plot_residual_vs_predicted(model))
    test_save_plot(file_name='data/rt_regression_plot_residual_vs_variable__mtcars__wt.png',
                   plot=rt_regression_plot_residual_vs_variable(result$model, 'wt', reg_data_orignal))

    rt_regression_plot_actual_vs_predicted(result$model)
    rt_regression_plot_residual_vs_predicted(model)

    rt_regression_plot_residual_vs_variable(result$model, 'carat', reg_data_orignal)
    rt_regression_plot_residual_vs_variable(result$model, 'cut', reg_data_orignal)

















    # reg_data <- diamonds
    # dependent_variable = 'price'
    # independent_variables = c('carat', 'cut')

    reg_data <- reg_data %>% select(dependent_variable, independent_variables)
    reg_data_orignal <- reg_data
    expected_formula <- rt_regression_build_formula(dependent_variable = dependent_variable,
                                                    independent_variables = independent_variables)
    result <- rt_regression(dataset = reg_data,
                            dependent_variable = dependent_variable,
                            independent_variables = independent_variables)

    summary(result$model)

    library(modelr)
    reg_data <- reg_data %>% add_predictions(result$model) %>% add_residuals(result$model)



    rt_regression_plot_residual_vs_variable(result$model, 'wt', reg_data_orignal)
    rt_regression_plot_residual_vs_variable(result$model, 'cyl', reg_data_orignal)



    modelr::

    reg_data %>% model_matrix(formula=formula(expected_formula))

    ?predict

    ?lm

    expected_result <- stats::lm(data = diamonds, price ~ carat + cut)
    summary(expected_result)
    diamonds %>% add_predictions(expected_result)
    summary(diamonds %>% add_predictions(expected_result))


    expected_result <- lm(data = diamonds, log(price) ~ log(carat) + cut)
    rt_regression_plot_actual_v_predicted(expected_result)
    expected_result$model$`log(price)`
    plot(expected_result, which=1)
    diamonds %>%
        add_predictions(expected_result) %>%
        mutate(pred = exp(pred))
    summary(expected_result)









    result$results$fitted.values

    diamonds$carat
    splines::ns(diamonds$carat, df=5)
    poly(diamonds$carat, degree = 3)

    poly(diamonds$price, degree = 3)

    log(diamonds$carat)
    log(diamonds$price, base = 2)
    # what base?


    expected_result <- lm(data = diamonds, log(price) ~ log(carat) + cut)
    summary(expected_result)
    plot(expected_result, which=1)

    expected_result$fitted.values
    expected_result$residuals

    ?add_residuals


    df <- tibble::data_frame(
        x = sort(runif(100)),
        y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
    )
    plot(df)

    library(modelr)
    m1 <- lm(y ~ x, data = df)
    df %>% add_residuals(m1)

    m2 <- lm(y ~ poly(x, 2), data = df)
    df %>% spread_residuals(m1, m2)
    df %>% gather_residuals(m1, m2)

    ??gam
    install.packages('gam')
    library(gam)

})
