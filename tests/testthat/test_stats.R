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
                                                           dependent_variable,
                                                           independent_variables)))
    result$model %>% save_lm_summary("data/rt_regression__mtcars__summary_1.txt")

    test_save_plot(file_name='data/rt_regression_plot_actual_vs_predicted__mtcars.png',
                   plot=rt_regression_plot_actual_vs_predicted(result$model))
    test_save_plot(file_name='data/rt_regression_plot_residual_vs_predicted__mtcars.png',
                   plot=rt_regression_plot_residual_vs_predicted(result$model))
    test_save_plot(file_name='data/rt_regression_plot_residual_vs_variable__mtcars__wt.png',
                   plot=rt_regression_plot_residual_vs_variable(result$model, 'wt', reg_data_orignal))


    reg_data <- diamonds
    dependent_variable = 'price'
    independent_variables = c('carat', 'cut', 'color', 'clarity')

    reg_data_orignal <- reg_data %>% select(dependent_variable, independent_variables)
    expected_formula <- rt_regression_build_formula(dependent_variable = dependent_variable,
                                                    independent_variables = independent_variables)

    result <- rt_regression(dataset = reg_data,
                            dependent_variable = dependent_variable,
                            independent_variables = independent_variables)
    expect_equal(length(result$rows_excluded), 0)
    compare_models(actual_model=result$model,
                   expected_model=lm(price ~ carat + cut + color + clarity, diamonds))
    expect_equal(result$formula, expected_formula)
    expect_equal(result$type, "Linear Regression")
    expect_true(setequal(independent_variables,
                         rt_regression_get_ind_var_options(result$model,
                                                           dependent_variable,
                                                           independent_variables)))
    result$model %>% save_lm_summary("data/rt_regression__diamonds__summary_1.txt")

    test_save_plot(file_name='data/rt_regression_plot_residual_vs_variable__diamonds__cut.png',
                   plot=rt_regression_plot_residual_vs_variable(result$model, 'cut', reg_data_orignal))


    reg_data <- diamonds
    dependent_variable = 'price'
    independent_variables = c('carat', 'cut', 'color', 'clarity')


    reg_data_orignal <- reg_data %>% select(dependent_variable, independent_variables)
    expected_formula <- rt_regression_build_formula(dependent_variable = dependent_variable,
                                                    independent_variables = independent_variables)

    result <- rt_regression(dataset = reg_data,
                            dependent_variable = dependent_variable,
                            independent_variables = independent_variables)
    expect_equal(length(result$rows_excluded), 0)
    compare_models(actual_model=result$model,
                   expected_model=lm(price ~ carat + cut + color + clarity, diamonds))
    expect_equal(result$formula, expected_formula)
    expect_equal(result$type, "Linear Regression")
    expect_true(setequal(independent_variables,
                         rt_regression_get_ind_var_options(result$model,
                                                           dependent_variable,
                                                           independent_variables)))
    result$model %>% save_lm_summary("data/rt_regression__diamonds__summary_1.txt")

    test_save_plot(file_name='data/rt_regression_plot_residual_vs_variable__diamonds__cut.png',
                   plot=rt_regression_plot_residual_vs_variable(result$model, 'cut', reg_data_orignal))


    # TODO: test when model has NAs, rt_regression_plot_residual_vs_variable for example won't work
    # because the $model (i.e. values used) has NAs removed. so I think I need to do "complete.cases"
})

test_that('rt_plot_proportions', {

    numerators <- c(197, 135)
    denominators <- c(14600, 7700)
    categories <- c('Old', 'New')
    test_save_plot(file_name='data/rt_plot_proportions__2a.png',
                   rt_plot_proportions(numerators, denominators, categories))

    numerators <- c(110, 1000)
    denominators <- c(1000, 10000)
    categories <- c('A', 'B')
    test_save_plot(file_name='data/rt_plot_proportions__2b.png',
                   rt_plot_proportions(numerators, denominators, categories))
    set.seed(42)
    numerators <- rbinom(20, 500, 0.5)
    set.seed(42)
    denominators <- round(rnorm(20, 500, 5))
    set.seed(42)
    numerators <- c(numerators, rbinom(20, 100, 0.5))
    set.seed(42)
    denominators <- c(denominators, round(rnorm(20, 100, 5)))
    categories <- paste('category', 1:40)
    test_save_plot(file_name='data/rt_plot_proportions__many.png',
                   rt_plot_proportions(numerators, denominators, categories,
                        text_size=2, x_label = "Categories", y_label = 'Proportions', title="Test"))
    test_save_plot(file_name='data/rt_plot_proportions__many_no_confidence_values.png',
                   rt_plot_proportions(numerators, denominators, categories,
                                       show_confidence_values=FALSE,
                                       text_size=2, x_label = "Categories", y_label = 'Proportions', title="Test"))
    test_save_plot(file_name='data/rt_plot_proportions__many__90_conf.png',
                   rt_plot_proportions(numerators, denominators, categories,
                                       confidence_level = 0.90,
                                       text_size=2, x_label = "Categories", y_label = 'Proportions', title="Test"))

})

test_that('rt_plot_funnel:axes_flip', {

    numerators <- c(197, 135)
    denominators <- c(14600, 7700)
    categories <- c('Old', 'New')
    test_save_plot(file_name='data/rt_plot_proportions__2a__flip.png',
                   rt_plot_proportions(numerators, denominators, categories,
                                       axes_flip = TRUE))

    test_save_plot(file_name='data/rt_plot_proportions__2a__flip__axis_limits.png',
                   rt_plot_proportions(numerators, denominators, categories,
                                       axes_flip = TRUE,
                                       axis_limits = c(0, 0.1)))

    numerators <- c(110, 1000)
    denominators <- c(1000, 10000)
    categories <- c('A', 'B')
    test_save_plot(file_name='data/rt_plot_proportions__2b__flip.png',
                   rt_plot_proportions(numerators, denominators, categories,
                                       axes_flip = TRUE))
    set.seed(42)
    numerators <- rbinom(20, 500, 0.5)
    set.seed(42)
    denominators <- round(rnorm(20, 500, 5))
    set.seed(42)
    numerators <- c(numerators, rbinom(20, 100, 0.5))
    set.seed(42)
    denominators <- c(denominators, round(rnorm(20, 100, 5)))
    categories <- paste('category', 1:40)
    test_save_plot(file_name='data/rt_plot_proportions__many__flip.png',
                   rt_plot_proportions(numerators, denominators, categories,
                                       axes_flip = TRUE,
                                       text_size=2, x_label = "Categories", y_label = 'Proportions', title="Test"))
    test_save_plot(file_name='data/rt_plot_proportions__many_no_confidence_values__flip.png',
                   rt_plot_proportions(numerators, denominators, categories,
                                       axes_flip = TRUE,
                                       show_confidence_values=FALSE,
                                       text_size=2, x_label = "Categories", y_label = 'Proportions', title="Test"))
    test_save_plot(file_name='data/rt_plot_proportions__many__90_conf__flip.png',
                   rt_plot_proportions(numerators, denominators, categories,
                                       axes_flip = TRUE,
                                       confidence_level = 0.90,
                                       text_size=2, x_label = "Categories", y_label = 'Proportions', title="Test"))

})

test_that('rt_plot_2_proportions_test', {

    prop_1 = c(197, 14600)
    prop_2 = c(135, 7700)
    # prop_test_results <- prop.test(c(197, 135), c(14600, 7700))
    # prop_test_results$p.value
    categories <- c('Old', 'New')
    test_save_plot(file_name='data/rt_plot_2_proportions_test.png',
                   rt_plot_2_proportions_test(prop_1, prop_2, categories,
                                              #confidence_level=0.95,
                                              axes_flip = FALSE,
                                              title=NULL,
                                              caption=NULL
                                              ))

    test_save_plot(file_name='data/rt_plot_2_proportions_test__90_conf.png',
                   rt_plot_2_proportions_test(prop_1, prop_2, categories,
                                              confidence_level=0.90,
                                              axes_flip = FALSE,
                                              title=NULL,
                                              caption=NULL
                   ))

    test_save_plot(file_name='data/rt_plot_2_proportions_test__title_caption.png',
                   rt_plot_2_proportions_test(prop_1, prop_2, categories,
                                              confidence_level=0.90,
                                              axes_flip = FALSE,
                                              title="My Title",
                                              caption="My caption"
                   ))


    test_save_plot(file_name='data/rt_plot_2_proportions_test__not_stat_sig.png',
                   rt_plot_2_proportions_test(c(15, 100), c(20, 100), categories,
                                              confidence_level=0.95,
                                              axes_flip = FALSE,
                                              title=NULL,
                                              caption=NULL
                   ))

    test_save_plot(file_name='data/rt_plot_2_proportions_test__flip.png',
                   rt_plot_2_proportions_test(prop_1, prop_2, categories,
                                              #confidence_level=0.95,
                                              axes_flip = TRUE,
                                              title=NULL,
                                              caption=NULL
                   ))

    test_save_plot(file_name='data/rt_plot_2_proportions_test__90_conf__flip.png',
                   rt_plot_2_proportions_test(prop_1, prop_2, categories,
                                              confidence_level=0.90,
                                              axes_flip = TRUE,
                                              title=NULL,
                                              caption=NULL
                   ))

    test_save_plot(file_name='data/rt_plot_2_proportions_test__title_caption__flip.png',
                   rt_plot_2_proportions_test(prop_1, prop_2, categories,
                                              confidence_level=0.90,
                                              axes_flip = TRUE,
                                              title="My Title",
                                              caption="My caption"
                   ))


    test_save_plot(file_name='data/rt_plot_2_proportions_test__not_stat_sig__flip.png',
                   rt_plot_2_proportions_test(c(15, 100), c(20, 100), categories,
                                              confidence_level=0.95,
                                              axes_flip = TRUE,
                                              title=NULL,
                                              caption=NULL
                   ))

    # should NOT be stat sig at .95 conf-level
    #prop.test(c(15, 26), c(100, 100))
    categories <- c("Category A", "Category B")
    test_save_plot(file_name='data/rt_plot_2_proportions_test__check_stat_sig__false.png',
                   rt_plot_2_proportions_test(c(15, 100), c(26, 100), categories,
                                              confidence_level=0.95,
                                              axes_flip = TRUE,
                                              title=NULL,
                                              caption=NULL
                   ))
    # should be stat sig at .90 conf-level
    test_save_plot(file_name='data/rt_plot_2_proportions_test__check_stat_sig__true.png',
                   rt_plot_2_proportions_test(c(15, 100), c(26, 100), categories,
                                              confidence_level=0.90,
                                              axes_flip = TRUE,
                                              title=NULL,
                                              caption=NULL
                   ))
})

test_that('rt_plot_multinom_cis', {
    credit_data <- read.csv("data/credit.csv", header=TRUE)

    custom_levels <- c('< 0 DM', '1 - 200 DM', '> 200 DM', 'unknown')
    credit_data$checking_balance <- factor(credit_data$checking_balance, levels=custom_levels)

    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA
    credit_data[2, 'default'] <- NA
    variable <- 'checking_balance'

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=NULL,
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__simple.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=NULL,
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             simple_mode=TRUE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))


    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$default,
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))

    # simple shouldn't do anything when using groups
    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default__simple.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$default,
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             simple_mode=TRUE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default__within.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$default,
                                             ci_within_variable=TRUE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default__within__flip.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$default,
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=TRUE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))



    credit_data$checking_balance <- factor(credit_data$checking_balance,
                                           levels=c("< 0 DM", "1 - 200 DM", "> 200 DM", "unknown"),
                                           ordered=TRUE)

    # change the order of the secondary/comparison variable
    credit_data$default <- factor(credit_data$default,
                                  levels=c("yes", "no"),
                                  ordered=TRUE)

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default_fac.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$default,
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default_fac__within.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$default,
                                             ci_within_variable=TRUE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default_fac__within__flip.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$default,
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=TRUE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"))


})

test_that('rt_plot_multinom_cis__facet', {
    credit_data <- read.csv("data/credit.csv", header=TRUE)

    custom_levels <- c('< 0 DM', '1 - 200 DM', '> 200 DM', 'unknown')
    credit_data$checking_balance <- factor(credit_data$checking_balance, levels=custom_levels)

    # make sure it handles NAs
    credit_data[1, 'checking_balance'] <- NA
    credit_data[2, 'default'] <- NA
    variable <- 'checking_balance'

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__facet.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=NULL,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8, 10))


    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__facet__simple.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=NULL,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             simple_mode=TRUE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8, 10))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default__facet.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$credit_history,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8, 10))

    # simple should not do anything
    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default__facet__simple.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$credit_history,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             simple_mode=TRUE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8, 10))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default__within__facet.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$credit_history,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=TRUE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8, 10))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default__within__flip__facet.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$credit_history,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=TRUE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8, 10))

    credit_data$checking_balance <- factor(credit_data$checking_balance,
                                           levels=c("< 0 DM", "1 - 200 DM", "> 200 DM", "unknown"),
                                           ordered=TRUE)

    # change the order of the secondary/comparison variable
    credit_data$default <- factor(credit_data$default,
                                  levels=c("yes", "no"),
                                  ordered=TRUE)

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default_fac__facet.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$credit_history,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8,10))

    test_save_plot(file_name='data/rt_plot_multinom_cis__checking_balance__default_fac__within__facet.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$credit_history,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=TRUE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=FALSE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8, 10))

    test_save_plot(file_name='data/rt_plot_multinom_cis__default_fac__within__flip__facet.png',
                   plot=rt_plot_multinom_cis(values=credit_data$checking_balance,
                                             groups=credit_data$credit_history,
                                             facets=credit_data$default,
                                             facet_variable_name='default',
                                             ci_within_variable=FALSE,
                                             confidence_level = 0.95,
                                             show_confidence_values=TRUE,
                                             axes_flip=TRUE,
                                             axis_limits=NULL,
                                             text_size=4,
                                             line_size=0.35,
                                             base_size=11,
                                             x_label="Custom X",
                                             y_label="Custom Y",
                                             group_name="Group Name",
                                             title="My Title",
                                             subtitle="My Subtitle",
                                             caption="My Caption"),
                   size_inches = c(8, 10))
})
