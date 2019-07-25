context('String Manipulation')
library(testthat)
library(ggplot2)

test_that('rt_pretty_text', {

    ##########################################################################################################
    # Test Basic Use Cases
    ##########################################################################################################
    values <-   c('asdf', 'Asdf', 'ASDF', 'asdf_adsf', 1,   'asdf ASdf', NA, 'asdf_Asdf ASdF', 'Asdf_ASDF_asdf', NA)
    expected <- c('Asdf', 'Asdf', 'ASDF', 'Asdf Adsf', '1', 'Asdf ASdf', NA, 'Asdf Asdf ASdF', 'Asdf ASDF Asdf', NA)
    results <- rt_pretty_text(values)
    # ensure if expected is NA then results is NA
    expect_true(all(ifelse(is.na(expected), is.na(results), expected == results)))


    values <-   c('abc/xyz',   'Abc/XYZ',   'abc/xYz',   'abc / xyz', 'Abc / XYZ', 'abc / xYz', 'abc /xyz', 'abc/ xyz')
    expected <- c('Abc / Xyz', 'Abc / XYZ', 'Abc / XYz', 'Abc / Xyz', 'Abc / XYZ', 'Abc / XYz', 'Abc /xyz', 'Abc/ Xyz')
    results <- rt_pretty_text(values)
    # ensure if expected is NA then results is NA
    expect_true(all(expected == results))

    ##########################################################################################################
    # Test Cases That Should be Ignored
    ##########################################################################################################
    values <- c(1, 1, NA, 1)
    expected <- values  # should not do anything with numeric values
    results <- rt_pretty_text(values)
    # ensure if expected is NA then results is NA
    expect_true(all(ifelse(is.na(expected), is.na(results), expected == results)))

    values <- c(NA, NA, NA, NA)
    expected <- values  # should not do anything with NA values
    results <- rt_pretty_text(values)
    # ensure if expected is NA then results is NA
    expect_true(all(ifelse(is.na(expected), is.na(results), expected == results)))

    ##########################################################################################################
    # Test Dataset with various values (and factor variables)
    ##########################################################################################################
    values <- c('checking_balance', 'months_loan_duration', 'credit_history', 'purpose', 'amount',
                'savings_balance', 'employment_duration', 'percent_of_income', 'years_at_residence', 'age',
                'other_credit', 'housing', 'existing_loans_count', 'job', 'dependents', 'phone', 'default')
    expected <- c('Checking Balance', 'Months Loan Duration', 'Credit History', 'Purpose', 'Amount',
                  'Savings Balance', 'Employment Duration', 'Percent Of Income' , 'Years At Residence', 'Age',
                  'Other Credit', 'Housing', 'Existing Loans Count', 'Job', 'Dependents', 'Phone', 'Default')
    results <- rt_pretty_text(values)
    expect_true(all(results == expected))

    # factors
    checking_balance_vector <- c('< 0 dm', '> 200 DM', '1 - 200_dm', 'unknown')
    # give factor different order, just to test
    factor_levels=c(checking_balance_vector[1],
                    checking_balance_vector[3],
                    checking_balance_vector[2],
                    checking_balance_vector[4])
    checking_balance <- factor(checking_balance_vector, levels=factor_levels, ordered=TRUE)
    expected_checking_balance <- c('< 0 Dm', '> 200 DM', '1 - 200 Dm', 'Unknown')
    expected_factor_levels <- c("< 0 Dm", "1 - 200 Dm", "> 200 DM", "Unknown")

    expect_true(is.factor(checking_balance))  # make sure we are testing a factor
    expect_true(is.ordered(checking_balance))
    results <- rt_pretty_text(checking_balance)

    expect_identical(as.character(results), expected_checking_balance)
    expect_true(is.factor(results))
    expect_true(is.ordered(results))
    expect_identical(levels(results), expected_factor_levels)

    checking_balance_chars <- checking_balance_vector  # ensure no difference between character/factor
    expect_false(is.factor(checking_balance_chars))  # make sure we are NOT testing a factor
    results <- rt_pretty_text(checking_balance_chars)
    expect_identical(results, expected_checking_balance)  # should still give the same thing as before
    expect_false(is.factor(results))

    purpose_vector <- c('business', 'car', 'car0', 'education', 'furniture/appliances', 'renovations')
    purpose <- factor(purpose_vector)
    expect_true(is.factor(purpose))  # make sure we are testing a factor
    results <- rt_pretty_text(purpose)
    expect_true(is.factor(results))  # make sure we are testing a factor
    expect_false(is.ordered(results))
    expect_identical(as.character(results),
                     c('Business', 'Car', 'Car0', 'Education', 'Furniture / Appliances', 'Renovations'))
})

test_that('rt_pretty_dataset', {

    ugly_data <- read.csv("data/credit.csv", header=TRUE)
    pretty_data <- rt_pretty_dataset(ugly_data)
    expect_true(rt_are_dataframes_equal_from_file(dataframe1=pretty_data, rds_file='data/pretty_dataset.RDS'))
})

test_that('rt_pretty_numerics', {

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=0, sd=0.001)),
                 c("1.37e-03", "-5.65e-04", "3.63e-04", "6.33e-04", "4.04e-04", "-1.06e-04", "1.51e-03", "-9.47e-05", "2.02e-03", "-6.27e-05"))

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=0, sd=0.1)),
                 c(0.14, -0.06, 0.04, 0.06, 0.04, -0.01, 0.15, -0.01, 0.20, -0.01))

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=0)),
                 c(1.4, -0.6, 0.4, 0.6, 0.4, -0.1, 1.5, -0.1, 2.0, -0.1))

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=10)),
                 c(11.4, 9.4, 10.4, 10.6, 10.4, 9.9, 11.5, 9.9, 12.0, 9.9))

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=100, sd=10)),
                 c(114, 94, 104, 106, 104, 99, 115, 99, 120, 99))

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=1000, sd=100)),
                 c("1.14K", "0.94K", "1.04K", "1.06K", "1.04K", "0.99K", "1.15K", "0.99K", "1.2K", "0.99K"))

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=10000, sd=1000)),
                 c("11.4K", "9.4K", "10.4K", "10.6K", "10.4K", "9.9K", "11.5K", "9.9K", "12K", "9.9K"))

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=100000, sd=10000)),
                 c("113.7K", "94.4K", "103.6K", "106.3K", "104K", "98.9K", "115.1K", "99.1K", "120.2K", "99.4K" ))

    set.seed(42)
    expect_equal(rt_pretty_numerics(rnorm(n=10, mean=1000000, sd=100000)),
                 c("1.14M", "0.94M", "1.04M", "1.06M", "1.04M", "0.99M", "1.15M", "0.99M", "1.2M", "0.99M"))
})
