context('String Manipulation')
library(testthat)
library(ggplot2)

test_that("rt_explore_categoric_summary_NAs", {

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
    credit_data <- read.csv("data/credit.csv", header=TRUE)

    values <- colnames(credit_data)
    expected <- c('Checking Balance', 'Months Loan Duration', 'Credit History', 'Purpose', 'Amount',
                  'Savings Balance', 'Employment Duration', 'Percent Of Income' , 'Years At Residence', 'Age',
                  'Other Credit', 'Housing', 'Existing Loans Count', 'Job', 'Dependents', 'Phone', 'Default')
    results <- rt_pretty_text(values)
    expect_true(all(results == expected))

    # factors
    checking_balance <- sort(unique(credit_data$checking_balance))
    expected_checking_balance <- c("< 0 DM", "> 200 DM", "1 - 200 DM", "Unknown")
    expect_true(is.factor(checking_balance))  # make sure we are testing a factor
    results <- rt_pretty_text(checking_balance)
    expect_true(all(results == expected_checking_balance))

    checking_balance_chars <- sort(as.character(unique(credit_data$checking_balance)))  # ensure no difference between character/factor
    expect_false(is.factor(checking_balance_chars))  # make sure we are NOT testing a factor
    results <- rt_pretty_text(checking_balance_chars)
    expect_true(all(results == expected_checking_balance))  # should still give the same thing as before

    employment_duration <- sort(unique(credit_data$employment_duration))
    expect_true(is.factor(employment_duration))  # make sure we are testing a factor
    results <- rt_pretty_text(employment_duration)
    expect_true(all(results == c("< 1 Year", "> 7 Years", "1 - 4 Years", "4 - 7 Years", "Unemployed")))

    savings_balance <- sort(unique(credit_data$savings_balance))
    expect_true(is.factor(savings_balance))  # make sure we are testing a factor
    results <- rt_pretty_text(savings_balance)
    expect_true(all(results == c("< 100 DM", "> 1000 DM", "100 - 500 DM", "500 - 1000 DM", "Unknown")))

    purpose <- sort(unique(credit_data$purpose))
    expect_true(is.factor(purpose))  # make sure we are testing a factor
    results <- rt_pretty_text(purpose)
    expect_true(all(results == c("Business", "Car", "Car0", "Education", "Furniture / Appliances", "Renovations")))
})
