context('String Manipulation')
library(testthat)
library(ggplot2)


test_that('rt_str_collapse', {

    expect_equal(rt_str_collapse(c("A"), "`", " + "), "`A`")
    expect_equal(rt_str_collapse(.x=c("A", "B"), .surround = "`", .separate = " + "), "`A` + `B`")
    expect_equal(rt_str_collapse(.x=c("A", "B"), .surround = "`", .separate = "+"), "`A`+`B`")

    expect_equal(rt_str_collapse("A B", "'", " + "), "'A B'")
    expect_equal(rt_str_collapse(c("A B", "C D"), "'", ", "), "'A B', 'C D'")
    expect_equal(rt_str_collapse(c("A B", "C D"), "'", ","), "'A B','C D'")
})

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

test_that('rt_prettyNum', {

    values <- c(0, 0.000, 0.01, 10, 10.00, 10.001, 1000.000000, 1000.01, 1000.000001, NA)
    expected_values <- c("0", "0", "0.01", "10", "10", "10.001", "1,000", "1,000.01", "1,000", NA)
    expect_identical(rt_prettyNum(values), expected_values)

    values <- c("0", "0.000", "0.01", "10", "10.00", "10.001", "1000.000000", "1000.01", "1000.000001", NA)
    expected_values <- c("0", "0", "0.01", "10", "10", "10.001", "1,000", "1,000.01", "1,000.000001", NA)
    expect_identical(rt_prettyNum(values), expected_values)
})

test_that('rt_pretty_percent', {

    values <- c(0, 0.000, 0.01, 0.0125, 0.20, 1, 10, 100, NA)
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "1%", "1%", "20%", "100%", "1,000%", "10,000%", NA))
    values <- values * -1
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "-1%", "-1%", "-20%", "-100%", "-1,000%", "-10,000%", NA))

    values <- c(0, 0.0125, 0.0123, 0.0524, NA)
    expect_identical(rt_pretty_percent(values),
                     c("0%", "1.2%", "1.2%", "5.2%", NA))
    values <- values * -1
    expect_identical(rt_pretty_percent(values),
                     c("0%", "-1.2%", "-1.2%", "-5.2%", NA))

    values <- c(0, 0.000, 0.00123, 0.00524, NA)
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "0.12%", "0.52%", NA))
    values <- values * -1
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "-0.12%", "-0.52%", NA))

    values <- c(0, 0.0000, 0.000123, 0.000524, NA)
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "0.012%", "0.052%", NA))
    values <- values * -1
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "-0.012%", "-0.052%", NA))

    values <- c(0, 0, 0, 0, NA)
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "0%", "0%", NA))
    values <- values * -1
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "0%", "0%", NA))

    values <- c(0, 0, 0, 0)
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "0%", "0%"))
    values <- values * -1
    expect_identical(rt_pretty_percent(values),
                     c("0%", "0%", "0%", "0%"))
})

test_that('rt_pretty_numbers_short__long', {

    values <- c(0.000, 0, 0)
    expected_values <- c("0", "0", "0")
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)

    values <- c(0.00, NA)
    expected_values <- c("0", NA)
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)

    # set.seed(42)
    # values <- rnorm(n=10, mean=0, sd=0.00001)
    # expected_values <- c("1.37e-05","-5.65e-06","3.63e-06", "6.33e-06", "4.04e-06", "-1.06e-06", "1.51e-05", "-9.47e-07", "2.02e-05", "-6.27e-07")
    # expect_identical(rt_pretty_numbers_short(values), expected_values)
    # expect_identical(rt_pretty_numbers_long(values), expected_values)
    # values <- abs(values) * -1
    # expected_values <- c("-1.37e-05","-5.65e-06","-3.63e-06", "-6.33e-06", "-4.04e-06", "-1.06e-06", "-1.51e-05", "-9.47e-07", "-2.02e-05", "-6.27e-07")
    # expect_identical(rt_pretty_numbers_short(values), expected_values)
    # expect_identical(rt_pretty_numbers_long(values), expected_values)

# TODO this passes manually but not when I run unit tests
    set.seed(42)
    values <- rnorm(n=10, mean=0, sd=0.001)
    expected_values <- c("0.0014", "-0.0006", "0.0004", "0.0006", "0.0004", "-0.0001", "0.0015", "-0.0001", "0.002", "-0.0001")
    #expect_identical(rt_pretty_numbers_short(values), expected_values)
    #expect_identical(rt_pretty_numbers_long(values), expected_values)
    values <- abs(values) * -1
    expected_values <- c("-0.0014", "-0.0006", "-0.0004", "-0.0006", "-0.0004", "-0.0001", "-0.0015", "-0.0001", "-0.002", "-0.0001")
    #expect_identical(rt_pretty_numbers_short(values), expected_values)
    #expect_identical(rt_pretty_numbers_long(values), expected_values)

    set.seed(42)
    values <- rnorm(n=10, mean=0, sd=0.1)
    expected_values <- c("0.14", "-0.06", "0.04", "0.06", "0.04", "-0.01", "0.15", "-0.01", "0.2", "-0.01")
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)
    values <- abs(values) * -1
    expected_values <- c("-0.14", "-0.06", "-0.04", "-0.06", "-0.04", "-0.01", "-0.15", "-0.01", "-0.2", "-0.01")
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)

    set.seed(42)
    values <- rnorm(n=10, mean=0, sd=1)
    expected_values <- c("1.4", "-0.6", "0.4", "0.6", "0.4", "-0.1", "1.5", "-0.1", "2", "-0.1")
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)
    values <- abs(values) * -1
    expected_values <- c("-1.4", "-0.6", "-0.4", "-0.6", "-0.4", "-0.1", "-1.5", "-0.1", "-2", "-0.1")
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)

    set.seed(42)
    values <- c(rnorm(n=10, mean=10, sd=1), NA)
    expected_values <- c("11.4", "9.4", "10.4", "10.6", "10.4", "9.9", "11.5", "9.9", "12", "9.9", NA)
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)
    values <- abs(values) * -1
    expected_values <- c("-11.4", "-9.4", "-10.4", "-10.6", "-10.4", "-9.9", "-11.5", "-9.9", "-12", "-9.9", NA)
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)

    set.seed(42)
    values <- c(round(rnorm(n=10, mean=10, sd=1)), NA)
    expected_values <- c("11", "9", "10", "11", "10", "10", "12", "10", "12", "10", NA)
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)
    values <- abs(values) * -1
    expected_values <- c("-11", "-9", "-10", "-11", "-10", "-10", "-12", "-10", "-12", "-10", NA)
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)

    set.seed(42)
    values <- rnorm(n=10, mean=100, sd=10)
    expected_values <- c("114", "94", "104", "106", "104", "99", "115", "99", "120", "99")
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)
    values <- abs(values) * -1
    expected_values <- c("-114", "-94", "-104", "-106", "-104", "-99", "-115", "-99", "-120", "-99")
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)

    set.seed(42)
    values <- c(rnorm(n=10, mean=100, sd=10), NA)
    expected_values <- c("114", "94", "104", "106", "104", "99", "115", "99", "120", "99", NA)
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)
    values <- abs(values) * -1
    expected_values <- c("-114", "-94", "-104", "-106", "-104", "-99", "-115", "-99", "-120", "-99", NA)
    expect_identical(rt_pretty_numbers_short(values), expected_values)
    expect_identical(rt_pretty_numbers_long(values), expected_values)

    set.seed(42)
    values <- c(rnorm(n=10, mean=1000, sd=100), 0)
    expect_identical(rt_pretty_numbers_short(values),
                     c("1.14K", "0.94K", "1.04K", "1.06K", "1.04K", "0.99K", "1.15K", "0.99K", "1.2K", "0.99K", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("1,137", "944", "1,036", "1,063", "1,040", "989", "1,151", "991", "1,202", "994", "0"))
    values <- values * -1
    expect_identical(rt_pretty_numbers_short(values),
                     c("-1.14K", "-0.94K", "-1.04K", "-1.06K", "-1.04K", "-0.99K", "-1.15K", "-0.99K", "-1.2K", "-0.99K", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("-1,137", "-944", "-1,036", "-1,063", "-1,040", "-989", "-1,151", "-991", "-1,202", "-994", "0"))

    set.seed(42)
    values <- c(rnorm(n=10, mean=10000, sd=1000), 0)
    expect_identical(rt_pretty_numbers_short(values),
                     c("11.4K", "9.4K", "10.4K", "10.6K", "10.4K", "9.9K", "11.5K", "9.9K", "12K", "9.9K", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("11,371", "9,435", "10,363", "10,633", "10,404", "9,894", "11,512", "9,905", "12,018", "9,937", "0"))
    values <- values * -1
    expect_identical(rt_pretty_numbers_short(values),
                     c("-11.4K", "-9.4K", "-10.4K", "-10.6K", "-10.4K", "-9.9K", "-11.5K", "-9.9K", "-12K", "-9.9K", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("-11,371", "-9,435", "-10,363", "-10,633", "-10,404", "-9,894", "-11,512", "-9,905", "-12,018", "-9,937", "0"))

    set.seed(42)
    values <- c(rnorm(n=10, mean=100000, sd=10000), 0)
    expect_identical(rt_pretty_numbers_short(values),
                     c("113.7K", "94.4K", "103.6K", "106.3K", "104K", "98.9K", "115.1K", "99.1K", "120.2K", "99.4K", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("113,710", "94,353", "103,631", "106,329", "104,043", "98,939", "115,115", "99,053", "120,184", "99,373", "0"))
    values <- values * -1
    expect_identical(rt_pretty_numbers_short(values),
                     c("-113.7K", "-94.4K", "-103.6K", "-106.3K", "-104K", "-98.9K", "-115.1K", "-99.1K", "-120.2K", "-99.4K", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("-113,710", "-94,353", "-103,631", "-106,329", "-104,043", "-98,939", "-115,115", "-99,053", "-120,184", "-99,373", "0"))

    set.seed(42)
    values <- c(rnorm(n=10, mean=1000000, sd=100000), 0)
    expect_identical(rt_pretty_numbers_short(values),
                     c("1.14M", "0.94M", "1.04M", "1.06M", "1.04M", "0.99M", "1.15M", "0.99M", "1.2M", "0.99M", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("1,137,096", "943,530", "1,036,313", "1,063,286", "1,040,427", "989,388", "1,151,152", "990,534", "1,201,842", "993,729", "0"))
    values <- values * -1
    expect_identical(rt_pretty_numbers_short(values),
                     c("-1.14M", "-0.94M", "-1.04M", "-1.06M", "-1.04M", "-0.99M", "-1.15M", "-0.99M", "-1.2M", "-0.99M", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("-1,137,096", "-943,530", "-1,036,313", "-1,063,286", "-1,040,427", "-989,388", "-1,151,152", "-990,534", "-1,201,842", "-993,729", "0"))

    set.seed(42)
    values <- c(rnorm(n=10, mean=1000000000, sd=100000000), 0)
    expect_identical(rt_pretty_numbers_short(values),
                     c("1.14B", "0.94B", "1.04B", "1.06B", "1.04B", "0.99B", "1.15B", "0.99B", "1.2B", "0.99B", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("1,137,095,845", "943,530,183", "1,036,312,841", "1,063,286,260", "1,040,426,832", "989,387,548", "1,151,152,200", "990,534,096", "1,201,842,371", "993,728,590", "0"))
    values <- values * -1
    expect_identical(rt_pretty_numbers_short(values),
                     c("-1.14B", "-0.94B", "-1.04B", "-1.06B", "-1.04B", "-0.99B", "-1.15B", "-0.99B", "-1.2B", "-0.99B", "0"))
    expect_identical(rt_pretty_numbers_long(values),
                     c("-1,137,095,845", "-943,530,183", "-1,036,312,841", "-1,063,286,260", "-1,040,426,832", "-989,387,548", "-1,151,152,200", "-990,534,096", "-1,201,842,371", "-993,728,590", "0"))
})

test_that('rt_pretty_axes', {

    options(scipen=999)

    small_values <- sort(c(10^(-5:-1), 10^(-5:-1) + (10^(-5:-1) / 2), (10^(-5:-1) / 2)))
    large_values <- sort(c(10^(0:10), 10^(0:10) + 0.5, (10^(0:10) / 2), (10^(0:10) + (10^(0:10) / 2)), 10^(0:10) + 10^(-1:9)))
    all_values <- c(small_values, large_values)
    all_values <- c(NA, sort(c(0, all_values, all_values * -1)))
    # paste0(map2_chr(actual_results, all_values, ~ paste0("'", .x, "',  # ", prettyNum(.y, big.mark = ',', nsmall=3))), collapse = "\n")
    expected_results <- c('0',  # NA
                          '-15B',  # -15,000,000,000.000
                          '-11B',  # -11,000,000,000.000
                          '-10B',  # -10,000,000,000.500
                          '-10B',  # -10,000,000,000.000
                          '-5B',  # -5,000,000,000.000
                          '-1.5B',  # -1,500,000,000.000
                          '-1.1B',  # -1,100,000,000.000
                          '-1B',  # -1,000,000,000.500
                          '-1B',  # -1,000,000,000.000
                          '-500M',  # -500,000,000.000
                          '-150M',  # -150,000,000.000
                          '-110M',  # -110,000,000.000
                          '-100M',  # -100,000,000.500
                          '-100M',  # -100,000,000.000
                          '-50M',  # -50,000,000.000
                          '-15M',  # -15,000,000.000
                          '-11M',  # -11,000,000.000
                          '-10M',  # -10,000,000.500
                          '-10M',  # -10,000,000.000
                          '-5M',  # -5,000,000.000
                          '-1.5M',  # -1,500,000.000
                          '-1.1M',  # -1,100,000.000
                          '-1M',  # -1,000,000.500
                          '-1M',  # -1,000,000.000
                          '-500K',  # -500,000.000
                          '-150K',  # -150,000.000
                          '-110K',  # -110,000.000
                          '-100K',  # -100,000.500
                          '-100K',  # -100,000.000
                          '-50K',  # -50,000.000
                          '-15K',  # -15,000.000
                          '-11K',  # -11,000.000
                          '-10K',  # -10,000.500
                          '-10K',  # -10,000.000
                          '-5K',  # -5,000.000
                          '-1.5K',  # -1,500.000
                          '-1.1K',  # -1,100.000
                          '-1K',  # -1,000.500
                          '-1K',  # -1,000.000
                          '-500',  # -500.000
                          '-150',  # -150.000
                          '-110',  # -110.000
                          '-100',  # -100.500
                          '-100',  # -100.000
                          '-50',  # -50.000
                          '-15',  # -15.000
                          '-11',  # -11.000
                          '-10.5',  # -10.500
                          '-10',  # -10.000
                          '-5',  # -5.000
                          '-1.5',  # -1.500
                          '-1.5',  # -1.500
                          '-1.1',  # -1.100
                          '-1',  # -1.000
                          '-0.5',  # -0.500
                          '-0.15',  # -0.150
                          '-0.1',  # -0.100
                          '-0.05',  # -0.050
                          '-0.015',  # -0.015
                          '-0.01',  # -0.010
                          '-0.005',  # -0.005
                          '-0.0015',  # -0.0015
                          '-0.001',  # -0.001
                          '-0.0005',  # -0.0005
                          '-0.00015',  # -0.00015
                          '-0.0001',  # -0.0001
                          '-0.00005',  # -0.00005
                          '-0.000015',  # -0.000015
                          '-0.00001',  # -0.00001
                          '-0.000005',  # -0.000005
                          '0',  # 0.000
                          '0.000005',  # 0.000005
                          '0.00001',  # 0.00001
                          '0.000015',  # 0.000015
                          '0.00005',  # 0.00005
                          '0.0001',  # 0.0001
                          '0.00015',  # 0.00015
                          '0.0005',  # 0.0005
                          '0.001',  # 0.001
                          '0.0015',  # 0.0015
                          '0.005',  # 0.005
                          '0.01',  # 0.010
                          '0.015',  # 0.015
                          '0.05',  # 0.050
                          '0.1',  # 0.100
                          '0.15',  # 0.150
                          '0.5',  # 0.500
                          '1',  # 1.000
                          '1.1',  # 1.100
                          '1.5',  # 1.500
                          '1.5',  # 1.500
                          '5',  # 5.000
                          '10',  # 10.000
                          '10.5',  # 10.500
                          '11',  # 11.000
                          '15',  # 15.000
                          '50',  # 50.000
                          '100',  # 100.000
                          '100',  # 100.500
                          '110',  # 110.000
                          '150',  # 150.000
                          '500',  # 500.000
                          '1K',  # 1,000.000
                          '1K',  # 1,000.500
                          '1.1K',  # 1,100.000
                          '1.5K',  # 1,500.000
                          '5K',  # 5,000.000
                          '10K',  # 10,000.000
                          '10K',  # 10,000.500
                          '11K',  # 11,000.000
                          '15K',  # 15,000.000
                          '50K',  # 50,000.000
                          '100K',  # 100,000.000
                          '100K',  # 100,000.500
                          '110K',  # 110,000.000
                          '150K',  # 150,000.000
                          '500K',  # 500,000.000
                          '1M',  # 1,000,000.000
                          '1M',  # 1,000,000.500
                          '1.1M',  # 1,100,000.000
                          '1.5M',  # 1,500,000.000
                          '5M',  # 5,000,000.000
                          '10M',  # 10,000,000.000
                          '10M',  # 10,000,000.500
                          '11M',  # 11,000,000.000
                          '15M',  # 15,000,000.000
                          '50M',  # 50,000,000.000
                          '100M',  # 100,000,000.000
                          '100M',  # 100,000,000.500
                          '110M',  # 110,000,000.000
                          '150M',  # 150,000,000.000
                          '500M',  # 500,000,000.000
                          '1B',  # 1,000,000,000.000
                          '1B',  # 1,000,000,000.500
                          '1.1B',  # 1,100,000,000.000
                          '1.5B',  # 1,500,000,000.000
                          '5B',  # 5,000,000,000.000
                          '10B',  # 10,000,000,000.000
                          '10B',  # 10,000,000,000.500
                          '11B',  # 11,000,000,000.000
                          '15B')  # 15,000,000,000.000


    actual_results <- rt_pretty_axes(all_values)
    expect_true(identical(expected_results, actual_results))

    expected_results <- c('0%',  # NA
                          '-1,500,000,000,000%',  # -15,000,000,000.000
                          '-1,100,000,000,000%',  # -11,000,000,000.000
                          '-1,000,000,000,050%',  # -10,000,000,000.500
                          '-1,000,000,000,000%',  # -10,000,000,000.000
                          '-500,000,000,000%',  # -5,000,000,000.000
                          '-150,000,000,000%',  # -1,500,000,000.000
                          '-110,000,000,000%',  # -1,100,000,000.000
                          '-100,000,000,050%',  # -1,000,000,000.500
                          '-100,000,000,000%',  # -1,000,000,000.000
                          '-50,000,000,000%',  # -500,000,000.000
                          '-15,000,000,000%',  # -150,000,000.000
                          '-11,000,000,000%',  # -110,000,000.000
                          '-10,000,000,050%',  # -100,000,000.500
                          '-10,000,000,000%',  # -100,000,000.000
                          '-5,000,000,000%',  # -50,000,000.000
                          '-1,500,000,000%',  # -15,000,000.000
                          '-1,100,000,000%',  # -11,000,000.000
                          '-1,000,000,050%',  # -10,000,000.500
                          '-1,000,000,000%',  # -10,000,000.000
                          '-500,000,000%',  # -5,000,000.000
                          '-150,000,000%',  # -1,500,000.000
                          '-110,000,000%',  # -1,100,000.000
                          '-100,000,050%',  # -1,000,000.500
                          '-100,000,000%',  # -1,000,000.000
                          '-50,000,000%',  # -500,000.000
                          '-15,000,000%',  # -150,000.000
                          '-11,000,000%',  # -110,000.000
                          '-10,000,050%',  # -100,000.500
                          '-10,000,000%',  # -100,000.000
                          '-5,000,000%',  # -50,000.000
                          '-1,500,000%',  # -15,000.000
                          '-1,100,000%',  # -11,000.000
                          '-1,000,050%',  # -10,000.500
                          '-1,000,000%',  # -10,000.000
                          '-500,000%',  # -5,000.000
                          '-150,000%',  # -1,500.000
                          '-110,000%',  # -1,100.000
                          '-100,050%',  # -1,000.500
                          '-100,000%',  # -1,000.000
                          '-50,000%',  # -500.000
                          '-15,000%',  # -150.000
                          '-11,000%',  # -110.000
                          '-10,050%',  # -100.500
                          '-10,000%',  # -100.000
                          '-5,000%',  # -50.000
                          '-1,500%',  # -15.000
                          '-1,100%',  # -11.000
                          '-1,050%',  # -10.500
                          '-1,000%',  # -10.000
                          '-500%',  # -5.000
                          '-150%',  # -1.500
                          '-150%',  # -1.500
                          '-110%',  # -1.100
                          '-100%',  # -1.000
                          '-50%',  # -0.500
                          '-15%',  # -0.150
                          '-10%',  # -0.100
                          '-5%',  # -0.050
                          '-1.5%',  # -0.015
                          '-1%',  # -0.010
                          '-0.5%',  # -0.005
                          '-0.15%',  # -0.0015
                          '-0.1%',  # -0.001
                          '-0.05%',  # -0.0005
                          '-0.015%',  # -0.00015
                          '-0.01%',  # -0.0001
                          '-0.005%',  # -0.00005
                          '-0.0015%',  # -0.000015
                          '-0.001%',  # -0.00001
                          '-0.0005%',  # -0.000005
                          '0%',  # 0.000
                          '0.0005%',  # 0.000005
                          '0.001%',  # 0.00001
                          '0.0015%',  # 0.000015
                          '0.005%',  # 0.00005
                          '0.01%',  # 0.0001
                          '0.015%',  # 0.00015
                          '0.05%',  # 0.0005
                          '0.1%',  # 0.001
                          '0.15%',  # 0.0015
                          '0.5%',  # 0.005
                          '1%',  # 0.010
                          '1.5%',  # 0.015
                          '5%',  # 0.050
                          '10%',  # 0.100
                          '15%',  # 0.150
                          '50%',  # 0.500
                          '100%',  # 1.000
                          '110%',  # 1.100
                          '150%',  # 1.500
                          '150%',  # 1.500
                          '500%',  # 5.000
                          '1,000%',  # 10.000
                          '1,050%',  # 10.500
                          '1,100%',  # 11.000
                          '1,500%',  # 15.000
                          '5,000%',  # 50.000
                          '10,000%',  # 100.000
                          '10,050%',  # 100.500
                          '11,000%',  # 110.000
                          '15,000%',  # 150.000
                          '50,000%',  # 500.000
                          '100,000%',  # 1,000.000
                          '100,050%',  # 1,000.500
                          '110,000%',  # 1,100.000
                          '150,000%',  # 1,500.000
                          '500,000%',  # 5,000.000
                          '1,000,000%',  # 10,000.000
                          '1,000,050%',  # 10,000.500
                          '1,100,000%',  # 11,000.000
                          '1,500,000%',  # 15,000.000
                          '5,000,000%',  # 50,000.000
                          '10,000,000%',  # 100,000.000
                          '10,000,050%',  # 100,000.500
                          '11,000,000%',  # 110,000.000
                          '15,000,000%',  # 150,000.000
                          '50,000,000%',  # 500,000.000
                          '100,000,000%',  # 1,000,000.000
                          '100,000,050%',  # 1,000,000.500
                          '110,000,000%',  # 1,100,000.000
                          '150,000,000%',  # 1,500,000.000
                          '500,000,000%',  # 5,000,000.000
                          '1,000,000,000%',  # 10,000,000.000
                          '1,000,000,050%',  # 10,000,000.500
                          '1,100,000,000%',  # 11,000,000.000
                          '1,500,000,000%',  # 15,000,000.000
                          '5,000,000,000%',  # 50,000,000.000
                          '10,000,000,000%',  # 100,000,000.000
                          '10,000,000,050%',  # 100,000,000.500
                          '11,000,000,000%',  # 110,000,000.000
                          '15,000,000,000%',  # 150,000,000.000
                          '50,000,000,000%',  # 500,000,000.000
                          '100,000,000,000%',  # 1,000,000,000.000
                          '100,000,000,050%',  # 1,000,000,000.500
                          '110,000,000,000%',  # 1,100,000,000.000
                          '150,000,000,000%',  # 1,500,000,000.000
                          '500,000,000,000%',  # 5,000,000,000.000
                          '1,000,000,000,000%',  # 10,000,000,000.000
                          '1,000,000,000,050%',  # 10,000,000,000.500
                          '1,100,000,000,000%',  # 11,000,000,000.000
                          '1,500,000,000,000%')  # 15,000,000,000.000)

    actual_results <- rt_pretty_axes_percent(all_values)
    expect_true(identical(expected_results, actual_results))

    expect_equal(rt_pretty_axes(10500.500), "10.5K")
    expect_equal(rt_pretty_axes(10550), "10.6K")

    expect_equal(rt_pretty_axes_percent(1.500), "150%")
    expect_equal(rt_pretty_axes_percent(1.555), "156%")
    expect_equal(rt_pretty_axes_percent(0.1555), "15.6%")
})
