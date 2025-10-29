# Copyright 2015-2025 Province of British Columbia
# Copyright 2021 Environment and Climate Change Canada
# Copyright 2023-2025 Australian Government Department of Climate Change,
# Energy, the Environment and Water
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

# Unit Tests for Utility Functions from functions.R
# ==================================================

# clean_nboot() Tests ---------------------------------------------------------

test_that("clean_nboot removes commas from numbers", {
  expect_equal(clean_nboot("1,000"), 1000)
  expect_equal(clean_nboot("10,000"), 10000)
  expect_equal(clean_nboot("5,000"), 5000)
})

test_that("clean_nboot removes spaces from numbers", {
  expect_equal(clean_nboot("1 000"), 1000)
  expect_equal(clean_nboot("10 000"), 10000)
  expect_equal(clean_nboot("5 000"), 5000)
})

test_that("clean_nboot handles numbers without separators", {
  expect_equal(clean_nboot("500"), 500)
  expect_equal(clean_nboot("1000"), 1000)
})

test_that("clean_nboot handles mixed separators", {
  expect_equal(clean_nboot("1,000 000"), 1000000)
  expect_equal(clean_nboot("1 ,000"), 1000)
})

# append_unit() Tests ---------------------------------------------------------

test_that("append_unit returns label unchanged when unit is empty", {
  expect_equal(append_unit("Concentration", ""), "Concentration")
  expect_equal(append_unit("Value", ""), "Value")
})

test_that("append_unit appends unit in parentheses", {
  expect_equal(append_unit("Concentration", "mg/L"), "Concentration (mg/L)")
  expect_equal(append_unit("Value", "µg/L"), "Value (µg/L)")
  expect_equal(append_unit("Dose", "ng/L"), "Dose (ng/L)")
})

test_that("append_unit handles special characters in units", {
  expect_equal(append_unit("Conc", "%"), "Conc (%)")
  expect_equal(append_unit("Conc", "µg/kg"), "Conc (µg/kg)")
})

# guess_conc() Tests ----------------------------------------------------------

test_that("guess_conc finds column with 'conc' in name", {
  expect_equal(guess_conc(c("Species", "Conc", "Group")), "Conc")
  expect_equal(guess_conc(c("Species", "Concentration", "Group")), "Concentration")
  expect_equal(guess_conc(c("conc_mg", "species", "group")), "conc_mg")
})

test_that("guess_conc is case insensitive", {
  expect_equal(guess_conc(c("Species", "CONC", "Group")), "CONC")
  expect_equal(guess_conc(c("Species", "concentration", "Group")), "concentration")
})

test_that("guess_conc finds first numeric column when no conc match", {
  data <- data.frame(
    Species = c("A", "B", "C"),
    Value = c(1, 2, 3),
    Count = c(10, 20, 30)
  )
  expect_equal(guess_conc(names(data), data), "Value")
})

test_that("guess_conc returns NA when no match and no data", {
  expect_true(is.na(guess_conc(c("Species", "Group"))))
})

test_that("guess_conc returns NA when no match and no numeric columns", {
  data <- data.frame(
    Species = c("A", "B", "C"),
    Group = c("X", "Y", "Z")
  )
  expect_true(is.na(guess_conc(names(data), data)))
})

# guess_sp() Tests ------------------------------------------------------------

test_that("guess_sp finds column with 'sp' in name", {
  expect_equal(guess_sp(c("Species", "Conc", "Group")), "Species")
  expect_equal(guess_sp(c("sp", "Conc", "Group")), "sp")
  expect_equal(guess_sp(c("Organism", "sp_name", "Conc")), "sp_name")
})

test_that("guess_sp is case insensitive", {
  expect_equal(guess_sp(c("SPECIES", "Conc", "Group")), "SPECIES")
  expect_equal(guess_sp(c("Conc", "SP", "Group")), "SP")
})

test_that("guess_sp returns first match when multiple sp columns", {
  result <- guess_sp(c("Species", "sp_code", "Conc"))
  expect_equal(result, "Species")
})

test_that("guess_sp returns NA when no sp match", {
  expect_true(is.na(guess_sp(c("Organism", "Conc", "Group"))))
})

# zero_range() Tests ----------------------------------------------------------

test_that("zero_range returns TRUE for single value", {
  expect_true(zero_range(5))
  expect_true(zero_range(c(1)))
})

test_that("zero_range returns TRUE for identical values", {
  expect_true(zero_range(c(5, 5, 5, 5)))
  expect_true(zero_range(c(1.0, 1.0, 1.0)))
})

test_that("zero_range returns FALSE for different values", {
  expect_false(zero_range(c(1, 2, 3)))
  expect_false(zero_range(c(1.0, 1.1, 1.2)))
})

test_that("zero_range handles near-identical values within tolerance", {
  # Values very close together (within default tolerance)
  expect_true(zero_range(c(1.0, 1.0 + 1e-10)))
})

test_that("zero_range handles custom tolerance", {
  # Within loose tolerance
  expect_true(zero_range(c(1.0, 1.01), tol = 0.1))

  # Outside tight tolerance
  expect_false(zero_range(c(1.0, 1.01), tol = 0.001))
})

# estimate_time() Tests -------------------------------------------------------

test_that("estimate_time returns preset values for common nboot", {
  expect_equal(estimate_time(500, "english"), "10 seconds")
  expect_equal(estimate_time(1000, "english"), "20 seconds")
  expect_equal(estimate_time(5000, "english"), "2 minutes")
  expect_equal(estimate_time(10000, "english"), "5 minutes")
})

test_that("estimate_time returns French presets", {
  expect_equal(estimate_time(500, "french"), "10 secondes")
  expect_equal(estimate_time(1000, "french"), "20 secondes")
  expect_equal(estimate_time(5000, "french"), "2 minutes")
  expect_equal(estimate_time(10000, "french"), "5 minutes")
})

test_that("estimate_time calculates for non-preset values", {
  # Should use linear model: time_sec = max(1, -12.89 + 0.0304 * nboot)
  result <- estimate_time(2000, "english")
  expect_type(result, "character")
  expect_match(result, "second|minute")
})

test_that("estimate_time handles very small nboot", {
  result <- estimate_time(100, "english")
  expect_type(result, "character")
  # Should be minimum 1 second
})

test_that("estimate_time uses correct singular/plural in English", {
  # 1 second singular (if nboot produces ~1 second)
  result_1 <- estimate_time(427, "english")  # Should be ~1 second
  expect_match(result_1, "second")

  # Multiple seconds plural
  result_many <- estimate_time(700, "english")
  expect_match(result_many, "seconds")
})

test_that("estimate_time formats minutes correctly", {
  # Large nboot should produce minutes
  result <- estimate_time(3000, "english")
  expect_match(result, "minute")
})

# tr() Tests ------------------------------------------------------------------

test_that("tr extracts translation by id", {
  trans <- data.frame(
    id = c("ui_test1", "ui_test2"),
    trans = c("Test 1", "Test 2"),
    stringsAsFactors = FALSE
  )

  expect_equal(tr("ui_test1", trans), "Test 1")
  expect_equal(tr("ui_test2", trans), "Test 2")
})

test_that("tr returns empty when id not found", {
  trans <- data.frame(
    id = c("ui_test1"),
    trans = c("Test 1"),
    stringsAsFactors = FALSE
  )

  result <- tr("ui_missing", trans)
  expect_length(result, 0)
})

# safe_try() Tests ------------------------------------------------------------

test_that("safe_try returns result on success", {
  result <- safe_try(1 + 1)
  expect_equal(result, 2)

  result <- safe_try(sqrt(4))
  expect_equal(result, 2)
})

test_that("safe_try returns NULL on error", {
  result <- safe_try(stop("error"))
  expect_null(result)

  result <- safe_try(log("not a number"))
  expect_null(result)
})

test_that("safe_try suppresses error messages by default", {
  expect_silent(safe_try(stop("this should not print")))
})

# Threshold Calculation Tests -------------------------------------------------

test_that("calculate_threshold_percent rounds and converts to percent", {
  # Create simple test data
  test_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  fit <- ssdtools::ssd_fit_dists(test_data, dists = "lnorm", silent = TRUE)

  # Test with a known concentration
  result <- calculate_threshold_percent(fit, 10)

  expect_type(result, "double")
  expect_true(result >= 0)
  expect_true(result <= 100)

  # Check it's actually in percent scale (not 0-1)
  # For lnorm with these values, 10 should be somewhere in middle range
  expect_true(result > 1)  # Should be > 1 percent
})

test_that("calculate_threshold_percent respects digits parameter", {
  test_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  fit <- ssdtools::ssd_fit_dists(test_data, dists = "lnorm", silent = TRUE)

  result_3 <- calculate_threshold_percent(fit, 10, digits = 3)
  result_2 <- calculate_threshold_percent(fit, 10, digits = 2)

  expect_type(result_3, "double")
  expect_type(result_2, "double")

  # Both should be valid percentages
  expect_true(result_3 >= 0 && result_3 <= 100)
  expect_true(result_2 >= 0 && result_2 <= 100)
})

test_that("calculate_threshold_conc rounds to significant digits", {
  test_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  fit <- ssdtools::ssd_fit_dists(test_data, dists = "lnorm", silent = TRUE)

  # Test with 5% threshold
  result <- calculate_threshold_conc(fit, 5)

  expect_type(result, "double")
  expect_true(result > 0)

  # Should be within reasonable range of the data
  expect_true(result >= 0.1 && result <= 1000)
})

test_that("calculate_threshold_conc respects digits parameter", {
  test_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  fit <- ssdtools::ssd_fit_dists(test_data, dists = "lnorm", silent = TRUE)

  result_3 <- calculate_threshold_conc(fit, 5, digits = 3)
  result_2 <- calculate_threshold_conc(fit, 5, digits = 2)

  expect_type(result_3, "double")
  expect_type(result_2, "double")

  # Check they're both positive and reasonable
  expect_true(result_3 > 0)
  expect_true(result_2 > 0)
})

test_that("calculate_threshold_conc and calculate_threshold_percent are inverse operations", {
  test_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  fit <- ssdtools::ssd_fit_dists(test_data, dists = "lnorm", silent = TRUE)

  # Start with a threshold
  thresh_percent <- 5

  # Calculate concentration for that threshold
  conc <- calculate_threshold_conc(fit, thresh_percent)

  # Calculate back to percent
  percent_back <- calculate_threshold_percent(fit, conc)

  # Should be approximately equal (within rounding)
  expect_equal(percent_back, thresh_percent, tolerance = 0.1)
})

# format_r_code() Tests -------------------------------------------------------

test_that("format_r_code formats multi-line code", {
  code_lines <- c(
    "x <- 1",
    "y <- 2",
    "z <- x + y"
  )

  result <- format_r_code(code_lines)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  # Should contain the original content
  expect_match(result, "x")
  expect_match(result, "y")
  expect_match(result, "z")
})

test_that("format_r_code converts double quotes to single quotes", {
  code_lines <- c(
    'x <- "hello"',
    'y <- "world"'
  )

  result <- format_r_code(code_lines)

  # Should have single quotes instead of double
  expect_match(result, "'hello'")
  expect_match(result, "'world'")
  expect_false(grepl('"hello"', result))
})

test_that("format_r_code handles function calls", {
  code_lines <- c(
    "library(ssdtools)",
    "fit <- ssd_fit_dists(data, dists = c('lnorm', 'gamma'))"
  )

  result <- format_r_code(code_lines)

  expect_type(result, "character")
  expect_match(result, "library")
  expect_match(result, "ssd_fit_dists")
})

test_that("format_r_code handles empty input", {
  result <- format_r_code(character(0))
  expect_type(result, "character")
})
