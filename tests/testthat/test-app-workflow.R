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

library(shinytest2)

test_that("App loads successfully", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "app_loads",
    height = 1080,
    width = 1920,
    wait = TRUE,
    timeout = 20000
  )

  # Check that the app loaded
  expect_true(app$get_js("!!document.querySelector('.navbar')"))

  # Check main navigation exists
  expect_true(app$get_js("!!document.getElementById('main_nav')"))

  app$stop()
})

test_that("Demo data loads and displays correctly", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "demo_data_workflow",
    height = 1080,
    width = 1920,
    wait = TRUE,
    timeout = 20000
  )

  # Click demo data button
  app$click("data_mod-demoData")
  app$wait_for_idle(timeout = 5000)

  # Verify data is loaded by checking has_data output
  has_data <- app$get_value(output = "data_mod-has_data")
  expect_true(has_data)

  # Check that data preview table exists
  expect_true(app$get_js("!!document.getElementById('data_mod-viewUpload')"))

  app$stop()
})

test_that("Fit workflow completes successfully with demo data", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "fit_workflow",
    height = 1080,
    width = 1920,
    wait = TRUE,
    timeout = 20000
  )

  # Load demo data
  app$click("data_mod-demoData")
  app$wait_for_idle(timeout = 5000)

  # Navigate to Fit tab
  app$set_inputs(`main_nav` = "fit")
  app$wait_for_idle(timeout = 3000)

  # Verify fit tab is active
  nav_value <- app$get_value(input = "main_nav")
  expect_equal(nav_value, "fit")

  # Wait for fit to auto-update (app auto-fits on tab navigation)
  app$wait_for_idle(timeout = 10000)

  # Check that fit completed by verifying has_fit output
  has_fit <- app$get_value(output = "fit_mod-has_fit")
  expect_true(has_fit)

  # Verify plot exists
  expect_true(app$get_js("!!document.getElementById('fit_mod-plotDist')"))

  # Verify goodness of fit table exists
  expect_true(app$get_js("!!document.getElementById('fit_mod-tableGof')"))

  # Verify input values
  conc_input <- app$get_value(input = "fit_mod-selectConc")
  expect_equal(conc_input, "Conc")

  dist_input <- app$get_value(input = "fit_mod-selectDist")
  expect_true(all(c("gamma", "lgumbel", "llogis", "lnorm") %in% dist_input))

  app$stop()
})

test_that("Predict workflow generates model average plot", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "predict_workflow",
    height = 1080,
    width = 1920,
    wait = TRUE,
    timeout = 20000
  )

  # Load demo data
  app$click("data_mod-demoData")
  app$wait_for_idle(timeout = 5000)

  # Navigate to Fit tab and let it auto-fit
  app$set_inputs(`main_nav` = "fit")
  app$wait_for_idle(timeout = 10000)

  # Navigate to Predict tab
  app$set_inputs(`main_nav` = "predict")
  app$wait_for_idle(timeout = 5000)

  # Verify predict tab is active
  nav_value <- app$get_value(input = "main_nav")
  expect_equal(nav_value, "predict")

  # Wait for prediction to generate
  app$wait_for_idle(timeout = 10000)

  # Check that prediction completed
  has_predict <- app$get_value(output = "predict_mod-has_predict")
  expect_true(has_predict)

  # Verify model average plot exists
  expect_true(app$get_js("!!document.getElementById('predict_mod-plotPred')"))

  # Verify input values exist
  thresh_input <- app$get_value(input = "predict_mod-thresh")
  expect_type(thresh_input, "character")
  expect_true(nchar(thresh_input) > 0)

  thresh_type <- app$get_value(input = "predict_mod-threshType")
  expect_type(thresh_type, "character")

  app$stop()
})

test_that("Complete workflow: Data -> Fit -> Predict -> R Code", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "complete_workflow",
    height = 1080,
    width = 1920,
    wait = TRUE,
    timeout = 20000
  )

  # 1. Load demo data
  app$click("data_mod-demoData")
  app$wait_for_idle(timeout = 5000)

  # 2. Navigate to Fit
  app$set_inputs(`main_nav` = "fit")
  app$wait_for_idle(timeout = 10000)

  # 3. Navigate to Predict
  app$set_inputs(`main_nav` = "predict")
  app$wait_for_idle(timeout = 10000)

  # 4. Navigate to R Code tab
  app$set_inputs(`main_nav` = "rcode")
  app$wait_for_idle(timeout = 3000)

  # Verify R code tab is active
  nav_value <- app$get_value(input = "main_nav")
  expect_equal(nav_value, "rcode")

  # Check that code output exists
  has_code <- app$get_value(output = "rcode_mod-has_code")
  expect_true(has_code)

  # Verify code container exists
  expect_true(app$get_js(
    "!!document.getElementById('rcode_mod-code-container')"
  ))

  app$stop()
})

test_that("Toxicant name propagates across modules", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "toxicant_name",
    height = 1080,
    width = 1920,
    wait = TRUE,
    timeout = 20000
  )

  # Load demo data
  app$click("data_mod-demoData")
  app$wait_for_idle(timeout = 5000)

  # Set toxicant name
  app$set_inputs(`data_mod-toxicant` = "Test Chemical")
  app$wait_for_idle(timeout = 2000)

  # Navigate to Fit and check title input
  app$set_inputs(`main_nav` = "fit")
  app$wait_for_idle(timeout = 10000)

  fit_title <- app$get_value(input = "fit_mod-title")
  expect_equal(fit_title, "Test Chemical")

  # Navigate to Predict and check title input
  app$set_inputs(`main_nav` = "predict")
  app$wait_for_idle(timeout = 10000)

  predict_title <- app$get_value(input = "predict_mod-title")
  expect_equal(predict_title, "Test Chemical")

  app$stop()
})
