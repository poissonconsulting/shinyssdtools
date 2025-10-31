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

# Visual Regression Tests for Plots
# ====================================
# Tests plot rendering using shinytest2 with actual app

library(shinytest2)

test_that("fit plot with default distributions", {
  app <- AppDriver$new(
    name = "fit-plot-default",
    height = 800,
    width = 1200,
    variant = platform_variant()
  )
  withr::defer(app$stop())

  app$set_inputs(main_nav = "fit")
  app$wait_for_idle()
  app$click("data_mod-demoData")
  app$wait_for_idle()

  # Give plot time to render in browser
  Sys.sleep(1)

  app$wait_for_value(output = "fit_mod-plotDist", timeout = 10000)
  app$expect_screenshot(
    selector = "#fit_mod-plotDist",
    name = "fit-plot-default"
  )
})

test_that("fit plot with different distributions", {
  app <- AppDriver$new(
    name = "fit-plot-multiple-dists",
    height = 800,
    width = 1200,
    variant = platform_variant()
  )
  withr::defer(app$stop())

  app$set_inputs(main_nav = "fit")
  app$wait_for_idle()
  app$click("data_mod-demoData")
  app$wait_for_idle()

  app$set_inputs(
    `fit_mod-selectDist` = c("lnorm", "gamma", "lgumbel", "weibull")
  )
  app$wait_for_idle(500)

  app$expect_screenshot(
    selector = "#fit_mod-plotDist",
    name = "fit-plot-multiple-dists"
  )
})

test_that("predict plot with default settings", {
  app <- AppDriver$new(
    name = "predict-plot-default",
    height = 800,
    width = 1200,
    variant = platform_variant()
  )
  withr::defer(app$stop())

  app$set_inputs(main_nav = "fit")
  app$wait_for_idle()
  app$click("data_mod-demoData")
  app$wait_for_idle()

  app$set_inputs(main_nav = "predict")
  app$wait_for_idle()

  app$expect_screenshot(
    selector = "#predict_mod-plotPred",
    name = "predict-plot-default"
  )
})

test_that("predict plot with different threshold", {
  app <- AppDriver$new(
    name = "predict-plot-thresh-1",
    height = 800,
    width = 1200,
    variant = platform_variant()
  )
  withr::defer(app$stop())

  app$set_inputs(main_nav = "fit")
  app$wait_for_idle()
  app$click("data_mod-demoData")
  app$wait_for_idle()

  app$set_inputs(main_nav = "predict")
  app$wait_for_idle()

  app$set_inputs(`predict_mod-thresh` = "1")
  app$wait_for_idle()

  app$expect_screenshot(
    selector = "#predict_mod-plotPred",
    name = "predict-plot-thresh-1"
  )
})

test_that("predict plot with confidence intervals", {
  app <- AppDriver$new(
    name = "predict-plot-with-ci",
    height = 800,
    width = 1200,
    variant = platform_variant()
  )
  withr::defer(app$stop())

  app$set_inputs(main_nav = "fit")
  app$wait_for_idle()
  app$click("data_mod-demoData")
  app$wait_for_idle()

  app$set_inputs(main_nav = "predict")
  app$wait_for_idle()

  app$set_inputs(`predict_mod-includeCi` = TRUE, wait_ = FALSE)
  # Need to set custom selectizeInput entry
  set_bootstrap_samples(app, "5")
  app$wait_for_idle()

  # wait_ = FALSE avoids timeout waiting for waiter loading screen to complete
  app$click("predict_mod-getCl", wait_ = FALSE)

  # Wait for bootstrap calculation to complete
  app$wait_for_value(output = "predict_mod-has_cl", timeout = 10000)

  app$expect_screenshot(
    selector = "#predict_mod-plotPred",
    name = "predict-plot-with-ci"
  )
})

test_that("predict plot with line style CI", {
  app <- AppDriver$new(
    name = "predict-plot-with-ci-lines",
    height = 800,
    width = 1200,
    variant = platform_variant()
  )
  withr::defer(app$stop())

  app$set_inputs(main_nav = "fit")
  app$wait_for_idle()
  app$click("data_mod-demoData")
  app$wait_for_idle()

  app$set_inputs(main_nav = "predict")
  app$wait_for_idle()

  app$set_inputs(`predict_mod-includeCi` = TRUE)
  set_bootstrap_samples(app, "5")
  app$set_inputs(`predict_mod-ribbonStyle` = "FALSE")
  app$wait_for_idle()

  # wait_ = FALSE avoids timeout waiting for waiter loading screen to complete
  app$click("predict_mod-getCl", wait_ = FALSE)

  # Wait for bootstrap calculation to complete
  app$wait_for_value(output = "predict_mod-has_cl", timeout = 10000)

  app$expect_screenshot(
    selector = "#predict_mod-plotPred",
    name = "predict-plot-with-ci-lines"
  )
})
