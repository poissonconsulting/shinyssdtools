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

test_that("{shinytest2} recording: demo-data", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "demo-data",
    height = 916,
    width = 1382
  )
  app$set_inputs(selectUnit = "")
  app$set_inputs(
    viewUpload_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    viewUpload_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    viewUpload_state = c(
      1752888164814,
      0,
      10,
      "",
      TRUE,
      FALSE,
      TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE)
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_window_size(width = 1382, height = 916)
  app$click("demoData")
  app$set_inputs(
    viewUpload_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    viewUpload_rows_all = c(
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    viewUpload_state = c(
      1752888183451,
      0,
      10,
      "",
      TRUE,
      FALSE,
      TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE)
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_window_size(width = 1382, height = 916)
  app$set_inputs(main_nav = "fit")
  app$set_inputs(
    waiter_shown = TRUE,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    waiter_shown = TRUE,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_window_size(width = 1382, height = 916)
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(main_nav = "predict")
  app$set_inputs(
    waiter_shown = TRUE,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_window_size(width = 1382, height = 916)
  app$set_inputs(
    waiter_shown = TRUE,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    waiter_shown = TRUE,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    waiter_shown = TRUE,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_window_size(width = 1382, height = 916)
  app$expect_values()
  app$expect_screenshot()
})
