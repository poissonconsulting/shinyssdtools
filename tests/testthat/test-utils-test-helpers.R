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

# Essential Tests for Helper Functions
# =====================================

# dt_options() ----------------------------------------------------------------

test_that("dt_options returns valid structure for English", {
  opts <- dt_options("english")

  expect_type(opts, "list")
  expect_equal(opts$pageLength, 15)
  expect_equal(opts$lengthMenu, c(10, 15, 25, 50, 100))
  expect_equal(opts$language$search, "Search data:")
})

test_that("dt_options returns French text for French language", {
  opts <- dt_options("french")

  expect_equal(opts$language$search, "Rechercher :")
  expect_equal(
    opts$language$info,
    "Affichage de _START_ à _END_ sur _TOTAL_ entrées"
  )
})

# Plot Helpers ----------------------------------------------------------------

test_that("has_plot_title detects plot title correctly", {
  plot_with_title <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Test Title")

  plot_no_title <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  expect_true(has_plot_title(plot_with_title))
  expect_false(has_plot_title(plot_no_title))
})

# Mock Module Helpers ---------------------------------------------------------

test_that("mock_data_module creates valid reactive structure", {
  mock <- mock_data_module()

  expect_type(mock, "list")
  expect_true("data" %in% names(mock))
  expect_true("has_data" %in% names(mock))
  expect_true("toxicant_name" %in% names(mock))
  expect_true(shiny::is.reactive(mock$data))
  expect_true(shiny::is.reactive(mock$has_data))
})

test_that("mock_fit_module creates valid reactive structure", {
  mock <- mock_fit_module()

  expect_type(mock, "list")
  expect_true("fit_dist" %in% names(mock))
  expect_true("has_fit" %in% names(mock))
  expect_true(shiny::is.reactive(mock$fit_dist))

  fit <- shiny::isolate(mock$fit_dist())
  expect_s3_class(fit, "fitdists")
})

# Snapshot Helpers ------------------------------------------------------------

test_that("save_csv creates valid CSV file", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  path <- save_csv(df)

  expect_true(file.exists(path))
  expect_true(grepl("\\.csv$", path))

  df_read <- as.data.frame(readr::read_csv(path, show_col_types = FALSE))
  expect_equal(df_read, df)
})
