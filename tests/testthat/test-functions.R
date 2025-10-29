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

# Unit Tests for Helper Functions
# =================================
# Tests for pure functions that don't require Shiny reactive context

# dt_options() Tests ----------------------------------------------------------

test_that("dt_options returns valid structure with English language", {
  opts <- dt_options("english")

  # Check required top-level elements
  expect_type(opts, "list")
  expect_true("pageLength" %in% names(opts))
  expect_true("lengthMenu" %in% names(opts))
  expect_true("language" %in% names(opts))

  # Check values
  expect_equal(opts$pageLength, 15)
  expect_equal(opts$lengthMenu, c(10, 15, 25, 50, 100))
  expect_true(opts$searchHighlight)
})

test_that("dt_options returns English text for english language", {
  opts <- dt_options("english")
  lang <- opts$language

  expect_equal(lang$search, "Search data:")
  expect_equal(lang$lengthMenu, "Show _MENU_ entries")
  expect_equal(lang$info, "Showing _START_ to _END_ of _TOTAL_ entries")
  expect_equal(lang$infoEmpty, "No data available")
  expect_equal(lang$processing, "Processing...")
})

test_that("dt_options returns French text for french language", {
  opts <- dt_options("french")
  lang <- opts$language

  expect_equal(lang$search, "Rechercher :")
  expect_equal(lang$lengthMenu, "Afficher _MENU_ entrées")
  expect_equal(lang$info, "Affichage de _START_ à _END_ sur _TOTAL_ entrées")
  expect_equal(lang$infoEmpty, "Aucune donnée disponible")
  expect_equal(lang$processing, "Traitement en cours...")
})

test_that("dt_options pagination settings are consistent across languages", {
  opts_en <- dt_options("english")
  opts_fr <- dt_options("french")

  # Pagination settings should be identical
  expect_equal(opts_en$pageLength, opts_fr$pageLength)
  expect_equal(opts_en$lengthMenu, opts_fr$lengthMenu)
  expect_equal(opts_en$searchHighlight, opts_fr$searchHighlight)
  expect_equal(opts_en$columnDefs, opts_fr$columnDefs)
})

test_that("dt_options handles default language (no argument)", {
  opts <- dt_options()
  lang <- opts$language

  # Should default to English
  expect_equal(lang$search, "Search data:")
  expect_equal(lang$processing, "Processing...")
})

# Helper Function Tests from helpers.R ----------------------------------------

test_that("has_plot_title detects plot title correctly", {
  # Create plot with title
  plot_with_title <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Test Title")

  # Create plot without title
  plot_no_title <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  expect_true(has_plot_title(plot_with_title))
  expect_false(has_plot_title(plot_no_title))
})

test_that("get_plot_title extracts title correctly", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("My Title")

  expect_equal(get_plot_title(plot), "My Title")
})

test_that("compare_data_frames detects identical data frames", {
  df1 <- data.frame(a = 1:3, b = c("x", "y", "z"))
  df2 <- data.frame(a = 1:3, b = c("x", "y", "z"))

  expect_true(compare_data_frames(df1, df2))
})

test_that("compare_data_frames detects different data frames", {
  df1 <- data.frame(a = 1:3, b = c("x", "y", "z"))
  df2 <- data.frame(a = 1:3, b = c("x", "y", "w"))

  expect_false(compare_data_frames(df1, df2))
})

test_that("compare_data_frames handles numeric tolerance", {
  df1 <- data.frame(x = c(1.0, 2.0, 3.0))
  df2 <- data.frame(x = c(1.00000001, 2.00000001, 3.00000001))

  # Should match with default tolerance (diff is 1e-8, default tolerance is 1e-7)
  expect_true(compare_data_frames(df1, df2))

  # Should not match with very strict tolerance
  expect_false(compare_data_frames(df1, df2, tolerance = 1e-10))
})

test_that("compare_data_frames detects different column names", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 1:3)

  expect_false(compare_data_frames(df1, df2))
})

test_that("compare_data_frames detects different row counts", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(a = 1:4)

  expect_false(compare_data_frames(df1, df2))
})

# Mock Module Helper Tests ----------------------------------------------------

test_that("mock_data_module creates valid structure", {
  mock <- mock_data_module()

  # Check all required elements exist
  expect_type(mock, "list")
  expect_true("data" %in% names(mock))
  expect_true("has_data" %in% names(mock))
  expect_true("toxicant_name" %in% names(mock))
  expect_true("conc" %in% names(mock))
  expect_true("group" %in% names(mock))
  expect_true("units" %in% names(mock))

  # Check reactives return expected types
  expect_true(shiny::is.reactive(mock$data))
  expect_true(shiny::is.reactive(mock$has_data))
  expect_true(shiny::is.reactive(mock$toxicant_name))
})

test_that("mock_data_module uses custom data", {
  custom_data <- data.frame(Conc = 1:5, Value = rnorm(5))
  mock <- mock_data_module(data = custom_data)

  # Test within reactive context using isolate
  expect_equal(shiny::isolate(mock$data()), custom_data)
  expect_true(shiny::isolate(mock$has_data()))
})

test_that("mock_data_module uses custom toxicant name", {
  mock <- mock_data_module(toxicant_name = "Boron")

  expect_equal(shiny::isolate(mock$toxicant_name()), "Boron")
})

test_that("mock_fit_module creates valid structure", {
  mock <- mock_fit_module()

  expect_type(mock, "list")
  expect_true("fit_dist" %in% names(mock))
  expect_true("has_fit" %in% names(mock))
  expect_true("title" %in% names(mock))

  expect_true(shiny::is.reactive(mock$fit_dist))
  expect_true(shiny::is.reactive(mock$has_fit))
  expect_true(shiny::is.reactive(mock$title))
})

test_that("mock_fit_module creates default fit", {
  mock <- mock_fit_module()

  fit <- shiny::isolate(mock$fit_dist())
  expect_s3_class(fit, "fitdists")
  expect_true(shiny::isolate(mock$has_fit()))
})

test_that("mock_fit_module accepts custom fit object", {
  custom_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  custom_fit <- ssdtools::ssd_fit_dists(
    custom_data,
    dists = c("lnorm", "gamma")
  )

  mock <- mock_fit_module(fit = custom_fit)

  fit <- shiny::isolate(mock$fit_dist())
  expect_equal(length(fit), 2)
  expect_equal(names(fit), c("lnorm", "gamma"))
})

test_that("mock_fit_module uses custom title", {
  mock <- mock_fit_module(title = "Test Chemical")

  expect_equal(shiny::isolate(mock$title()), "Test Chemical")
})

# Snapshot Helper Tests -------------------------------------------------------

test_that("save_csv creates valid CSV file", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  path <- save_csv(df)

  expect_true(file.exists(path))
  expect_true(grepl("\\.csv$", path))

  # Read back and verify (convert tibble to data.frame for comparison)
  df_read <- as.data.frame(readr::read_csv(path, show_col_types = FALSE))
  expect_equal(df_read, df)
})

test_that("save_rds creates valid RDS file", {
  obj <- list(a = 1:3, b = "test")
  path <- save_rds(obj)

  expect_true(file.exists(path))
  expect_true(grepl("\\.rds$", path))

  # Read back and verify
  obj_read <- readRDS(path)
  expect_equal(obj_read, obj)
})

test_that("set_test_seed sets reproducible seed", {
  set_test_seed(123)
  r1 <- rnorm(5)

  set_test_seed(123)
  r2 <- rnorm(5)

  expect_equal(r1, r2)
})
