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

# testServer Tests for Fit Module
# =================================
# Tests for reactive logic in mod_fit_server using testServer

# Setup translations
test_translations <- translations
test_translations$trans <- test_translations[["english"]]

# Use actual boron data
test_data <- clean_ssd_data(boron.data)
data_mod <- mock_data_module(data = test_data)

# check required inputs -----------------------------------------------------
test_that("mod_fit_server produces valid fit object with boron data", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      # Set required inputs
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      # Check fit object from module return values
      returned <- session$returned
      fit <- returned$fit_dist()
      expect_s3_class(fit, "fitdists")
      expect_equal(length(fit), 2)
      expect_equal(names(fit), c("lnorm", "gamma"))
    }
  )
})

test_that("mod_fit_server has_fit is TRUE after successful fit", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      returned <- session$returned
      expect_true(returned$has_fit())
    }
  )
})

# Goodness-of-Fit Table Tests -------------------------------------------------

test_that("gof_table produces valid table after fit", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      returned <- session$returned
      gof <- returned$gof_table()
      expect_true(is.data.frame(gof))
      expect_equal(nrow(gof), 2) # Two distributions

      # Check for expected columns
      expect_true("aicc" %in% names(gof))
      expect_true("bic" %in% names(gof))
    }
  )
})

# Plot Generation Tests -------------------------------------------------------

test_that("fit_plot generates valid ggplot object", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        selectUnit = "mg/L",
        rescale = FALSE,
        updateFit = 1,
        yaxis2 = "Cumulative Probability",
        xaxis2 = "Concentration",
        size2 = 12,
        title = ""
      )
      session$flushReact()

      returned <- session$returned
      plot <- returned$fit_plot()
      expect_true(ggplot2::is_ggplot(plot))
    }
  )
})

test_that("fit_plot includes custom title when provided", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        selectUnit = "mg/L",
        rescale = FALSE,
        updateFit = 1,
        yaxis2 = "Cumulative Probability",
        xaxis2 = "Concentration",
        size2 = 12,
        title = "Boron Toxicity"
      )
      session$flushReact()

      returned <- session$returned
      plot <- returned$fit_plot()
      expect_true(has_plot_title(plot))
      expect_equal(get_plot_title(plot), "Boron Toxicity")
    }
  )
})

test_that("fit_plot omits title when empty string", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        selectUnit = "mg/L",
        rescale = FALSE,
        updateFit = 1,
        yaxis2 = "Cumulative Probability",
        xaxis2 = "Concentration",
        size2 = 12,
        title = ""
      )
      session$flushReact()

      returned <- session$returned
      plot <- returned$fit_plot()
      expect_false(has_plot_title(plot))
    }
  )
})

# Distribution Selection Tests ------------------------------------------------

test_that("fit respects selected distributions", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      # Fit with single distribution
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("gamma"),
        rescale = FALSE,
        updateFit = 1
      )
      session$flushReact()

      returned <- session$returned
      fit <- returned$fit_dist()
      expect_equal(length(fit), 1)
      expect_equal(names(fit), "gamma")
    }
  )
})

# Module Return Values Tests --------------------------------------------------

test_that("mod_fit_server returns all expected reactive values", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      session$setInputs(
        selectConc = "Conc",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1,
        selectUnit = "mg/L",
        yaxis2 = "Cumulative Probability",
        xaxis2 = "Concentration",
        size2 = 12,
        title = "Test"
      )
      session$flushReact()

      returned <- session$returned

      # Check all return values exist
      expect_true(is.reactive(returned$fit_dist))
      expect_true(is.reactive(returned$fit_plot))
      expect_true(is.reactive(returned$gof_table))
      expect_true(is.reactive(returned$has_fit))
      expect_true(is.reactive(returned$conc_column))
      expect_true(is.reactive(returned$units))
      expect_true(is.reactive(returned$dists))
      expect_true(is.reactive(returned$rescale))
      expect_true(is.reactive(returned$title))

      # Check they return expected values
      expect_equal(returned$conc_column(), "Conc")
      expect_equal(returned$units(), "mg/L")
      expect_equal(returned$dists(), c("lnorm", "gamma"))
      expect_false(returned$rescale())
      expect_equal(returned$title(), "Test")
    }
  )
})

# Validation Tests ------------------------------------------------------------
test_that("validation fails if concentration column not selected", {
  testServer(
    mod_fit_server,
    args = list(
      translations = reactive(test_translations),
      lang = reactive("english"),
      data_mod = data_mod,
      big_mark = reactive(","),
      decimal_mark = reactive("."),
      main_nav = reactive("fit")
    ),
    {
      session$setInputs(
        selectConc = "Species",
        selectDist = c("lnorm", "gamma"),
        rescale = FALSE,
        updateFit = 1,
        selectUnit = "mg/L",
      )
      session$flushReact()

      expect_false(iv$is_valid())
    }
  )
})
