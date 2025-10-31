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

# testServer Tests for Predict Module
# =====================================
# Tests for reactive logic in mod_predict_server using testServer

# Setup translations
test_translations <- translations
test_translations$trans <- test_translations[["english"]]

# Use actual boron data
test_data <- clean_ssd_data(boron.data)
data_mod <- mock_data_module(data = test_data)

# Create actual fit from boron data
test_fit <- ssdtools::ssd_fit_dists(test_data, dists = c("lnorm", "gamma"))
fit_mod <- mock_fit_module(fit = test_fit, conc_column = "Conc", units = "")

# Module args used across all tests
predict_args <- list(
  translations = reactive(test_translations),
  lang = reactive("english"),
  data_mod = data_mod,
  fit_mod = fit_mod,
  big_mark = reactive(","),
  decimal_mark = reactive("."),
  main_nav = reactive("predict")
)

# Prediction Generation Tests -------------------------------------------------

test_that("mod_predict_server generates predictions from fit", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      # Set threshold
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      returned <- session$returned

      # Check predictions exist
      pred <- returned$predictions()
      expect_true(is.data.frame(pred))
      expect_true(nrow(pred) > 0)
      # Check expected columns exist
      expect_true("est" %in% names(pred))
      expect_true("dist" %in% names(pred))

      # Check predictions have numeric values
      expect_true(is.numeric(pred$est))
      expect_true(all(!is.na(pred$est)))

      # Get CL not clicked yet
      expect_true(all(is.na(pred$lcl)))
    }
  )
})

test_that("has_predict is TRUE after prediction generated", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE,
        bootSamp = "100"
      )
      session$flushReact()

      returned <- session$returned
      expect_true(returned$has_predict())
    }
  )
})

# Confidence Interval Tests ---------------------------------------------------

test_that("cl_requested is FALSE without clicking Get CL button", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      # Check checkbox but don't click Get CL
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "100"
      )
      session$flushReact()

      returned <- session$returned
      # cl_requested should be FALSE (Get CL not clicked)
      expect_false(returned$cl_requested())
    }
  )
})

test_that("cl_nboot stores bootstrap count when Get CL clicked", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "100"
      )
      session$flushReact()

      # Click Get CL button
      session$setInputs(getCl = 1)
      session$flushReact()

      returned <- session$returned
      expect_true(returned$cl_requested())
      expect_equal(returned$cl_nboot(), 100)
    }
  )
})

test_that("unchecking CI checkbox clears cl_requested when Get CL clicked", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      # First request CI
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "100",
        getCl = 1
      )
      session$flushReact()

      returned <- session$returned
      expect_true(returned$cl_requested())

      # Uncheck CI and click Get CL again
      session$setInputs(
        includeCi = FALSE,
        getCl = 2
      )
      session$flushReact()

      # Should clear cl_requested
      expect_false(returned$cl_requested())
      expect_null(returned$cl_nboot())
    }
  )
})

# Reactive Value Tests --------------------------------------------------------

test_that("ribbon reactive returns correct boolean value", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE
      )
      returned <- session$returned

      # Test black ribbon (TRUE)
      session$setInputs(ribbonStyle = "TRUE")
      expect_true(returned$ribbon())

      # Test red/green lines (FALSE)
      session$setInputs(ribbonStyle = "FALSE")
      expect_false(returned$ribbon())
    }
  )
})

test_that("threshold_type reactive returns selected type", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      returned <- session$returned

      session$setInputs(threshType = "Concentration")
      expect_equal(returned$threshold_type(), "Concentration")

      session$setInputs(threshType = "Fraction")
      expect_equal(returned$threshold_type(), "Fraction")
    }
  )
})

test_that("threshold_values returns percent and conc", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5"
      )
      session$flushReact()

      returned <- session$returned
      thresholds <- returned$threshold_values()
      expect_type(thresholds, "list")
      expect_true("percent" %in% names(thresholds))
      expect_true("conc" %in% names(thresholds))
    }
  )
})

test_that("include_ci reflects checkbox state", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5"
      )
      returned <- session$returned

      # Initially unchecked
      session$setInputs(includeCi = FALSE)
      expect_false(returned$include_ci())

      # Check the box
      session$setInputs(includeCi = TRUE)
      expect_true(returned$include_ci())
    }
  )
})

test_that("title reactive returns input value", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5"
      )
      returned <- session$returned

      session$setInputs(title = "Copper Toxicity")
      expect_equal(returned$title(), "Copper Toxicity")

      session$setInputs(title = "")
      expect_equal(returned$title(), "")
    }
  )
})

# Module Return Values Tests --------------------------------------------------

test_that("mod_predict_server returns all expected reactive values", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE,
        ribbonStyle = "TRUE",
        title = "Test"
      )
      session$flushReact()

      returned <- session$returned

      # Check all return values exist
      expect_true(is.reactive(returned$predictions))
      expect_true(is.reactive(returned$threshold_values))
      expect_true(is.reactive(returned$threshold_type))
      expect_true(is.reactive(returned$include_ci))
      expect_true(is.reactive(returned$ribbon))
      expect_true(is.reactive(returned$title))
      expect_true(is.reactive(returned$has_predict))
      expect_true(is.reactive(returned$cl_requested))
      expect_true(is.reactive(returned$cl_nboot))

      # Check they return expected values
      expect_equal(returned$threshold_type(), "Concentration")
      expect_false(returned$include_ci())
      expect_true(returned$ribbon())
      expect_equal(returned$title(), "Test")
    }
  )
})

# Threshold Change Tests ------------------------------------------------------

test_that("predictions change when % affecting threshold changes", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      # First prediction at 5%
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      returned <- session$returned
      pred_vals <- returned$threshold_values()
      pred_perc <- pred_vals$percent
      pred_conc <- pred_vals$conc

      # Change to 10%
      session$setInputs(thresh = "10")
      session$flushReact()

      pred_vals_10 <- returned$threshold_values()
      pred_perc_10 <- pred_vals_10$percent
      pred_conc_10 <- pred_vals_10$conc

      expect_identical(pred_perc_10, 10)
      # Estimates should be different
      expect_false(identical(pred_perc, pred_perc_10))
      expect_false(identical(pred_conc, pred_conc_10))
    }
  )
})

test_that("predictions change when switching to Fraction threshold type", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      # First prediction using Concentration threshold
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      returned <- session$returned
      pred_vals <- returned$threshold_values()
      pred_perc <- pred_vals$percent
      pred_conc <- pred_vals$conc

      # Switch to Fraction threshold
      session$setInputs(
        threshType = "Fraction",
        conc = 1.0
      )
      session$flushReact()

      pred_vals_frac <- returned$threshold_values()
      pred_perc_frac <- pred_vals_frac$percent
      pred_conc_frac <- pred_vals_frac$conc

      expect_identical(pred_conc_frac, 1.0)
      # Estimates should be different
      expect_false(identical(pred_perc, pred_perc_frac))
      expect_false(identical(pred_conc, pred_conc_frac))
    }
  )
})

# Plot Tests ------------------------------------------------------------------

test_that("model_average_plot returns ggplot object", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      suppressWarnings({
        session$setInputs(
          threshType = "Concentration",
          thresh = "5",
          includeCi = FALSE,
          title = "",
          xaxis = "Concentration",
          yaxis = "Percent",
          selectColour = "-none-",
          selectLabel = "-none-",
          selectShape = "-none-",
          checkHc = TRUE,
          adjustLabel = 1,
          xlog = TRUE,
          xbreaks = c(1, 10, 100),
          xMin = NULL,
          xMax = NULL,
          selectPalette = "Set1",
          legendColour = "Colour",
          legendShape = "Shape",
          size3 = 12,
          sizeLabel3 = 3,
          ribbonStyle = "TRUE"
        )
      })
      session$flushReact()

      returned <- session$returned
      plot <- returned$model_average_plot()

      # Check it's a ggplot
      expect_true(ggplot2::is_ggplot(plot))
    }
  )
})

# Confidence Interval / Bootstrap Tests ---------------------------------------

test_that("clicking Get CL generates confidence interval table", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      # Set up initial prediction
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "5"
      )
      session$flushReact()

      session$setInputs(getCl = 1)
      session$flushReact()

      returned <- session$returned

      expect_true(returned$has_cl())
      cl_table <- returned$predict_cl()
      expect_true(is.data.frame(cl_table))
      expect_true(nrow(cl_table) > 0)

      # # Check it has confidence interval columns
      expect_true("lcl" %in% names(cl_table))
      expect_true("ucl" %in% names(cl_table))
    }
  )
})

# get cl clikced changes predictions -------------------------------------
test_that("mod_predict_server predictions include lcl/ucl when Get CL clicked", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      # Set threshold
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE,
        bootSamp = 5
      )
      session$flushReact()

      session$setInputs(getCl = 1, includeCi = TRUE)
      session$flushReact()

      returned <- session$returned

      # Check predictions exist
      pred <- returned$predictions()
      expect_true(is.data.frame(pred))
      expect_true(all(!is.na(pred$lcl)))
      expect_true(all(!is.na(pred$ucl)))
    }
  )
})

test_that("model_average_plot includes confidence intervals after Get CL", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      suppressWarnings({
        # Set up initial plot without CI
        session$setInputs(
          threshType = "Concentration",
          thresh = "5",
          includeCi = TRUE,
          bootSamp = "5",
          title = "",
          xaxis = "Concentration",
          yaxis = "Percent",
          selectColour = "-none-",
          selectLabel = "-none-",
          selectShape = "-none-",
          checkHc = TRUE,
          adjustLabel = 1,
          xlog = TRUE,
          xbreaks = c(1, 10, 100),
          xMin = NULL,
          xMax = NULL,
          selectPalette = "Set1",
          legendColour = "Colour",
          legendShape = "Shape",
          size3 = 12,
          sizeLabel3 = 3,
          ribbonStyle = "TRUE"
        )

        session$flushReact()

        returned <- session$returned
        plot_before <- returned$model_average_plot()
        expect_false(has_confidence_intervals(plot_before))

        # Click Get CL
        session$setInputs(getCl = 1)
        session$flushReact()
      })
      plot_after <- returned$model_average_plot()

      # Plot should now have confidence intervals
      # Check for ribbon or line layers indicating CI
      expect_true(has_confidence_intervals(plot_after))
    }
  )
})

test_that("changing bootstrap samples updates CL table nboot", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      # First CL request with 100 samples
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = TRUE,
        bootSamp = "5",
        getCl = 1
      )
      session$flushReact()

      returned <- session$returned
      pred <- returned$predictions()
      expect_equal(returned$cl_nboot(), 5)
      expect_equal(unique(pred$nboot), 5)

      # Change to 500 samples and click Get CL again
      session$setInputs(
        bootSamp = "10",
        getCl = 2
      )
      session$flushReact()

      expect_equal(returned$cl_nboot(), 10)
      pred <- returned$predictions()
      expect_equal(unique(pred$nboot), 10)
    }
  )
})

# Number Formatting Tests -----------------------------------------------------

test_that("concentration estimates use English number formatting", {
  testServer(
    mod_predict_server,
    args = predict_args,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      # Get formatted outputs
      estConc <- output$estConc
      hcConc <- output$hcConc

      # Should use English formatting (period for decimal, comma for thousands)
      # Check that decimal mark is period (if present)
      if (grepl("[.,]", estConc)) {
        # If there's a separator, it should be period for decimal
        expect_true(grepl("\\.", estConc) || !grepl(",", estConc))
      }
    }
  )
})

test_that("concentration estimates use French number formatting", {
  # Set up French translations
  test_trans_fr <- translations
  test_trans_fr$trans <- test_trans_fr[["french"]]

  predict_args_fr <- predict_args
  predict_args_fr$translations <- reactive(test_trans_fr)
  predict_args_fr$lang <- reactive("french")
  predict_args_fr$big_mark <- reactive(" ")
  predict_args_fr$decimal_mark <- reactive(",")

  testServer(
    mod_predict_server,
    args = predict_args_fr,
    {
      session$setInputs(
        threshType = "Concentration",
        thresh = "5",
        includeCi = FALSE
      )
      session$flushReact()

      # Get formatted outputs
      estConc <- output$estConc
      hcConc <- output$hcConc

      # Should use French formatting (comma for decimal, space for thousands)
      # Check that decimal mark is comma (if present)
      if (grepl("[.,]", estConc)) {
        expect_true(grepl(",", estConc))
      }
    }
  )
})
