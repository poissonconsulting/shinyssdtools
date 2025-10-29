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

# Prediction Generation Tests -------------------------------------------------

test_that("mod_predict_server generates predictions from fit", {
  # Setup mock data and fit
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  # Create fit
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm", "gamma"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    # Set threshold
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = FALSE
    )
    session$flushReact()

    # Check predictions exist
    pred <- predictions()
    expect_true(is.data.frame(pred))
    expect_true(nrow(pred) > 0)
    expect_true("est" %in% names(pred))
  })
})

test_that("has_predict is FALSE initially", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("data")  # Not on predict tab
  ), {
    expect_false(has_predict())
  })
})

test_that("has_predict is TRUE after prediction generated", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = FALSE
    )
    session$flushReact()

    expect_true(has_predict())
  })
})

# Confidence Interval Tests ---------------------------------------------------

test_that("CI is NOT included without clicking Get CL button", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    # Check checkbox but don't click Get CL
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = TRUE,
      bootSamp = "100"
    )
    session$flushReact()

    # cl_requested should be FALSE (Get CL not clicked)
    expect_false(cl_requested())
  })
})

test_that("cl_nboot stores bootstrap count when Get CL clicked", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    # Set up for CI
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = TRUE,
      bootSamp = "100"
    )
    session$flushReact()

    # Click Get CL button
    session$setInputs(getCl = 1)
    session$flushReact()

    # cl_requested should be TRUE
    expect_true(cl_requested())

    # cl_nboot should store the bootstrap count
    expect_equal(cl_nboot(), 100)
  })
})

test_that("cl_nboot persists when navigating away and back", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  # Use reactiveVal for main_nav to simulate navigation
  nav <- reactiveVal("predict")

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = nav
  ), {
    # Set up and click Get CL
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = TRUE,
      bootSamp = "100",
      getCl = 1
    )
    session$flushReact()

    # Verify CL requested
    expect_true(cl_requested())
    expect_equal(cl_nboot(), 100)

    # Change bootstrap samples (but don't click Get CL)
    session$setInputs(bootSamp = "500")
    session$flushReact()

    # cl_nboot should still be 100 (not updated without Get CL click)
    expect_equal(cl_nboot(), 100)
  })
})

test_that("unchecking CI checkbox clears cl_requested when Get CL clicked", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    # First request CI
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = TRUE,
      bootSamp = "100",
      getCl = 1
    )
    session$flushReact()
    expect_true(cl_requested())

    # Uncheck CI and click Get CL again
    session$setInputs(
      includeCi = FALSE,
      getCl = 2
    )
    session$flushReact()

    # Should clear cl_requested
    expect_false(cl_requested())
    expect_null(cl_nboot())
  })
})

# Ribbon Style Tests ----------------------------------------------------------

test_that("ribbon reactive returns correct boolean value", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    # Test black ribbon (TRUE)
    session$setInputs(ribbonStyle = "TRUE")
    expect_true(ribbon())

    # Test red/green lines (FALSE)
    session$setInputs(ribbonStyle = "FALSE")
    expect_false(ribbon())
  })
})

# Threshold Type Tests --------------------------------------------------------

test_that("threshold_type reactive returns selected type", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    session$setInputs(threshType = "Concentration")
    expect_equal(threshold_type(), "Concentration")

    session$setInputs(threshType = "Fraction")
    expect_equal(threshold_type(), "Fraction")
  })
})

test_that("threshold_values returns percent and conc", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-"
    )
    session$flushReact()

    thresholds <- threshold_values()
    expect_type(thresholds, "list")
    expect_true("percent" %in% names(thresholds))
    expect_true("conc" %in% names(thresholds))
  })
})

# Title Tests -----------------------------------------------------------------

test_that("title reactive returns input value", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    session$setInputs(title = "Copper Toxicity")
    expect_equal(title(), "Copper Toxicity")

    session$setInputs(title = "")
    expect_equal(title(), "")
  })
})

# Module Return Values Tests --------------------------------------------------

test_that("mod_predict_server returns all expected reactive values", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = FALSE,
      ribbonStyle = "TRUE",
      title = "Test"
    )
    session$flushReact()

    # Check all return values exist
    expect_true(is.reactive(predictions))
    expect_true(is.reactive(threshold_values))
    expect_true(is.reactive(threshold_type))
    expect_true(is.reactive(include_ci))
    expect_true(is.reactive(ribbon))
    expect_true(is.reactive(title))
    expect_true(is.reactive(has_predict))
    expect_true(is.reactive(cl_requested))
    expect_true(is.reactive(cl_nboot))

    # Check they return expected values
    expect_equal(threshold_type(), "Concentration")
    expect_false(include_ci())
    expect_true(ribbon())
    expect_equal(title(), "Test")
  })
})

# Include CI Reactive Tests ---------------------------------------------------

test_that("include_ci reflects checkbox state", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    # Initially unchecked
    session$setInputs(includeCi = FALSE)
    expect_false(include_ci())

    # Check the box
    session$setInputs(includeCi = TRUE)
    expect_true(include_ci())
  })
})

# Edge Cases ------------------------------------------------------------------

test_that("predictions handle multiple thresholds", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    # Set threshold to 5%
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = FALSE
    )
    session$flushReact()

    pred1 <- predictions()
    expect_true(is.data.frame(pred1))

    # Change threshold to 10%
    session$setInputs(thresh = "10")
    session$flushReact()

    pred2 <- predictions()
    expect_true(is.data.frame(pred2))
  })
})

test_that("bootstrap sample count can be changed and applied", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)
  fit <- ssdtools::ssd_fit_dists(mock_data, dists = c("lnorm"))
  fit_mod <- mock_fit_module(fit = fit)

  testServer(mod_predict_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    fit_mod = fit_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("predict")
  ), {
    # Set low bootstrap count
    session$setInputs(
      threshType = "Concentration",
      thresh = "5",
      selectLabel = "-none-",
      selectColour = "-none-",
      selectShape = "-none-",
      includeCi = TRUE,
      bootSamp = "100",
      getCl = 1
    )
    session$flushReact()
    expect_equal(cl_nboot(), 100)

    # Change to higher count
    session$setInputs(
      bootSamp = "500",
      getCl = 2
    )
    session$flushReact()
    expect_equal(cl_nboot(), 500)
  })
})
