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

# Fit Reactive Tests ----------------------------------------------------------

test_that("mod_fit_server produces valid fit object with demo data", {
  # Create mock data module
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    # Set required inputs
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm", "gamma"),
      rescale = FALSE,
      updateFit = 1
    )
    session$flushReact()

    # Check fit object
    fit <- fit_dist()
    expect_s3_class(fit, "fitdists")
    expect_equal(length(fit), 2)
    expect_equal(names(fit), c("lnorm", "gamma"))
  })
})

test_that("mod_fit_server has_fit is FALSE initially", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("data")  # Not on fit tab
  ), {
    expect_false(has_fit())
  })
})

test_that("mod_fit_server has_fit is TRUE after successful fit", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm", "gamma"),
      rescale = FALSE,
      updateFit = 1
    )
    session$flushReact()

    expect_true(has_fit())
  })
})

# Goodness-of-Fit Table Tests -------------------------------------------------

test_that("gof_table produces valid table after fit", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm", "gamma"),
      rescale = FALSE,
      updateFit = 1
    )
    session$flushReact()

    gof <- gof_table()
    expect_true(is.data.frame(gof))
    expect_equal(nrow(gof), 2)  # Two distributions

    # Check for expected columns
    expect_true("aicc" %in% names(gof))
    expect_true("bic" %in% names(gof))
  })
})

test_that("gof_table includes distribution names", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm", "gamma", "lgumbel"),
      rescale = FALSE,
      updateFit = 1
    )
    session$flushReact()

    gof <- gof_table()
    expect_equal(nrow(gof), 3)

    # Distribution column should exist
    expect_true("dist" %in% names(gof) || "distribution" %in% names(gof))
  })
})

# Plot Generation Tests -------------------------------------------------------

test_that("fit_plot generates valid ggplot object", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm", "gamma"),
      rescale = FALSE,
      updateFit = 1,
      yaxis2 = "Cumulative Probability",
      xaxis2 = "Concentration",
      size2 = 12,
      title = ""
    )
    session$flushReact()

    plot <- fit_plot()
    expect_s3_class(plot, "ggplot")
  })
})

test_that("fit_plot includes custom title when provided", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm"),
      rescale = FALSE,
      updateFit = 1,
      yaxis2 = "Cumulative Probability",
      xaxis2 = "Concentration",
      size2 = 12,
      title = "Boron Toxicity"
    )
    session$flushReact()

    plot <- fit_plot()
    expect_true(has_plot_title(plot))
    expect_equal(get_plot_title(plot), "Boron Toxicity")
  })
})

test_that("fit_plot omits title when empty string", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm"),
      rescale = FALSE,
      updateFit = 1,
      yaxis2 = "Cumulative Probability",
      xaxis2 = "Concentration",
      size2 = 12,
      title = ""
    )
    session$flushReact()

    plot <- fit_plot()
    expect_false(has_plot_title(plot))
  })
})

# Distribution Selection Tests ------------------------------------------------

test_that("fit respects selected distributions", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    # Fit with single distribution
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm"),
      rescale = FALSE,
      updateFit = 1
    )
    session$flushReact()

    fit <- fit_dist()
    expect_equal(length(fit), 1)
    expect_equal(names(fit), "lnorm")
  })
})

test_that("fit works with multiple distributions", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm", "gamma", "lgumbel", "llogis"),
      rescale = FALSE,
      updateFit = 1
    )
    session$flushReact()

    fit <- fit_dist()
    expect_equal(length(fit), 4)
  })
})

# Rescale Option Tests --------------------------------------------------------

test_that("rescale option is passed to fit function", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    # Test with rescale TRUE
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm"),
      rescale = TRUE,
      updateFit = 1
    )
    session$flushReact()

    fit <- fit_dist()
    expect_s3_class(fit, "fitdists")
  })
})

# Module Return Values Tests --------------------------------------------------

test_that("mod_fit_server returns all expected reactive values", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm"),
      rescale = FALSE,
      updateFit = 1,
      selectUnit = "mg/L",
      yaxis2 = "Cumulative Probability",
      xaxis2 = "Concentration",
      size2 = 12,
      title = "Test"
    )
    session$flushReact()

    # Check all return values exist
    expect_true(is.reactive(fit_dist))
    expect_true(is.reactive(fit_plot))
    expect_true(is.reactive(gof_table))
    expect_true(is.reactive(has_fit))
    expect_true(is.reactive(conc_column))
    expect_true(is.reactive(units))
    expect_true(is.reactive(dists))
    expect_true(is.reactive(rescale))
    expect_true(is.reactive(title))

    # Check they return expected values
    expect_equal(conc_column(), "Conc")
    expect_equal(units(), "mg/L")
    expect_equal(dists(), "lnorm")
    expect_false(rescale())
    expect_equal(title(), "Test")
  })
})

# Title Input Tests -----------------------------------------------------------

test_that("title reactive returns input value", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    session$setInputs(title = "Copper Toxicity")
    expect_equal(title(), "Copper Toxicity")

    session$setInputs(title = "")
    expect_equal(title(), "")
  })
})

# Edge Cases ------------------------------------------------------------------

test_that("fit handles rescale toggle", {
  mock_data <- data.frame(Conc = c(1, 2, 5, 10, 20, 50, 100))
  data_mod <- mock_data_module(data = mock_data)

  testServer(mod_fit_server, args = list(
    translations = mock_translations(),
    lang = reactive("english"),
    data_mod = data_mod,
    big_mark = reactive(","),
    decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ), {
    # Initial fit without rescale
    session$setInputs(
      selectConc = "Conc",
      selectDist = c("lnorm"),
      rescale = FALSE,
      updateFit = 1
    )
    session$flushReact()

    fit1 <- fit_dist()
    expect_s3_class(fit1, "fitdists")

    # Toggle rescale
    session$setInputs(
      rescale = TRUE,
      updateFit = 2
    )
    session$flushReact()

    fit2 <- fit_dist()
    expect_s3_class(fit2, "fitdists")
  })
})
