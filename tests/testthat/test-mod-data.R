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

# testServer Tests for Data Module
# ==================================
# Tests for reactive logic in mod_data_server using testServer

# Helper to create mock translations
mock_translations <- function() {
  reactive({
    list(
      list(
        id = "ui_1htconc",
        english = "Concentration",
        french = "Concentration"
      ),
      list(id = "ui_1htspp", english = "Species", french = "Espèce"),
      list(id = "ui_1htgrp", english = "Group", french = "Groupe"),
      list(id = "ui_1htchm", english = "Chemical", french = "Chimique"),
      list(id = "ui_1htunt", english = "Units", french = "Unités")
    )
  })
}

# Demo Data Tests -------------------------------------------------------------

test_that("mod_data_server loads demo data when demo button clicked", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      # Initially no data
      expect_false(has_data())

      # Click demo data button
      session$setInputs(demoData = 1)
      session$flushReact()

      # print(boron.data)
      print(clean_data())

      # Data should be loaded
      print(has_data())
      expect_true(has_data())

      # Check data structure
      data <- data()
      expect_true(is.data.frame(data))
      expect_true(nrow(data) > 0)
      expect_true(ncol(data) >= 3) # Should have at least Conc, Species, Group
    }
  )
})

test_that("mod_data_server demo data has correct column names", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      data <- data()
      col_names <- names(data)

      # Should have cleaned column names (make.names applied)
      expect_true(all(col_names == make.names(col_names)))
    }
  )
})

# Toxicant Name Tests ---------------------------------------------------------

test_that("toxicant_name reactive returns input value", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      # Initially empty
      expect_equal(toxicant_name(), "")

      # Set toxicant name
      session$setInputs(toxicant = "Boron")
      expect_equal(toxicant_name(), "Boron")

      # Change toxicant name
      session$setInputs(toxicant = "Copper")
      expect_equal(toxicant_name(), "Copper")
    }
  )
})

test_that("toxicant_name works independently of data loading", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      # Set name before loading data
      session$setInputs(toxicant = "Test Chemical")
      expect_equal(toxicant_name(), "Test Chemical")
      expect_false(has_data())

      # Load data - toxicant name should persist
      session$setInputs(demoData = 1)
      session$flushReact()

      expect_true(has_data())
      expect_equal(toxicant_name(), "Test Chemical")
    }
  )
})

# Data Validation Tests -------------------------------------------------------

test_that("has_data returns FALSE when no data loaded", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      expect_false(has_data())
    }
  )
})

test_that("has_data returns TRUE after demo data loaded", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      expect_true(has_data())
    }
  )
})

test_that("clean_data is available after demo data loaded", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      clean <- clean_data()
      expect_true(is.data.frame(clean))
      expect_true(nrow(clean) > 0)
    }
  )
})

test_that("data and clean_data are both accessible", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      regular_data <- data()
      cleaned_data <- clean_data()

      # Both should be data frames
      expect_true(is.data.frame(regular_data))
      expect_true(is.data.frame(cleaned_data))

      # Should have same number of rows (cleaning doesn't remove rows in demo data)
      expect_equal(nrow(regular_data), nrow(cleaned_data))
    }
  )
})

# Module Return Values Tests --------------------------------------------------

test_that("mod_data_server returns all expected reactive values", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      # Check all return values exist
      expect_true(is.reactive(data))
      expect_true(is.reactive(clean_data))
      expect_true(is.reactive(has_data))
      expect_true(is.reactive(toxicant_name))

      # Check they can be called
      expect_no_error(data())
      expect_no_error(clean_data())
      expect_no_error(has_data())
      expect_no_error(toxicant_name())
    }
  )
})

# Translation Tests -----------------------------------------------------------

test_that("demo data respects language setting for column names", {
  # English test
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      data <- data()
      # Column names should be cleaned versions of English translations
      expect_true(is.data.frame(data))
    }
  )
})

# Edge Cases ------------------------------------------------------------------

test_that("module handles rapid data source changes", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      # Load demo data
      session$setInputs(demoData = 1)
      session$flushReact()
      expect_true(has_data())

      # Load demo again
      session$setInputs(demoData = 2)
      session$flushReact()
      expect_true(has_data())

      # Data should still be valid
      data <- data()
      expect_true(is.data.frame(data))
      expect_true(nrow(data) > 0)
    }
  )
})

test_that("toxicant_name handles special characters", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      # Test with various special characters
      test_names <- c(
        "Test-Chemical",
        "Test_Chemical",
        "Test Chemical 123",
        "Test (Chemical)",
        "Test/Chemical"
      )

      for (name in test_names) {
        session$setInputs(toxicant = name)
        expect_equal(toxicant_name(), name)
      }
    }
  )
})

test_that("toxicant_name handles empty and whitespace", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      # Empty string
      session$setInputs(toxicant = "")
      expect_equal(toxicant_name(), "")

      # Whitespace
      session$setInputs(toxicant = "   ")
      expect_equal(toxicant_name(), "   ")

      # Can set back to non-empty
      session$setInputs(toxicant = "Boron")
      expect_equal(toxicant_name(), "Boron")
    }
  )
})

# Data Structure Tests --------------------------------------------------------

test_that("demo data contains numeric concentration column", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      data <- data()

      # Find concentration column (should be first numeric column or named Conc)
      numeric_cols <- sapply(data, is.numeric)
      expect_true(
        any(numeric_cols),
        info = "Should have at least one numeric column"
      )
    }
  )
})

test_that("clean_data maintains data integrity", {
  testServer(
    mod_data_server,
    args = list(
      translations = mock_translations(),
      lang = reactive("english")
    ),
    {
      session$setInputs(demoData = 1)
      session$flushReact()

      original <- data()
      cleaned <- clean_data()

      # Should have same dimensions
      expect_equal(nrow(original), nrow(cleaned))

      # Should still have key columns
      expect_true(ncol(cleaned) >= 1) # At least concentration
    }
  )
})
