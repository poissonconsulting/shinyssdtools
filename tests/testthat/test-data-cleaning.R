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

test_that("remove_blank_headers removes X1, X2, etc. columns", {
  df <- data.frame(
    Concentration = c(1, 2, 3),
    Species = c("A", "B", "C"),
    X1 = c(NA, NA, NA),
    X2 = c("", "", "")
  )

  result <- remove_blank_headers(df)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("Concentration", "Species"))
})

test_that("remove_blank_headers preserves data when no blank headers", {
  df <- data.frame(
    Concentration = c(1, 2, 3),
    Species = c("A", "B", "C")
  )

  result <- remove_blank_headers(df)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("Concentration", "Species"))
  expect_equal(nrow(result), 3)
})

test_that("remove_empty_columns removes columns with all NA", {
  df <- data.frame(
    Concentration = c(1, 2, 3),
    Species = c("A", "B", "C"),
    Empty1 = c(NA, NA, NA),
    Empty2 = c("", "", "")
  )

  result <- remove_empty_columns(df)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("Concentration", "Species"))
})

test_that("remove_empty_columns preserves single column with data", {
  df <- data.frame(
    Concentration = c(1, 2, 3),
    Empty1 = c(NA, NA, NA),
    Empty2 = c("", "", "")
  )

  result <- remove_empty_columns(df)

  expect_equal(names(result), "Concentration")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
})

test_that("remove_empty_columns handles mixed NA and empty strings", {
  df <- data.frame(
    Concentration = c(1, 2, 3),
    Empty = c(NA, "", NA)
  )

  result <- remove_empty_columns(df)

  expect_equal(ncol(result), 1)
  expect_equal(names(result), "Concentration")
})

test_that("remove_empty_rows removes rows with all NA", {
  df <- data.frame(
    Concentration = c(1, NA, 3),
    Species = c("A", NA, "C")
  )

  result <- remove_empty_rows(df)

  expect_equal(nrow(result), 2)
  expect_equal(result$Concentration, c(1, 3))
  expect_equal(result$Species, c("A", "C"))
})

test_that("remove_empty_rows removes rows with all empty strings", {
  df <- data.frame(
    Concentration = c(1, "", 3),
    Species = c("A", "", "C"),
    stringsAsFactors = FALSE
  )

  result <- remove_empty_rows(df)

  expect_equal(nrow(result), 2)
})

test_that("remove_empty_rows handles mixed NA and empty strings", {
  df <- data.frame(
    Concentration = c(1, NA, 3),
    Species = c("A", "", "C"),
    stringsAsFactors = FALSE
  )

  result <- remove_empty_rows(df)

  expect_equal(nrow(result), 2)
  expect_equal(result$Concentration, c(1, 3))
})

test_that("remove_empty_rows preserves rows with partial data", {
  df <- data.frame(
    Concentration = c(1, 2, NA),
    Species = c("A", NA, "C")
  )

  result <- remove_empty_rows(df)

  expect_equal(nrow(result), 3)
})

test_that("clean_ssd_data handles typical messy CSV", {
  df <- data.frame(
    Concentration = c(1, 2, NA, 4),
    Species = c("A", "B", NA, "D"),
    X1 = c(NA, NA, NA, NA),
    Empty = c("", "", "", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_ssd_data(df)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("Concentration", "Species"))
  expect_equal(nrow(result), 3)
})

test_that("clean_ssd_data handles handsontable input", {
  # Simulate handsontable with filled Concentration but empty Species/Group
  df <- data.frame(
    Concentration = c(1, 2, 3, NA, NA),
    Species = c("", "", "", "", ""),
    Group = c("", "", "", "", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_ssd_data(df)

  expect_equal(ncol(result), 1)
  expect_equal(names(result), "Concentration")
  expect_equal(nrow(result), 3)
})

test_that("clean_ssd_data returns empty tibble for all empty data", {
  df <- data.frame(
    X1 = c(NA, NA, NA),
    X2 = c("", "", "")
  )

  result <- clean_ssd_data(df)

  # When all columns are removed, rows are preserved (empty tibble with rows)
  expect_equal(ncol(result), 0)
  expect_s3_class(result, "tbl_df")
})
