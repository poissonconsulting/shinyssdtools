# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

utils::globalVariables(c("."))

safe_try <- function(expr, silent = TRUE) {
  result <- try(expr, silent = silent)
  if (inherits(result, "try-error")) {
    return(NULL)
  }
  result
}

clean_nboot <- function(x) {
  as.integer(gsub("(,|\\s)", "", x))
}

# Remove Excel/Numbers blank header columns (X1, X2, etc.)
remove_blank_headers <- function(data) {
  data %>%
    dplyr::select(-dplyr::any_of(paste0("X", 1:200)))
}

# Remove completely empty columns
remove_empty_columns <- function(data) {
  data %>%
    dplyr::as_tibble() %>%
    dplyr::select(dplyr::where(~ !all(is.na(.x) | .x == "")))
}

# Remove completely empty rows
remove_empty_rows <- function(data) {
  data %>%
    dplyr::as_tibble() %>%
    dplyr::filter(
      if (ncol(.) > 0) {
        !dplyr::if_all(dplyr::everything(), ~ is.na(.x) | .x == "")
      } else {
        TRUE
      }
    )
}

#' Clean uploaded data
#'
#' Removes blank headers, empty columns, and empty rows from uploaded data
#'
#' @param data A data frame or tibble
#' @return A cleaned tibble
#' @export
clean_ssd_data <- function(data) {
  data %>%
    remove_blank_headers() %>%
    remove_empty_columns() %>%
    remove_empty_rows()
}

estimate_time <- function(nboot, lang) {
  preset_df <- data.frame(
    n = c(500, 1000, 5000, 10000),
    english = c("10 seconds", "20 seconds", "2 minutes", "5 minutes"),
    french = c("10 secondes", "20 secondes", "2 minutes", "5 minutes")
  )

  if (nboot %in% preset_df$n) {
    return(preset_df[preset_df$n == nboot, ][[lang]])
  }

  # use linear model if not in preset:
  time_sec <- max(1, -12.89 + 0.0304 * nboot)

  if (time_sec < 60) {
    time_num <- round(time_sec)
    time_str <- if (lang == "french") {
      paste(time_num, ifelse(time_num <= 1, "seconde", "secondes"))
    } else {
      paste(time_num, ifelse(time_num <= 1, "second", "seconds"))
    }
  } else {
    time_num <- round(time_sec / 60, 1)
    time_str <- if (lang == "french") {
      paste(time_num, ifelse(time_num <= 1, "minute", "minutes"))
    } else {
      paste(time_num, ifelse(time_num <= 1, "minute", "minutes"))
    }
  }

  time_str
}

tr <- function(id, trans) {
  trans$trans[trans$id == id]
}

paste_js <- function(x, ns) {
  paste0("output['", ns(x), "']")
}

guess_sp <- function(name) {
  name[grepl("sp", tolower(name))][1]
}

guess_conc <- function(name, data = NULL) {
  # First try to find column with 'conc' in name
  conc_match <- name[grepl("conc", tolower(name))][1]
  if (!is.na(conc_match)) {
    return(conc_match)
  }

  # If no 'conc' match and data provided, find first numeric column
  if (!is.null(data)) {
    numeric_cols <- vapply(data, is.numeric, logical(1))
    if (any(numeric_cols)) {
      return(names(data)[numeric_cols][1])
    }
  }

  # Return NA if no match found
  return(NA_character_)
}

label_mandatory <- function(label) {
  tagList(label, span("*", class = "mandatory_star"))
}

inline <- function(x) {
  tags$div(style = "display:inline-block;", x)
}

hint <- function(x) {
  HTML(paste0("<font color='grey'>", x, "</font>"))
}

zero_range <- function(x, tol = .Machine$double.eps^0.5) {
  if (length(x) == 1) {
    return(TRUE)
  }
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

estimate_hc <- function(x, percent) {
  ssdtools::ssd_hc(x, proportion = percent / 100)$est
}

estimate_hp <- function(x, conc) {
  ssdtools::ssd_hp(x, conc = conc)$est
}

ssd_hc_ave <- function(x, percent, nboot) {
  dist <- ssdtools::ssd_hc(
    x,
    proportion = percent / 100,
    ci = TRUE,
    average = FALSE,
    nboot = nboot,
    min_pboot = 0.8
  )

  if (length(x) == 1) {
    ave <- dist
    ave$dist <- "average"
  } else {
    ave <- ssdtools::ssd_hc(
      x,
      proportion = percent / 100,
      ci = TRUE,
      average = TRUE,
      nboot = nboot,
      min_pboot = 0.8
    )
  }

  dplyr::bind_rows(ave, dist) %>%
    dplyr::mutate_at(c("est", "se", "ucl", "lcl", "wt"), ~ signif(., 3))
}

ssd_hp_ave <- function(x, conc, nboot) {
  dist <- ssdtools::ssd_hp(
    x,
    conc = conc,
    ci = TRUE,
    average = FALSE,
    nboot = nboot,
    min_pboot = 0.8
  )

  if (length(x) == 1) {
    ave <- dist
    ave$dist <- "average"
  } else {
    ave <- ssdtools::ssd_hp(
      x,
      conc = conc,
      ci = TRUE,
      average = TRUE,
      nboot = nboot,
      min_pboot = 0.8
    )
  }

  dplyr::bind_rows(ave, dist) %>%
    dplyr::mutate_at(c("est", "se", "ucl", "lcl", "wt"), ~ signif(., 3))
}

# Helper function to format R code with proper indentation using styler
format_r_code <- function(code_lines) {
  # Join lines into a single string
  code_text <- paste(code_lines, collapse = "\n")

  # Use styler to format the code
  # scope = "tokens" provides lighter-weight formatting focused on spacing/indentation
  formatted <- styler::style_text(code_text, scope = "tokens")

  # Convert formatted text back to a single string
  formatted_text <- paste(formatted, collapse = "\n")

  # Replace double quotes with single quotes
  # This is done after styling to maintain R syntax validity during formatting
  # Important for structure() output from dput() which uses double quotes
  formatted_text <- gsub('"', "'", formatted_text)

  formatted_text
}
