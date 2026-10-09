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

# Compare the results of run.R for two checkouts, scenario by scenario.
# See README.md.
# Usage: Rscript compare.R <a.rds> <b.rds> <out.csv>
args <- commandArgs(TRUE)
a <- readRDS(args[1])
b <- readRDS(args[2])
stopifnot(length(a) == length(b))

num_cols <- c("est", "se", "lcl", "ucl", "wt", "proportion", "pboot", "nboot")

# "same", or the largest absolute difference, or why they cannot be compared.
compare_df <- function(x, y, key = NULL) {
  if (is.character(x) || is.character(y)) {
    return(if (identical(x, y)) "same" else paste("differ:", substr(paste(x), 1, 40), "|", substr(paste(y), 1, 40)))
  }
  if (is.null(x) && is.null(y)) return("none")
  if (is.null(x) || is.null(y)) return("missing on one")
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  if (!is.null(key)) {
    x <- x[do.call(order, x[key]), , drop = FALSE]
    y <- y[do.call(order, y[key]), , drop = FALSE]
  }
  if (nrow(x) != nrow(y)) return(sprintf("rows %d vs %d", nrow(x), nrow(y)))
  cols <- intersect(names(x), names(y))
  max_diff <- 0
  for (col in cols) {
    if (is.numeric(x[[col]]) && is.numeric(y[[col]])) {
      d <- abs(x[[col]] - y[[col]])
      d[is.na(x[[col]]) & is.na(y[[col]])] <- 0
      d[xor(is.na(x[[col]]), is.na(y[[col]]))] <- Inf
      d[is.infinite(x[[col]]) & identical(x[[col]], y[[col]])] <- 0
      max_diff <- max(max_diff, d, na.rm = TRUE)
    } else if (!identical(as.character(x[[col]]), as.character(y[[col]]))) {
      return(paste("column", col, "differs"))
    }
  }
  if (max_diff == 0) "same" else sprintf("max diff %.3g", max_diff)
}

# The bootstrap calls' settings, without the percents or concentrations.
call_settings <- function(calls) {
  boot <- Filter(function(x) isTRUE(x$settings$ci), calls)
  s <- vapply(boot, function(x) {
    st <- x$settings[order(names(x$settings))]
    paste(x$fn, x$dists, paste(names(st), unlist(lapply(st, format)), sep = "=", collapse = ";"))
  }, "")
  sort(unique(s))
}

rows <- lapply(seq_along(a), function(i) {
  x <- a[[i]]
  y <- b[[i]]
  sc <- x$scenario
  stopifnot(identical(sc, y$scenario))
  base <- data.frame(data = sc$data, dists = sc$dists, rescale = sc$rescale)
  if (!inherits(x$fit, "fitdists") || !inherits(y$fit, "fitdists")) {
    base$fit <- if (identical(class(x$fit), class(y$fit))) "no fit on either" else "fit on one only"
    return(base)
  }
  gof_key <- "dist"
  base$fit <- if (identical(names(x$fit), names(y$fit))) "same" else "dists differ"
  base$params <- compare_df(x$params, y$params)
  base$gof <- compare_df(x$gof, y$gof, gof_key)
  base$hc_estimates <- if (identical(lapply(x$hc, `[[`, "threshold"), lapply(y$hc, `[[`, "threshold"))) "same" else "differ"
  base$hc_curve <- {
    r <- vapply(names(x$hc), function(p) compare_df(x$hc[[p]]$predictions, y$hc[[p]]$predictions, "proportion"), "")
    if (all(r == "same")) "same" else paste(unique(r), collapse = "; ")
  }
  base$hc_cl_table <- compare_df(x$hc_cl$table, y$hc_cl$table, "dist")
  base$hc_cl_band <- compare_df(x$hc_cl$predictions, y$hc_cl$predictions, "proportion")
  base$hp_estimate <- if (identical(x$hp$threshold, y$hp$threshold)) "same" else "differ"
  base$hp_curve <- compare_df(x$hp$predictions, y$hp$predictions, "proportion")
  base$hp_cl_table <- compare_df(x$hp_cl$table, y$hp_cl$table, "dist")
  base$hp_cl_band <- compare_df(x$hp_cl$predictions, y$hp_cl$predictions, "proportion")
  base$boot_settings <- if (identical(call_settings(x$calls), call_settings(y$calls))) "same" else "differ"
  base
})
res <- dplyr::bind_rows(rows)
utils::write.csv(res, args[3], row.names = FALSE)
checks <- setdiff(names(res), c("data", "dists", "rescale"))
cat("Scenarios:", nrow(res), "\n")
for (ch in checks) {
  v <- res[[ch]]
  cat(sprintf("  %-14s %s\n", ch, paste(names(table(v)), table(v), sep = ": ", collapse = ", ")))
}
