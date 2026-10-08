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

#' Create a runner for slow jobs
#'
#' A runner runs one job at a time. `invoke(fn, args)` starts `do.call(fn,
#' args)`, `running()` is `TRUE` until it settles, and `done()` then becomes
#' `list(n, value)` or `list(n, error)`, `n` counting the jobs settled.
#' `cancel()` stops a running job, whose result is then discarded.
#'
#' With mirai daemons set (`inst/app/global.R`), jobs run on a daemon through
#' an [shiny::ExtendedTask], so the session, and every other session in the
#' same R process, stays responsive. Without daemons (as in `testServer()`
#' tests) a job runs in the session as soon as it is invoked.
#'
#' @param background Logical scalar: whether to run jobs on mirai daemons.
#' @return A list of the runner's functions.
#' @keywords internal
task_runner <- function(background = mirai::daemons_set()) {
  running <- reactiveVal(FALSE)
  done <- reactiveVal(NULL)
  n <- 0
  finish <- function(result) {
    running(FALSE)
    n <<- n + 1
    done(c(list(n = n), result))
  }

  if (!background) {
    invoke <- function(fn, args) {
      running(TRUE)
      finish(tryCatch(
        list(value = do.call(fn, args)),
        error = function(e) list(error = conditionMessage(e))
      ))
    }
    return(list(invoke = invoke, running = running, done = done, cancel = function() NULL))
  }

  current <- NULL
  task <- ExtendedTask$new(function(fn, args) {
    current <<- mirai::mirai(
      tryCatch(
        list(value = do.call(fn, args)),
        error = function(e) list(error = conditionMessage(e))
      ),
      fn = fn,
      args = args
    )
    current
  })

  # A cancelled job settles as an error after running() was cleared, so only
  # the result of a job still running is used.
  observe({
    status <- task$status()
    if (!status %in% c("success", "error") || !isolate(running())) {
      return()
    }
    isolate(finish(tryCatch(
      task$result(),
      error = function(e) list(error = conditionMessage(e))
    )))
  })

  cancel <- function() {
    if (!is.null(current)) mirai::stop_mirai(current)
    running(FALSE)
  }
  session <- getDefaultReactiveDomain()
  if (!is.null(session)) session$onSessionEnded(cancel)

  list(
    invoke = function(fn, args) {
      running(TRUE)
      task$invoke(fn, args)
    },
    running = running,
    done = done,
    cancel = cancel
  )
}

# The percents affected whose confidence limits Get CL always computes: the
# BCANZ hazard concentrations. Together they cost little more than the
# threshold alone, as every percent is computed from the same bootstrap fits.
cl_percents <- c(1, 5, 10, 20)

#' Compute bootstrap confidence limits
#'
#' Bootstraps the model-averaged hazard concentrations at every whole percent
#' and the threshold (for the plot's band, the model-averaged row of the
#' confidence limits table and the report), and the estimates of each
#' distribution for the table: by percent, at the threshold and at
#' [cl_percents]; by concentration, the fraction affected at `conc`, with its
#' model average. The percents of one call share their bootstrap fits, so
#' extra percents cost little. A model-averaged curve already bootstrapped
#' with the same fit and number of samples (`pred`) is used rather than
#' computed again.
#'
#' @param fit A `fitdists` object.
#' @param threshold_type Character string: `"Concentration"` (a hazard
#'   concentration for `percent`) or `"Fraction"` (the fraction affected at
#'   `conc`).
#' @param percent Numeric scalar percent of species affected.
#' @param conc Numeric scalar concentration.
#' @param nboot Integer scalar number of bootstrap samples.
#' @param pred Model-averaged hazard concentrations with confidence limits at
#'   every whole percent and the threshold (see [model_average_cl()]), or
#'   `NULL` to bootstrap them.
#' @return A list of `pred` (model-averaged hazard concentrations), `hc` (each
#'   distribution's hazard concentrations, or `NULL`), `hp` (each
#'   distribution's fraction affected and their model average, or `NULL`) and
#'   `n_dists` (the number of distributions fitted).
#' @keywords internal
cl_job <- function(fit, threshold_type, percent, conc, nboot, pred = NULL) {
  pred <- pred %||% model_average_cl(fit, percent, nboot)
  result <- list(pred = pred, hc = NULL, hp = NULL, n_dists = length(fit))
  if (threshold_type == "Concentration") {
    result$hc <- ssdtools::ssd_hc_bcanz(
      fit,
      proportion = sort(unique(c(cl_percents, percent))) / 100,
      ci = TRUE,
      average = FALSE,
      nboot = nboot,
      min_pboot = 0.8
    )
    return(result)
  }
  hp <- function(average) {
    ssdtools::ssd_hp_bcanz(
      fit,
      conc = conc,
      ci = TRUE,
      average = average,
      nboot = nboot,
      min_pboot = 0.8,
      proportion = TRUE
    )
  }
  result$hp <- list(dists = hp(average = FALSE), average = if (length(fit) > 1) hp(average = TRUE))
  result
}

#' Bootstrap the model-averaged hazard concentrations
#'
#' The model-averaged curve with confidence limits at every whole percent and
#' at `percent` (when it is not a whole percent): the plot's band, the
#' model-averaged row of the confidence limits table and the report's hazard
#' concentrations. Get CL and Get Report share it (see [has_percent()]).
#'
#' @param fit A `fitdists` object.
#' @param percent Optional numeric scalar percent of species affected.
#' @param nboot Integer scalar number of bootstrap samples.
#' @return Model-averaged hazard concentrations, as from
#'   [ssdtools::ssd_hc()].
#' @keywords internal
model_average_cl <- function(fit, percent = NULL, nboot) {
  loadNamespace("ssdtools")
  stats::predict(
    fit,
    proportion = unique(c(1:99, percent)) / 100,
    nboot = nboot,
    ci = TRUE
  )
}

#' Whether predictions include a percent affected
#'
#' @param pred Model-averaged hazard concentrations.
#' @param percent Numeric scalar percent of species affected.
#' @return A flag.
#' @keywords internal
has_percent <- function(pred, percent) {
  !is.null(percent) && any(abs(pred$proportion - percent / 100) < 1e-9)
}

#' Confidence limits table
#'
#' @param cl Confidence limits: the list returned by [cl_job()].
#' @param threshold_type,percent,conc The current threshold, as for [cl_job()].
#' @return A tibble of the model-averaged threshold estimate and that of each
#'   distribution, by descending weight, or `NULL` when `cl` does not cover
#'   the threshold.
#' @keywords internal
cl_table <- function(cl, threshold_type, percent, conc) {
  if (threshold_type == "Concentration") {
    if (is.null(cl$hc) || is.null(percent)) {
      return(NULL)
    }
    at_percent <- function(x) x[abs(x$proportion - percent / 100) < 1e-9, ]
    dists <- at_percent(cl$hc)
    average <- at_percent(cl$pred)
  } else {
    if (is.null(cl$hp) || !identical(cl$conc, conc)) {
      return(NULL)
    }
    dists <- cl$hp$dists
    average <- cl$hp$average
  }
  if (nrow(dists) == 0) {
    return(NULL)
  }
  if (cl$n_dists == 1) {
    average <- dplyr::mutate(dists, dist = "average")
  }
  dplyr::bind_rows(average, dists) |>
    dplyr::select(-dplyr::any_of(c("dists", "samples"))) |>
    dplyr::mutate(dplyr::across(c("est", "se", "ucl", "lcl", "wt"), ~ signif(.x, 3))) |>
    dplyr::arrange(dplyr::desc(.data$wt))
}

#' Confidence limits as text
#'
#' @param table The confidence limits table (from [cl_table()]), or `NULL`.
#' @param scale Numeric scalar to multiply the limits by: 100 to show a
#'   fraction affected as a percent.
#' @param big_mark,decimal_mark Character strings of the number marks.
#' @return The model-averaged lower and upper limits in brackets, separated
#'   by an en dash and preceded by a space, or `""` without a table.
#' @keywords internal
cl_limits_text <- function(table, scale = 1, big_mark = ",", decimal_mark = ".") {
  if (is.null(table)) {
    return("")
  }
  average <- table[table$dist == "average", ]
  limits <- vapply(
    c(average$lcl, average$ucl) * scale,
    format,
    "",
    big.mark = big_mark,
    decimal.mark = decimal_mark
  )
  sprintf(" (%s\u2013%s)", limits[[1]], limits[[2]])
}

#' Report confidence limits
#'
#' @param pred Model-averaged hazard concentrations with confidence limits,
#'   as returned by [ssdtools::ssd_hc_bcanz()].
#' @return The rows of the report's HC1, HC5, HC10 and HC20, with their
#'   protection levels.
#' @keywords internal
report_cl <- function(pred) {
  proportion <- c(0.01, 0.05, 0.1, 0.2)
  pred[vapply(pred$proportion, function(p) any(abs(p - proportion) < 1e-9), logical(1)), ] |>
    dplyr::mutate(HCx = .data$proportion * 100, PCx = (1 - .data$proportion) * 100) |>
    dplyr::select("HCx", "PCx", "est", "se", "lcl", "ucl", "nboot", "pboot")
}

#' Compute the report
#'
#' Renders the HTML report, with the hazard concentrations from the
#' model-averaged curve `pred`, bootstrapping the curve first when it is not
#' given.
#'
#' @param fit A `fitdists` object.
#' @param nboot Integer scalar number of bootstrap samples.
#' @param pred Model-averaged hazard concentrations with confidence limits at
#'   every whole percent (from [model_average_cl()]), or `NULL` to bootstrap
#'   them.
#' @param params Named list of report parameters other than `pred_cl`.
#' @param template Character string file name of the report template.
#' @return A list of `pred` (the model-averaged curve), `pred_cl` (the
#'   report's hazard concentrations) and `html` (the rendered report as one
#'   string).
#' @keywords internal
report_job <- function(fit, nboot, pred, params, template) {
  pred <- pred %||% model_average_cl(fit, nboot = nboot)
  params$pred_cl <- report_cl(pred)
  html <- tempfile(fileext = ".html")
  on.exit(unlink(html), add = TRUE)
  render_report(template, params, output_format = "html_document", output_file = html)
  list(
    pred = pred,
    pred_cl = params$pred_cl,
    html = paste(readLines(html, warn = FALSE), collapse = "\n")
  )
}
