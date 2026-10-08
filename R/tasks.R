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

#' Compute bootstrap confidence limits
#'
#' Bootstraps the model-averaged predictions at `proportion` (for the plot and
#' the report) and the estimates of each distribution at the threshold (for
#' the confidence limits table). A model-averaged hazard concentration is one
#' of the predictions, which share their bootstrap samples; a model-averaged
#' fraction affected is bootstrapped separately.
#'
#' @param fit A `fitdists` object.
#' @param threshold_type Character string: `"Concentration"` (a hazard
#'   concentration for `percent`) or `"Fraction"` (the percent affected at
#'   `conc`).
#' @param percent Numeric scalar percent of species affected.
#' @param conc Numeric scalar concentration.
#' @param nboot Integer scalar number of bootstrap samples.
#' @return A list of `pred` (model-averaged predictions), `dists` (the
#'   threshold estimate of each distribution) and `average` (the model-averaged
#'   threshold estimate by concentration, otherwise `NULL`).
#' @keywords internal
cl_job <- function(fit, threshold_type, percent, conc, nboot) {
  loadNamespace("ssdtools")
  pred <- stats::predict(
    fit,
    proportion = unique(c(1:99, percent)) / 100,
    nboot = nboot,
    ci = TRUE
  )
  if (threshold_type == "Concentration") {
    dists <- ssdtools::ssd_hc_bcanz(
      fit,
      proportion = percent / 100,
      ci = TRUE,
      average = FALSE,
      nboot = nboot,
      min_pboot = 0.8
    )
    average <- NULL
  } else {
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
    dists <- hp(average = FALSE)
    average <- if (length(fit) > 1) hp(average = TRUE)
  }
  list(pred = pred, dists = dists, average = average)
}

#' Confidence limits table
#'
#' @param result The list returned by [cl_job()].
#' @param fit The `fitdists` object `result` was computed from.
#' @param threshold_type,percent As for [cl_job()].
#' @return A tibble of the model-averaged threshold estimate and that of each
#'   distribution, by descending weight.
#' @keywords internal
cl_table <- function(result, fit, threshold_type, percent) {
  dists <- result$dists
  average <- if (length(fit) == 1) {
    dplyr::mutate(dists, dist = "average")
  } else if (threshold_type == "Concentration") {
    result$pred[abs(result$pred$proportion - percent / 100) < 1e-9, ]
  } else {
    result$average
  }
  dplyr::bind_rows(average, dists) |>
    dplyr::select(-dplyr::any_of(c("dists", "samples"))) |>
    dplyr::mutate(dplyr::across(c("est", "se", "ucl", "lcl", "wt"), ~ signif(.x, 3))) |>
    dplyr::arrange(dplyr::desc(.data$wt))
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
#' Bootstraps the report's hazard concentrations, unless they are given from
#' confidence limits already computed with the same fit and number of
#' bootstrap samples, and renders the HTML report.
#'
#' @param fit A `fitdists` object.
#' @param nboot Integer scalar number of bootstrap samples.
#' @param pred_cl The report's hazard concentrations (from [report_cl()]), or
#'   `NULL` to bootstrap them.
#' @param params Named list of report parameters other than `pred_cl`.
#' @param template Character string file name of the report template.
#' @return A list of `pred_cl` and `html` (the rendered report as one string).
#' @keywords internal
report_job <- function(fit, nboot, pred_cl, params, template) {
  if (is.null(pred_cl)) {
    pred_cl <- report_cl(ssdtools::ssd_hc_bcanz(
      fit,
      proportion = c(0.01, 0.05, 0.1, 0.2),
      ci = TRUE,
      nboot = nboot,
      min_pboot = 0.8
    ))
  }
  params$pred_cl <- pred_cl
  html <- tempfile(fileext = ".html")
  on.exit(unlink(html), add = TRUE)
  render_report(template, params, output_format = "html_document", output_file = html)
  list(pred_cl = pred_cl, html = paste(readLines(html, warn = FALSE), collapse = "\n"))
}
