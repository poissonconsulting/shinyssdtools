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

# Run the app's Fit and Predict server modules of one checkout over a grid of
# scenarios and save every result, with a log of the ssdtools calls made.
# See README.md.
# Usage: Rscript run.R <checkout> <out.rds> <nboot> [library]
args <- commandArgs(TRUE)
checkout <- args[1]
out <- args[2]
nboot <- as.integer(args[3])
if (length(args) > 3) .libPaths(c(args[4], .libPaths()))
options(shinyssdtools.daemons = 0, warn = 1)
suppressMessages({
  library(shiny)
  pkgload::load_all(checkout, quiet = TRUE, export_all = TRUE)
})
source(file.path(checkout, "tests/testthat/helpers.R"))
cat("checkout", checkout, " ssdtools", as.character(packageVersion("ssdtools")), "\n")

# Log every ssd_hc() and ssd_hp() call, and reset the seed at each, so a
# bootstrap gives the same samples whatever was computed before it.
calls <- list()
record <- function(name, env) {
  dots <- eval(quote(list(...)), env)
  x <- get("x", env)
  value <- dots$proportion %||% dots$conc
  calls[[length(calls) + 1]] <<- list(
    fn = name,
    dists = paste(names(x), collapse = ","),
    n = length(value),
    values = signif(value, 6),
    settings = dots[setdiff(names(dots), c("proportion", "conc", "save_to", "control", "samples"))]
  )
}
for (fn in c("ssd_hc", "ssd_hp")) {
  trace(fn, where = asNamespace("ssdtools"), print = FALSE, tracer = bquote({
    record(.(fn), environment())
    set.seed(42)
  }))
}

accepted <- function(f, args) args[intersect(names(args), names(formals(f)))]

csv <- function(name) {
  utils::read.csv(file.path(checkout, "tests/testthat/test-files", paste0(name, ".csv")), check.names = FALSE)
}
datasets <- list(
  boron = list(data = ssddata::ccme_boron, conc = "Conc"),
  cadmium = list(data = ssddata::ccme_cadmium, conc = "Conc"),
  chloride = list(data = ssddata::ccme_chloride, conc = "Conc"),
  endosulfan = list(data = ssddata::ccme_endosulfan, conc = "Conc"),
  glyphosate = list(data = ssddata::ccme_glyphosate, conc = "Conc"),
  silver = list(data = ssddata::ccme_silver, conc = "Conc"),
  uranium = list(data = ssddata::ccme_uranium, conc = "Conc"),
  tox1 = list(data = csv("test_tox1"), conc = "Value"),
  tox2 = list(data = csv("test_tox2"), conc = "Value"),
  tox3 = list(data = csv("test_tox3"), conc = "Toxicity"),
  nonsyntactic = list(data = csv("test_tox_nonsyntactic"), conc = "Toxicity value")
)
dist_sets <- list(
  default = c("gamma", "lgumbel", "llogis", "lnorm", "lnorm_lnorm", "weibull"),
  two = c("gamma", "lnorm"),
  one = "lnorm"
)
grid <- expand.grid(data = names(datasets), dists = names(dist_sets), rescale = FALSE, stringsAsFactors = FALSE)
grid <- rbind(grid, data.frame(data = c("boron", "uranium"), dists = "default", rescale = TRUE))

run <- function(sc) {
  d <- datasets[[sc$data]]
  data <- clean_ssd_data(d$data)
  # The data as the app's data module provides them, with syntactic names.
  names(data) <- make.names(names(data))
  data_mod <- mock_data_module(data = data)
  res <- list(scenario = sc)
  calls <<- list()

  fit_args <- accepted(mod_fit_server, list(
    translations = reactive(test_translations), lang = reactive("english"),
    data_mod = data_mod, big_mark = reactive(","), decimal_mark = reactive("."),
    main_nav = reactive("fit")
  ))
  testServer(mod_fit_server, args = fit_args, {
    session$setInputs(selectConc = d$conc, selectDist = dist_sets[[sc$dists]], rescale = sc$rescale, updateFit = 1)
    session$flushReact()
    res$fit <<- tryCatch(session$returned$fit_dist(), error = function(e) conditionMessage(e))
    res$gof <<- tryCatch(as.data.frame(session$returned$gof_table()), error = function(e) conditionMessage(e))
  })
  if (!inherits(res$fit, "fitdists")) {
    return(res)
  }
  res$params <- as.data.frame(ssdtools::estimates(res$fit, all_estimates = TRUE))

  fit_mod <- mock_fit_module(fit = res$fit, conc_column = d$conc)
  predict_args <- accepted(mod_predict_server, list(
    translations = reactive(test_translations), lang = reactive("english"),
    data_mod = data_mod, fit_mod = fit_mod, big_mark = reactive(","),
    decimal_mark = reactive("."), main_nav = reactive("predict")
  ))
  conc_value <- signif(stats::median(data[[make.names(d$conc)]]), 2)
  get <- function(f) tryCatch(as.data.frame(f()), error = function(e) conditionMessage(e))
  testServer(mod_predict_server, args = predict_args, {
    r <- session$returned
    for (p in c(1, 10, 20, 5)) {
      session$setInputs(threshType = "Concentration", thresh = as.character(p), includeCi = TRUE, bootSamp = as.character(nboot))
      session$flushReact()
      res$hc[[as.character(p)]] <<- list(threshold = r$threshold_values(), predictions = get(r$predictions))
    }
    session$setInputs(getCl = 1)
    session$flushReact()
    res$hc_cl <<- list(table = get(r$predict_cl), predictions = get(r$predictions))
    session$setInputs(threshType = "Fraction", conc = conc_value)
    session$flushReact()
    res$hp <<- list(threshold = r$threshold_values(), predictions = get(r$predictions))
    session$setInputs(getCl = 2)
    session$flushReact()
    res$hp_cl <<- list(table = get(r$predict_cl), predictions = get(r$predictions))
  })
  res$calls <- calls
  res
}

n <- as.integer(Sys.getenv("EQUIV_N", nrow(grid)))
results <- lapply(seq_len(n), function(i) {
  sc <- as.list(grid[i, ])
  t0 <- Sys.time()
  r <- tryCatch(run(sc), error = function(e) list(scenario = sc, error = conditionMessage(e)))
  cat(sprintf("%-13s %-8s rescale=%-5s %5.1fs %s\n", sc$data, sc$dists, sc$rescale,
    as.numeric(difftime(Sys.time(), t0, units = "secs")), r$error %||% ""))
  r
})
saveRDS(results, out)
