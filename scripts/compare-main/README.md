# Compare results with main

These scripts check that a branch gives the same fits, estimates and confidence limits as `main`.
They run the app's Fit and Predict server modules (with `shiny::testServer()`) of two checkouts over the same scenarios and compare every result.

## Scenarios

- Datasets: the CCME boron, cadmium, chloride, endosulfan, glyphosate, silver and uranium datasets from ssddata, and the four CSV files in `tests/testthat/test-files`.
- Distributions: the six default distributions, gamma and lnorm, and lnorm alone; boron and uranium are also fitted with the default distributions rescaled.
- For each fit: the hazard concentration at 1, 5, 10 and 20%; Get CL at 5%; the fraction affected at the median concentration of the data; and Get CL for it.

## Compared

- The distributions fitted, their parameter estimates and the goodness of fit table.
- The hazard concentrations and the fraction affected, and the model-averaged curve.
- The confidence limits table and the plot's confidence band, for a hazard concentration and for the fraction affected.
- The settings of every bootstrap (`ssdtools::ssd_hc()` and `ssdtools::ssd_hp()` calls with `ci = TRUE`).

Results are compared exactly, without a tolerance.
Before each `ssd_hc()` and `ssd_hp()` call, `run.R` sets the same seed, so a bootstrap gives the same samples whatever was computed before it.
The confidence limits at a percent do not depend on the other percents of the same call, so limits match even when a branch groups the percents of its bootstraps differently.

## Run

From the root of the branch, with `main` in another worktree:

```sh
git worktree add ../shinyssdtools-main origin/main
Rscript scripts/compare-main/run.R ../shinyssdtools-main main.rds 100
Rscript scripts/compare-main/run.R . branch.rds 100
Rscript scripts/compare-main/compare.R main.rds branch.rds comparison.csv
```

The third argument of `run.R` is the number of bootstrap samples.
An optional fourth argument is a library to load packages from first, to compare both checkouts with the same version of ssdtools.
Set the environment variable `EQUIV_N` to run only the first scenarios.

`compare.R` writes a row per scenario to the CSV file and prints a summary: "same", or the largest absolute difference.
