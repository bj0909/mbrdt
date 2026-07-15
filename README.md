
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mbrdt

<!-- badges: start -->

[![R-CMD-check](https://github.com/bj0909/mbrdt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bj0909/mbrdt/actions/workflows/R-CMD-check.yaml)
[![Travis build
status](https://travis-ci.com/bj0909/mbrdt.svg?branch=master)](https://travis-ci.com/bj0909/mbrdt)
<!-- badges: end -->

The goal of mbrdt is to provide tools for designing multistage binomial
reliability demonstration tests (MRDTs). It allows researchers and
practitioners to simulate and optimize the costs of MBRDT designs,
considering the multi-stage acceptance uncertainties and the potential
subsequent costs of RDT, such as reliability growth costs and warranty
service costs.

The maintained package is self-contained: no external historical dataset
is required. Test plans, simulation samples, stage outputs, and
post-processing summaries are generated from documented function
arguments and random seeds.

The workflow has been tested with R 4.2.1. Rebuilding the README and
vignette also requires `knitr`, `rmarkdown`, and Pandoc; these are not
runtime data dependencies.

## Installation

You can install the development version of mbrdt like so:

``` r
# Install remotes package if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
# Install the package from GitHub
remotes::install_github("bj0909/mbrdt")
```

## Main workflow

The main simulation function is `multiStageRdt()`. It enumerates
candidate test plans, generates Monte Carlo and Metropolis-Hastings
samples internally, and writes `data_<stage>.csv` plus
`params_stage_<stage+1>.csv` in the current working directory.

The general post-processing workflow uses:

- `read_stage_outputs()` or `combine_stage_outputs()` to align stage
  results;
- `calculate_recursive_costs()` to calculate costs backward over any
  number of stages;
- `select_minimum_cost_plans()` to require `consumer_risk <= beta` at
  every stage and retain all minimum-cost ties;
- `write_postprocess_results()` for optional CSV reports in an explicit
  output directory.

A complete fast example is provided in
[`inst/examples/minimal_end_to_end.R`](inst/examples/minimal_end_to_end.R),
with its configuration in
[`inst/examples/example_parameters.R`](inst/examples/example_parameters.R).
Run it from the repository root after installing the package:

``` powershell
Rscript --vanilla inst/examples/minimal_end_to_end.R
```

See [`HANDOFF.md`](HANDOFF.md) for the tested environment,
generated-output classification, data policy, known assumptions, and
future-maintenance guidance.

## Post-processing example

Stage outputs are supplied explicitly as data frames. Expected warranty
cost (`expW`) is the probability-weighted stage contribution produced by
the package. The calculation works backward from the final stage and
enforces consumer risk at every stage. Plan identifiers remain fixed
across stages, so rejection continues the recursion for the same aligned
plan rather than dynamically switching to another plan.

``` r
library(mbrdt)

stage_1 <- data.frame(
  plan = c("A", "B"), AP = c(0.7, 0.8), CR_b = c(0.03, 0.02),
  testing_cost = c(100, 120), expW = c(40, 35)
)
stage_2 <- data.frame(
  plan = c("A", "B"), AP = c(0.8, 0.9), CR_b = c(0.06, 0.03),
  testing_cost = c(100, 120), expW = c(30, 25)
)

stages <- combine_stage_outputs(
  list(stage_1, stage_2),
  id_cols = "plan"
)
costs <- calculate_recursive_costs(
  stages,
  continuation_costs = 50,
  terminal_penalty = 500
)
selection <- select_minimum_cost_plans(costs, beta = 0.05)
selection$selected_plans
#>   plan total_expected_cost feasible
#> 2    B                 204     TRUE
```

## Data and outputs

No historical simulation output is needed or supported as an input
dependency. Users should generate new stage outputs from a documented
parameter configuration. Large historical research outputs, manuscripts,
and presentations are intentionally excluded from the code repository
and releases.

Stage files are regenerable inputs to the file-based post-processing
workflow. Parameter files and optional report CSVs are also regenerable
and do not need to be archived with the package source.
