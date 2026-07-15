# MBRDT Paper 1 Code Handoff

## Purpose

`mbrdt` is the maintained R implementation of the multistage binomial
reliability demonstration testing methodology developed for Paper 1. The
repository supports test-plan enumeration, Monte Carlo and Metropolis-Hastings
simulation, reliability-growth updating, recursive cost post-processing, and
all-stage consumer-risk constrained plan selection.

The package provides a maintainable implementation of the methodology. The
small example is illustrative and does not attempt to reproduce the published
Paper 1 numerical results.

## Authoritative source and recommended release

The authoritative code repository is:

<https://github.com/bj0909/mbrdt>

The recommended stable handoff release is `v1.0.0-paper1-handoff`. Future work
should start from that release or a later release in the authoritative GitHub
repository, not from historical school-laptop copies.

Manuscripts and presentations are distributed separately. They are not package
dependencies and are not included in the repository.

## Tested environment

The handoff workflow was validated with:

- R 4.2.1 on 64-bit Windows;
- knitr 1.51 and rmarkdown 2.31 for documentation builds;
- Pandoc 3.1.1 for vignette and README rendering;
- `R CMD check --no-manual` with `Status: OK`.

The installed package uses base R plus the recommended `stats` and `utils`
packages. `knitr`, `rmarkdown`, and Pandoc are needed to rebuild documentation,
not to run the core installed package.

## Installation

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("bj0909/mbrdt@v1.0.0-paper1-handoff")
```

For source validation from a clone:

```powershell
R CMD build .
R CMD check --no-manual mbrdt_0.1.0.tar.gz
```

## Repository structure

- `R/`: simulation and post-processing functions.
- `man/`: generated function documentation.
- `tests/`: compact synthetic tests.
- `inst/examples/example_parameters.R`: documented small parameter configuration.
- `inst/examples/minimal_end_to_end.R`: complete simulation-to-selection example.
- `README.md`: installation and workflow overview.
- `vignettes/`: package vignette infrastructure.

## Running the minimal example

From the repository root after installing the package:

```powershell
Rscript --vanilla inst/examples/minimal_end_to_end.R
```

The script creates a unique temporary output directory, restores the caller's
working directory on exit, runs a small two-stage simulation, reads the stage
outputs, calculates recursive expected costs, applies the consumer-risk
threshold at every stage, selects minimum-cost feasible plans, and writes small
CSV summaries.

The configuration is separate from the runner so that users can review or copy
it without editing workflow code.

## Generated outputs

`multiStageRdt()` writes into the current working directory:

- `data_<stage>.csv`: stage plan identifiers, acceptance probability, consumer
  risk, testing cost, and expected warranty cost. These are the supported inputs
  to file-based post-processing.
- `params_stage_<stage+1>.csv`: fitted Beta parameters and goodness-of-fit
  diagnostics. These are regenerable diagnostics and are not required by
  post-processing.

The optional report writer creates:

- `<prefix>_plans.csv`;
- `<prefix>_stage_costs.csv`;
- `<prefix>_selected_plans.csv`.

Monte Carlo and Metropolis-Hastings samples are generated internally and are not
written by the maintained workflow.

## Data policy

No external historical dataset is required. Test plans, probability samples,
posterior samples, stage outputs, and post-processing summaries are generated
from function arguments and the documented random seed.

Large historical simulation outputs are generated, uncurated research
artifacts. They are not part of the supported code handoff, are not included in
GitHub releases, and should not be treated as package inputs. Users should
generate new outputs from a documented parameter configuration.

## Assumptions and limitations

- `expW` is an already probability-weighted expected warranty-cost contribution.
- Plan identifiers remain fixed across stages; recursion does not dynamically
  switch to a different plan after rejection.
- A plan is feasible only when every stage-specific consumer risk is less than
  or equal to `beta`.
- Nonterminal rejection incurs the configured continuation cost and the next
  stage's recursive expected cost. Final-stage rejection incurs the terminal
  penalty.
- `multiStageRdt()` writes files implicitly to the current working directory and
  returns `"Done"`; the example isolates these writes in a temporary directory.
- The `G` simulation argument is retained for interface compatibility but the
  maintained stage calculation does not currently use it. Monetary continuation
  costs are supplied explicitly during post-processing.
- The small example prioritizes speed and workflow verification, not numerical
  accuracy suitable for a research study.

## Validation

The maintained validation sequence is:

```powershell
Rscript --vanilla inst/examples/minimal_end_to_end.R
Rscript --vanilla smoke-test.R
Rscript --vanilla tests/postprocessing.R
R CMD build .
R CMD check --no-manual mbrdt_0.1.0.tar.gz
```

## Starting points for future work

1. Add an explicit `output_dir` argument and structured return value to
   `multiStageRdt()` while retaining backward compatibility.
2. Expand validation for simulation argument ranges and MH diagnostics.
3. Decide whether the unused `G` simulation argument should be deprecated in a
   future interface version.
4. Add scientifically justified larger configurations separately from the fast
   handoff example; do not commit generated bulk outputs.
