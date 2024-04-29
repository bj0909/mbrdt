
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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mbrdt)

## basic example code
```
