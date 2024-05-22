
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/gaballench/tbea/workflows/R-CMD-check/badge.svg)](https://github.com/gaballench/tbea/actions)
<!-- badges: end -->

# tbea

## Overview

An R package for pre- and post-processing in phylogenetic and
divergence-times Bayesian inference. You can use it for processing
output of phylogenetic bayesian programs (such as
[Beast](https://www.beast2.org/) or
[MrBayes](http://nbisweden.github.io/MrBayes/index.html)) or to prepare
input files and decision making. It also supports similarity
calculations using intersection for comparing two given densities
(e.g. prior vs. posterior). Functionality has been added for interacting
with output from parsimony analyses in TNT, and concatenating
morphological and molecular matrices for FBD analyses or standard
topology estimation.

## Installation

For now only the github development version is available. You can
install it using `devtools`:

``` r
install.packages("tbea")
devtools::install_github("gaballench/tbea")
```

Once the package makes its way into CRAN the standard `install.packages`
may be used for installing stable versions.

## Authorship

This package was created and maintained by Gustavo A. Ballen
(@gaballench) and Sandra Reinales (@spreinalesl).

## Problems?

If you find a bug or unexpected behavior, please [file an
issue](https://github.com/gaballench/tbea/issues).
