
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `xplainfi`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jemus42/xplainfi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jemus42/xplainfi/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `xplainfi` is to collect common feature importance methods
under a unified and extensible interface.

For now, it is built specifically around [mlr3](https://mlr-org.com/),
as available abstractions for learners, tasks, measures, etc. greatly
simplify the implementation of importance measures.

## Installation

You can install the development version of `xplainfi` like so:

``` r
# install.packages(pak)
pak::pak("jemus42/xplainfi")
```

## Example

Here is a basic example on how to calculate PFI for a given learner and
task:

``` r
library(xplainfi)
library(mlr3)
library(mlr3learners)

task = tsk("german_credit")
learner = lrn("classif.ranger", num.trees = 500)
measure = msr("classif.ce")

pfi = PFI$new(
  task = task, 
  learner = learner,
  measure = measure,
  resampling = rsmp("cv", folds = 3)
)
pfi$compute()
#>                     age                  amount          credit_history 
#>             0.003998010             0.018995043             0.004009998 
#>                duration     employment_duration          foreign_worker 
#>             0.018021015             0.004999011            -0.001999005 
#>                 housing        installment_rate                     job 
#>             0.003006000             0.007013001            -0.000989013 
#>          number_credits           other_debtors other_installment_plans 
#>            -0.000995007             0.005997015             0.001999005 
#>           people_liable     personal_status_sex       present_residence 
#>            -0.002997009             0.004004004             0.003995013 
#>                property                 purpose                 savings 
#>             0.006000012             0.007987029             0.016999035 
#>                  status               telephone 
#>             0.050023077             0.003008997
```
