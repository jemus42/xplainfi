
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `xplainfi`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jemus42/xplainfi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jemus42/xplainfi/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/jemus42/xplainfi/graph/badge.svg?token=QIQDMP3AM7)](https://codecov.io/gh/jemus42/xplainfi)
<!-- badges: end -->

The goal of `xplainfi` is to collect common feature importance methods
under a unified and extensible interface.

It is built around [mlr3](https://mlr-org.com/) as available
abstractions for learners, tasks, measures, etc. greatly simplify the
implementation of importance measures.

## Installation

You can install the development version of `xplainfi` like so:

``` r
# install.packages(pak)
pak::pak("jemus42/xplainfi")
```

## Example: PFI

Here is a basic example on how to calculate PFI for a given learner and
task, using repeated cross-validation as resampling strategy and
computing PFI within each resampling 5 times:

``` r
library(xplainfi)
library(mlr3)
library(mlr3learners)

task = tsk("german_credit")
learner = lrn("classif.ranger", num.trees = 100)
measure = msr("classif.ce")

pfi = PFI$new(
  task = task,
  learner = learner,
  measure = measure,
  resampling = rsmp("repeated_cv", folds = 3, repeats = 2),
  iters_perm = 5
)
```

Compute and print PFI scores:

``` r
pfi$compute()
#> Key: <feature>
#>                     feature    importance          sd
#>                      <char>         <num>       <num>
#>  1:                     age  9.929091e-04 0.010690982
#>  2:                  amount  1.288294e-02 0.017911433
#>  3:          credit_history  1.218554e-02 0.015895416
#>  4:                duration  1.598605e-02 0.019922101
#>  5:     employment_duration  3.890717e-03 0.009580233
#>  6:          foreign_worker -1.202700e-03 0.003576501
#>  7:                 housing -8.016999e-04 0.009771957
#>  8:        installment_rate  3.599408e-03 0.006323238
#>  9:                     job -1.002799e-03 0.008086486
#> 10:          number_credits -2.402103e-03 0.005482047
#> 11:           other_debtors  5.898713e-03 0.005011316
#> 12: other_installment_plans -9.095922e-04 0.010947167
#> 13:           people_liable  5.994018e-07 0.005945255
#> 14:     personal_status_sex -1.807496e-03 0.009310394
#> 15:       present_residence  6.944070e-04 0.012069722
#> 16:                property  1.291111e-03 0.012186182
#> 17:                 purpose  2.486918e-03 0.014410343
#> 18:                 savings  1.819694e-02 0.013125963
#> 19:                  status  3.978829e-02 0.015578736
#> 20:               telephone  1.293209e-03 0.008761937
#>                     feature    importance          sd
```

Retrieve scores later in `pfi$importance`.

When PFI is computed based on resampling with multiple iterations, and /
or multiple permutation iterations, the individual scores can be
retrieved as a `data.table`:

``` r
str(pfi$scores)
#> Classes 'data.table' and 'data.frame':   600 obs. of  6 variables:
#>  $ feature        : chr  "age" "age" "age" "age" ...
#>  $ iter_rsmp      : int  1 1 1 1 1 2 2 2 2 2 ...
#>  $ iter_perm      : int  1 2 3 4 5 1 2 3 4 5 ...
#>  $ classif.ce_orig: num  0.21 0.21 0.21 0.21 0.21 ...
#>  $ classif.ce_perm: num  0.231 0.234 0.228 0.222 0.216 ...
#>  $ importance     : num  0.02096 0.02395 0.01796 0.01198 0.00599 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "sorted")= chr [1:2] "feature" "iter_rsmp"
```

Where `iter_rsmp` corresponds to the resampling iteration, i.e., 3 \* 2
= 6 for 2 repeats of 3-fold cross-validation, and `iter_perm`
corresponds to the permutation iteration within each resampling
iteration, 5 in this case. While `pfi$importance` contains the means and
standard deviations across all iterations, `pfi$scores` allows you to
manually aggregate them in any way you see fit.

In the simplest case, you run PFI with a single resampling iteration
(holdout) and a single permutation iteration, and `pfi$importance` will
contain the same importance values as `pfi$scores`.

``` r
pfi_single = PFI$new(
  task = task,
  learner = learner,
  measure = measure
)

pfi_single$compute()
#> Key: <feature>
#>                     feature   importance
#>                      <char>        <num>
#>  1:                     age  0.003003003
#>  2:                  amount  0.012012012
#>  3:          credit_history  0.024024024
#>  4:                duration  0.012012012
#>  5:     employment_duration  0.006006006
#>  6:          foreign_worker  0.000000000
#>  7:                 housing  0.006006006
#>  8:        installment_rate  0.024024024
#>  9:                     job -0.003003003
#> 10:          number_credits -0.003003003
#> 11:           other_debtors  0.012012012
#> 12: other_installment_plans  0.006006006
#> 13:           people_liable  0.009009009
#> 14:     personal_status_sex  0.003003003
#> 15:       present_residence  0.006006006
#> 16:                property  0.003003003
#> 17:                 purpose  0.015015015
#> 18:                 savings  0.003003003
#> 19:                  status  0.054054054
#> 20:               telephone -0.003003003
#>                     feature   importance
str(pfi_single$scores)
#> Classes 'data.table' and 'data.frame':   20 obs. of  6 variables:
#>  $ feature        : chr  "age" "amount" "credit_history" "duration" ...
#>  $ iter_rsmp      : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ iter_perm      : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ classif.ce_orig: num  0.273 0.273 0.273 0.273 0.273 ...
#>  $ classif.ce_perm: num  0.276 0.285 0.297 0.285 0.279 ...
#>  $ importance     : num  0.003 0.01201 0.02402 0.01201 0.00601 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "sorted")= chr [1:2] "feature" "iter_rsmp"
```
