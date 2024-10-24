
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
#>                     feature    importance
#>                      <char>         <num>
#>  1:                     age  9.929091e-04
#>  2:                  amount  1.288294e-02
#>  3:          credit_history  1.218554e-02
#>  4:                duration  1.598605e-02
#>  5:     employment_duration  3.890717e-03
#>  6:          foreign_worker -1.202700e-03
#>  7:                 housing -8.016999e-04
#>  8:        installment_rate  3.599408e-03
#>  9:                     job -1.002799e-03
#> 10:          number_credits -2.402103e-03
#> 11:           other_debtors  5.898713e-03
#> 12: other_installment_plans -9.095922e-04
#> 13:           people_liable  5.994018e-07
#> 14:     personal_status_sex -1.807496e-03
#> 15:       present_residence  6.944070e-04
#> 16:                property  1.291111e-03
#> 17:                 purpose  2.486918e-03
#> 18:                 savings  1.819694e-02
#> 19:                  status  3.978829e-02
#> 20:               telephone  1.293209e-03
#>                     feature    importance
```

Retrieve scores later in `pfi$importance`.

When PFI is computed based on resampling with multiple iterations, and /
or multiple permutation iterations, the individual scores can be
retrieved as a `data.table`:

``` r
pfi$scores
#> Key: <feature, iter_rsmp, iter_perm>
#>      iter_rsmp   feature iter_perm classif.ce_perm classif.ce_orig   importance
#>          <int>    <char>     <int>           <num>           <num>        <num>
#>   1:         1       age         1       0.2305389       0.2095808  0.020958084
#>   2:         1       age         2       0.2335329       0.2095808  0.023952096
#>   3:         1       age         3       0.2275449       0.2095808  0.017964072
#>   4:         1       age         4       0.2215569       0.2095808  0.011976048
#>   5:         1       age         5       0.2155689       0.2095808  0.005988024
#>  ---                                                                           
#> 596:         6 telephone         1       0.2432432       0.2612613 -0.018018018
#> 597:         6 telephone         2       0.2552553       0.2612613 -0.006006006
#> 598:         6 telephone         3       0.2612613       0.2612613  0.000000000
#> 599:         6 telephone         4       0.2522523       0.2612613 -0.009009009
#> 600:         6 telephone         5       0.2402402       0.2612613 -0.021021021
```

Where `iter_rsmp` corresponds to the resampling iteration, i.e., 3 \* 2
= 6 for 2 repeats of 3-fold cross-validation, and `iter_perm`
corresponds to the permutation iteration, 5 in this case. While
`pfi$importance` contains the means across all iterations, `pfi$scores`
allows you to manually aggregate them in any way you see fit.

In the simplest case, you run PFI with a single resampling iteration
(holdout) and a single permutation iteration, and `pfi$importance` will
contain the same values as `pfi$scores`.

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
pfi_single$scores
#> Key: <feature, iter_rsmp, iter_perm>
#>     iter_rsmp                 feature iter_perm classif.ce_perm classif.ce_orig
#>         <int>                  <char>     <int>           <num>           <num>
#>  1:         1                     age         1       0.2762763       0.2732733
#>  2:         1                  amount         1       0.2852853       0.2732733
#>  3:         1          credit_history         1       0.2972973       0.2732733
#>  4:         1                duration         1       0.2852853       0.2732733
#>  5:         1     employment_duration         1       0.2792793       0.2732733
#>  6:         1          foreign_worker         1       0.2732733       0.2732733
#>  7:         1                 housing         1       0.2792793       0.2732733
#>  8:         1        installment_rate         1       0.2972973       0.2732733
#>  9:         1                     job         1       0.2702703       0.2732733
#> 10:         1          number_credits         1       0.2702703       0.2732733
#> 11:         1           other_debtors         1       0.2852853       0.2732733
#> 12:         1 other_installment_plans         1       0.2792793       0.2732733
#> 13:         1           people_liable         1       0.2822823       0.2732733
#> 14:         1     personal_status_sex         1       0.2762763       0.2732733
#> 15:         1       present_residence         1       0.2792793       0.2732733
#>       importance
#>            <num>
#>  1:  0.003003003
#>  2:  0.012012012
#>  3:  0.024024024
#>  4:  0.012012012
#>  5:  0.006006006
#>  6:  0.000000000
#>  7:  0.006006006
#>  8:  0.024024024
#>  9: -0.003003003
#> 10: -0.003003003
#> 11:  0.012012012
#> 12:  0.006006006
#> 13:  0.009009009
#> 14:  0.003003003
#> 15:  0.006006006
#>  [ reached getOption("max.print") -- omitted 6 rows ]
```
