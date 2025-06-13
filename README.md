
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `xplainfi`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jemus42/xplainfi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jemus42/xplainfi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jemus42/xplainfi/graph/badge.svg)](https://app.codecov.io/gh/jemus42/xplainfi)
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
#> Key: <feature, iter_rsmp>
#>        feature iter_rsmp iter_perm classif.ce_orig classif.ce_perm   importance
#>         <char>     <int>     <int>           <num>           <num>        <num>
#>   1:       age         1         1       0.2095808       0.2305389  0.020958084
#>   2:       age         1         2       0.2095808       0.2335329  0.023952096
#>   3:       age         1         3       0.2095808       0.2275449  0.017964072
#>   4:       age         1         4       0.2095808       0.2215569  0.011976048
#>   5:       age         1         5       0.2095808       0.2155689  0.005988024
#>  ---                                                                           
#> 596: telephone         6         1       0.2612613       0.2432432 -0.018018018
#> 597: telephone         6         2       0.2612613       0.2552553 -0.006006006
#> 598: telephone         6         3       0.2612613       0.2612613  0.000000000
#> 599: telephone         6         4       0.2612613       0.2522523 -0.009009009
#> 600: telephone         6         5       0.2612613       0.2402402 -0.021021021
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
#> Key: <feature, iter_rsmp>
#>                     feature iter_rsmp iter_perm classif.ce_orig classif.ce_perm
#>                      <char>     <int>     <int>           <num>           <num>
#>  1:                     age         1         1       0.2732733       0.2762763
#>  2:                  amount         1         1       0.2732733       0.2852853
#>  3:          credit_history         1         1       0.2732733       0.2972973
#>  4:                duration         1         1       0.2732733       0.2852853
#>  5:     employment_duration         1         1       0.2732733       0.2792793
#>  6:          foreign_worker         1         1       0.2732733       0.2732733
#>  7:                 housing         1         1       0.2732733       0.2792793
#>  8:        installment_rate         1         1       0.2732733       0.2972973
#>  9:                     job         1         1       0.2732733       0.2702703
#> 10:          number_credits         1         1       0.2732733       0.2702703
#> 11:           other_debtors         1         1       0.2732733       0.2852853
#> 12: other_installment_plans         1         1       0.2732733       0.2792793
#> 13:           people_liable         1         1       0.2732733       0.2822823
#> 14:     personal_status_sex         1         1       0.2732733       0.2762763
#> 15:       present_residence         1         1       0.2732733       0.2792793
#> 16:                property         1         1       0.2732733       0.2762763
#> 17:                 purpose         1         1       0.2732733       0.2882883
#> 18:                 savings         1         1       0.2732733       0.2762763
#> 19:                  status         1         1       0.2732733       0.3273273
#> 20:               telephone         1         1       0.2732733       0.2702703
#>                     feature iter_rsmp iter_perm classif.ce_orig classif.ce_perm
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
#> 16:  0.003003003
#> 17:  0.015015015
#> 18:  0.003003003
#> 19:  0.054054054
#> 20: -0.003003003
#>       importance
```
