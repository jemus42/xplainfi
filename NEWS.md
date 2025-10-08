# xplainfi 0.1.0.9000

This turns out to be still a period of major changes in the early phase, so, uhm, well.

## General changes and improvements

- `$importance` becomes a function `$importance()` with arguments `standardize` and `variance_method` (#40):
  - `"nadeau_bengio"` implements the correction method by Nadeau & Bengio (2003) recommended by Molnar et al. (2023).
- `$scores` becomes `$scores()` for more fleixibility analogous to `$importance()`. Original scores are stored in `private$.scores` (for now).
- Both `$importance()` and `$scores()` have argument `relation` defaulting to `"differrence"` which allows to calculate importances like PFI as either the difference or the ratio of baseline and post-modification loss. The argument is moved out of `$compute()` to avoid having to recompute any predictions or model refits.
- Add `sim_dgp_ewald()` and other `sim_dgp_*()` helpers to simulate data (in `Task` form) with simple DGPs as used for illustration in Ewald et al. (2024) for example, which should make it easier to interpret the results of various importance methods.

### Observation-wise losses

-  `$obs_scores()` analogously computes observation-wise importance scores base on losses stored in `$.obs_losses` **if** `measure` has a `Measure$obs_loss()`

- `$predictions` will probably be removed again?

## Method-specific changes

### `LeaveOutIn` -> `WVIM`

- Williamson's Variable Importance Measure (WVIM) generalizes LOCO / LOCI
- Feature grouping API is still WIP
- New implementation built around `mlr3fselect`, greatly simplifying the internals

### `PerturbationImportance`

- Streamline and speedup `PerturbationImportance` implementation, also by using `learner$predict_newdata_fast()` (#39), bumping the mlr3 dependency >= 1.1.0.

### Conditional sampling

- Extend `ARFSampler` to store more arguments on construction, making it easier to "preconfigure" the sampler via arguments used in `$sample()`.
- Standardize on `conditioning_set` as the name for the character vector defining features to condition on in `ConditionalSampler` and `RFI`.
- Add `KnockoffSampler` (#16 via @mnwright)
  - Currently does not support `conditioning_set`
  - Implementation is still incomplete

### `SAGE`

- Fix accidentally marginal `ConditionalSAGE`.
- Also using `learner$predict_newdata_fast()` now  (#39)
- `batch_size` controls number of observations used at once per `learner$predict_newdata_fast()` call (could lead to excessive RAM usage). 
- convergence tracking if `early_stopping = TRUE` ([#29](https://github.com/jemus42/xplainfi/pull/29))
  - Permutations are evaluated in steps of `check_interval` at a time, after each convergence is checked
  - If values change by less than `convergence_threshold`, convergence is assumed
  - A `$converged` field is set to `TRUE`
  - At least `min_permutations` are perfomed in any case, and `$n_permutations_used` shows the number of performed permutations
  - `$convergence_history` tracks convergence history and can be analyzed to see per-feature values after each checkpoint
  -  `$plot_convergence_history()` plots convergence history per feature
  -  Convergence is tracked only for first resampling iteration
  -  Also add standard error tracking as part of the convergence history ([#33](https://github.com/jemus42/xplainfi/pull/33))


# xplainfi 0.1.0

- Initial prototype with 
	- PFI
	- CFI and RFI (via `arf`-powered conditional sampling)
	- SAGE (marginal and conditional, the latter via `arf`)
	- LOCO and LOCI
- Includes comparison to reference implementation in Python via `fippy`
