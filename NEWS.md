# xplainfi 0.1.0.9003

This turns out to be still a period of major changes in the early phase, so, uhm, well.

## General changes and improvements

- `$importance` becomes a function `$importance()` with arguments `standardize` and `ci_method` (#40):
  - `"nadeau_bengio"` implements the correction method by Nadeau & Bengio (2003) recommended by Molnar et al. (2023).
  - `"quantiles"` gives empirical quantiles covering `conf_level`.
  - `"cpi"` implements CPI analogous to [cpi](https://bips-hb.github.io/cpi/articles/intro.html), supporting t-, Wilcoxon-, and Fisher tests.
- `$scores` becomes `$scores()` for more fleixibility analogous to `$importance()`. Original scores are stored in `private$.scores` (for now).
- Both `$importance()` and `$scores()` have argument `relation` defaulting to `"differrence"` which allows to calculate importances like PFI as either the difference or the ratio of baseline and post-modification loss. The argument is moved out of `$compute()` to avoid having to recompute any predictions or model refits.
- Add `sim_dgp_ewald()` and other `sim_dgp_*()` helpers to simulate data (in `Task` form) with simple DGPs as used for illustration in Ewald et al. (2024) for example, which should make it easier to interpret the results of various importance methods.

### Observation-wise losses

- `$obs_loss()` analogously to `$scores()` computes observation-wise importance scores based on losses stored in `$.obs_losses` **if** `measure` has a `Measure$obs_loss()` function.
- `$predictions` are kept for now just in case they're useful

### Groups of features

- `PerturbationImportance` and `WVIM` gain `groups` argument which overrides `features` and enables the definition of groups of features
- Example: `groups = list(effects = c("x1", "x2", "x3"), noise = c("noise1", "noise2"))`
- In the output of `$importance()`, `$scores()`, `$obs_loss()` the `feature` column gets the name of the group, which effectively becomes a "meta feature" as far as the output is concerned.

## Method-specific changes

### `LeaveOutIn` -> `WVIM`

- Williamson's Variable Importance Measure (WVIM) generalizes LOCO / LOCI
- New implementation built around `mlr3fselect`, greatly simplifying the internals

### `PerturbationImportance`

- Streamline and speedup `PerturbationImportance` implementation, also by using `learner$predict_newdata_fast()` (#39), bumping the mlr3 dependency >= 1.1.0.
- Now batches `iter_perms` internally to reduce the number of calls to `sampler$sample()`. 
  - May need further adjustment in case of large data / large `iters_perm` as intermediate data could grow too large

### Conditional sampling

- **Breaking**: Refactor `FeatureSampler` API to separate task-based and external data sampling (#49):
  - `$sample(feature, row_ids = NULL)` now samples from stored task using row IDs
  - `$sample_newdata(feature, newdata)` samples from external data (e.g., test set)
- Extend `ARFSampler` to store more arguments on construction, making it easier to "preconfigure" the sampler via arguments used in `$sample()`.
- Standardize on `conditioning_set` as the name for the character vector defining features to condition on in `ConditionalSampler` and `RFI`.
- Add `KnockoffSampler` (#16 via @mnwright)
  - Now supports `row_ids`-based sampling from stored task data
  - Can be used with CFI (but not yet with RFI which requires `sample_newdata()`)
  - Add convenience wrappers `KnockoffGaussianSampler` and `KnockoffSequentialSampler`
  - Added `iters` param to create multiple `x_tilde` up front, which then allow repeated samplings, so you can get multiple different knockoff values per row id

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
