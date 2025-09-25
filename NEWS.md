# xplainfi 0.1.0.9000

This turns out to be still a period of major changes in the early phase, so, uhm, well.

- Extend `ARFSampler` to store more arguments on construction, making it easier to "preconfigure" the sampler via arguments used in `$sample()`.
- Standardize on `conditioning_set` as the name for the character vector defining features to condition on in `ConditionalSampler` and `RFI`.
- Streamline and speedup `PerturbationImportance` implementation, also by using `learner$predict_newdata_fast()`, bumping the mlr3 dependency >= 1.1.0.
- Add `sim_dgp_ewald()` and other `sim_dgp_*()` helpers to simulate data (in `Task` form) with simple DGPs as used for illustration in Ewald et al. (2024) for example, which should make it easier to interpret the results of various importance methods.
- Add `KnockoffSampler` (#16 via @mnwright)
  - Currently does not support `conditioning_set`
- Add `$obs_loss` and `$predictions` fields to `FeatureImportanceMeasure`, now used by `LOCO` and `LOCI`
  - Both get arugments `obs_loss = FALSE` use the measure's `$aggregator` for aggregation in case of `obs_loss = TRUE`, to allow for  median of absolute differences calculation as in original LOCO formulation, rather than the "micro-"averaged approach calculated by default.
- `SAGE`:
  - Fix accidentally marginal `ConditionalSAGE`.
  -  Also using `learner$predict_newdata_fast()` now
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
