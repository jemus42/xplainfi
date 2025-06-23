# xplainfi 0.1.0.9000

- Extend `ARFSampler` to store more arguments on construction, making it easier to "preconfigure" the sampler via arguments used in `$sample()`.
- Standardize on `conditioning_set` as the name for the character vector defining features to condition on in `ConditionalSampler` and `RFI`.
- Streamline `PerturbationImportance` implementation.
- Add `batch_size` argument for `SAGE` methods to control number of observations used at once per `learner$predict_newdata()` call (could lead to excessive RAM usage). 
- Add `sim_dgp_ewald()` to simulate data with a simple DGP as used for illustration in Ewald et al. (2024), which should make it easier to interpret the results of various importance methods.
- Add `KnockoffSampler` (#16 via @mnwright)
  - Currently does not support `conditioning_set`
- Add `$obs_loss` and `$predictions` fields to `FeatureImportanceMeasure`, nosw used by `LOCO` and `LOCI`
  - Both get arugments `obs_loss = FALSE` and `aggregation_fun`, defaulting to `median` in case of `obs_loss = TRUE`, to allow for macro-averaged median of absolute differences calculcation as in original LOCO formulation, rather than the micro-averaged approach calculated by default.
- Fix accidentally marginal `ConditionalSAGE`.

# xplainfi 0.1.0

- Initial prototype with 
	- PFI
	- CFI and RFI (via `arf`-powered conditional sampling)
	- SAGE (marginal and conditional, the latter via `arf`)
	- LOCO and LOCI
- Includes comparison to reference implementation in Python via `fippy`
