#' @title Perturbation Feature Importance Base Class
#'
#' @description Abstract base class for perturbation-based importance methods PFI, CFI, and RFI
#'
#' @export
PerturbationImportance = R6Class(
	"PerturbationImportance",
	inherit = FeatureImportanceMethod, # Inherit from existing base class
	public = list(
		#' @field sampler ([FeatureSampler]) Sampler object for feature perturbation
		sampler = NULL,

		#' @description
		#' Creates a new instance of the PerturbationImportance class
		#' @param task,learner,measure,resampling,features,groups Passed to [FeatureImportanceMethod].
		#' @param sampler ([FeatureSampler]) Sampler to use for feature perturbation.
		#' @param relation (`character(1)`: `"difference"`) How to relate perturbed and baseline scores. Can also be `"ratio"`.
		#' @param n_repeats (`integer(1)`: `1L`) Number of permutation/conditional sampling iterations. Can be overridden in `$compute()`.
		#' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. When `NULL`, predicts all `test_size * n_repeats` rows in one call. Use smaller values to reduce memory usage at the cost of more prediction calls. Can be overridden in `$compute()`.
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			groups = NULL,
			sampler = NULL,
			relation = "difference",
			n_repeats = 1L,
			batch_size = NULL
		) {
			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				groups = groups,
				label = "Feature Importance (Abstract Class)"
			)

			# If no sampler is provided, create a default one (implementation dependent)
			self$sampler = sampler

			# Knockoffs only generate one x_tilde, hence n_repeats > 1 is meaningless
			if (inherits(sampler, "KnockoffSampler") && n_repeats > sampler$param_set$values$iters) {
				cli::cli_inform(c(
					"Requested {.code n_repeats = {n_repeats}} permutations with {.cls {class(sampler)[[1]]}}",
					"!" = "A {.cls KnockoffSampler} was constructed with {.val {sampler$param_set$values$iters}} iterations",
					i = "Proceeding with {.code n_repeats = {sampler$param_set$values$iters}}",
					i = "Reconstruct {.cls {class(sampler)[[1]]}} with {.code iters >= {n_repeats}} or use {.cls ConditionalARFSampler} if repeated sampling is required."
				))
				n_repeats = sampler$param_set$values$iters
			}

			# Set up common parameters for all perturbation-based methods
			ps = paradox::ps(
				relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
				n_repeats = paradox::p_int(lower = 1, default = 1),
				batch_size = paradox::p_int(lower = 1, special_vals = list(NULL), default = NULL)
			)

			ps$values$relation = relation
			ps$values$n_repeats = n_repeats
			ps$values$batch_size = batch_size
			self$param_set = ps

			# Add CPI to variance methods registry
			private$.ci_methods = c(private$.ci_methods, "cpi")
		}
	),

	private = list(
		.compute_baseline = function(store_models = TRUE, store_backends = TRUE) {
			self$resample_result = resample(
				self$task,
				self$learner,
				self$resampling,
				store_models = store_models,
				store_backends = store_backends
			)
			# Prepare baseline scores
			scores_baseline = self$resample_result$score(self$measure)[,
				.SD,
				.SDcols = c("iteration", self$measure$id)
			]
			setnames(scores_baseline, old = self$measure$id, "score_baseline")
			setnames(scores_baseline, old = "iteration", "iter_rsmp")
			scores_baseline[]
		},

		# Common computation method for all perturbation-based methods
		.compute_perturbation_importance = function(
			n_repeats = NULL,
			batch_size = NULL,
			store_models = TRUE,
			store_backends = TRUE,
			sampler = NULL
		) {
			# Use provided sampler or default to self$sampler
			sampler = sampler %||% self$sampler

			n_repeats = resolve_param(n_repeats, self$param_set$values$n_repeats, 1L)
			batch_size = resolve_param(batch_size, self$param_set$values$batch_size, NULL)

			scores_baseline = private$.compute_baseline(store_backends = store_backends)

			# Get predictions for each resampling iter, permutation iter, feature
			# Create progress bar that tracks resampling_iter × feature/group combinations
			# if (xplain_opt("progress")) {
			# n_features_or_groups = length(self$groups %||% self$features)
			# total_iterations = self$resampling$iters * n_features_or_groups
			# progress_bar_id = cli::cli_progress_bar(
			# 	"Computing importances",
			# 	total = total_iterations
			# )
			# }

			all_preds = lapply(seq_len(self$resampling$iters), \(iter) {
				# Extract the learner here once because apparently reassembly is expensive
				this_learner = self$resample_result$learners[[iter]]
				test_row_ids = self$resampling$test_set(iter)
				test_size = length(test_row_ids)

				if (is.null(self$groups)) {
					iteration_proxy = self$features
					# name so lapply returns named list, used as idcol in rbindlist()
					names(iteration_proxy) = iteration_proxy
				} else {
					iteration_proxy = self$groups
				}

				# Use unified parallelization helper
				pred_per_feature = xplainfi_map(
					length(iteration_proxy),
					\(
						foi,
						task,
						learner,
						sampler,
						test_row_ids,
						n_repeats,
						batch_size,
						learner_packages,
						is_sequential = TRUE
					) {
						# Load required packages in parallel workers
						if (!is_sequential) {
							library("data.table")
							library("mlr3")
							library("xplainfi")
							for (pkg in learner_packages) {
								library(pkg, character.only = TRUE)
							}
						}

						# Sample feature - sampler handles conditioning appropriately
						test_row_ids_replicated = rep.int(test_row_ids, times = n_repeats)
						perturbed_data = sampler$sample(foi, row_ids = test_row_ids_replicated)

						# Split perturbed data into repeats (n_repeats elements, each with test_size rows)
						test_size = length(test_row_ids)
						perturbed_data_list = split(
							perturbed_data,
							rep(seq_len(n_repeats), each = test_size)
						)

						# Use batched prediction helper
						preds = predict_batched(
							learner = learner,
							data_list = perturbed_data_list,
							task = task,
							test_row_ids = test_row_ids,
							batch_size = batch_size
						)

						# Store predictions in data.table list column
						pred_per_perm = lapply(preds, \(pred) data.table::data.table(prediction = list(pred)))

						# Append iteration id for within-resampling permutations
						data.table::rbindlist(pred_per_perm, idcol = "iter_repeat")
					},
					iteration_proxy, # Varying argument
					.args = list(
						task = self$task,
						learner = this_learner,
						sampler = sampler,
						test_row_ids = test_row_ids,
						n_repeats = n_repeats,
						batch_size = batch_size,
						learner_packages = this_learner$packages
					)
				)

				# When groups are defined, "feature" is the group name
				# mild misnomer for convenience because if-else'ing the column name is annoying
				rbindlist(pred_per_feature, idcol = "feature")
			})
			# Append iteration id for resampling
			all_preds = rbindlist(all_preds, idcol = "iter_rsmp")
			# setkeyv(all_preds, cols = c("feature", "iter_rsmp"))

			# store predictions for future reference maybe?
			self$predictions = all_preds

			# Close progress bar
			# if (xplain_opt("progress")) {
			# 	cli::cli_progress_done(id = progress_bar_id)
			# }

			scores = data.table::copy(all_preds)[,
				score_post := vapply(
					prediction,
					\(p) p$score(measures = self$measure)[[self$measure$id]],
					FUN.VALUE = numeric(1)
				)
			]
			vars_to_keep = c("feature", "iter_rsmp", "iter_repeat", "score_baseline", "score_post")
			scores = scores[scores_baseline, on = c("iter_rsmp")]
			private$.scores = scores[, .SD, .SDcols = vars_to_keep]

			# for obs_loss:
			# Not all losses are decomposable so this is optional and depends on the provided measure
			if (!is.null(self$measure$obs_loss)) {
				grouping_vars = c("feature", "iter_rsmp", "iter_repeat")

				obs_loss_all <- all_preds[,
					{
						pred <- prediction[[1]]
						obs_loss_vals <- self$measure$obs_loss(
							truth = pred$truth,
							response = pred$response
						)

						list(
							row_ids = pred$row_ids,
							loss_post = obs_loss_vals
						)
					},
					by = grouping_vars
				]

				private$.obs_losses = obs_loss_all
			}
		},

		# Conditional Predictive Impact (CPI) using one-sided t-test
		# CPI is specifically designed for CFI with knockoff samplers
		# Based on Watson et al. (2021) and implemented in the cpi package
		# @param scores data.table with feature and importance columns (not used, we use obs_loss directly)
		# @param aggregator function (not used for CPI)
		# @param conf_level confidence level for one-sided CI
		# @test character(1) Type of test to perform. Fisher is recommended
		# @param B integer(1) Number of replications for Fisher test.
		.importance_cpi = function(
			scores,
			aggregator,
			conf_level,
			test = c("t", "wilcoxon", "fisher", "binomial"),
			B = 1999
		) {
			# CPI requires observation-wise losses
			if (is.null(private$.obs_losses)) {
				cli::cli_abort(c(
					"CPI requires observation-wise losses.",
					i = "Ensure {.code measure} has an {.fun $obs_loss} method."
				))
			}
			if (class(self)[[1]] != "CFI") {
				cli::cli_warn(c(
					"!" = "CPI is only known to yield valid inference for {.cls CFI}.",
					x = "Inference with {.cls PFI} is known to be invalid and other methods are not studied yet."
				))
			}
			# Get observation-wise importances (already computed as differences)
			obs_loss_data = self$obs_loss(relation = "difference")
			# We need **at most** one row per feature and row_id for valid inference
			# so we aggregate over iter_rsmp
			dupes = obs_loss_data[, .N, by = c("feature", "row_ids", "iter_repeat")][N > 1]

			if (nrow(dupes) >= 1) {
				cli::cli_warn(c(
					"Resampling is of type {.val {self$resampling$id}} with {.val {self$resampling$iters}} iterations.",
					"Found {.val {length(unique(dupes[, row_ids]))}} duplicated observation{?s} in test sets",
					x = "Confidence intervals will have wrong coverage!",
					i = "CPI requires each observation to appear {.emph at most once} in the test set(s)",
					i = "Use holdout resampling to ensure valid inference"
				))
			}
			# Aggregating here over n_repeats
			obs_loss_agg = obs_loss_data[,
				list(obs_importance = mean(obs_importance)),
				by = c("row_ids", "feature")
			]

			test = match.arg(test)
			test_function = switch(
				test,
				t = stats::t.test,
				wilcoxon = stats::wilcox.test,
				fisher = fisher_one_sided,
				binomial = binom_one_sided
			)
			# For each feature, perform one-sided test (alternative = "greater")
			# H0: importance <= 0, H1: importance > 0
			result_list = lapply(unique(obs_loss_agg$feature), function(feat) {
				feat_obs = obs_loss_agg[feature == feat, obs_importance]

				if (mean(feat_obs) == 0) {
					htest_result = list(
						estimate = 0,
						statistic = 0,
						p.value = 1,
						conf.int = 0
					)
				} else {
					# One-sided test
					htest_result = test_function(
						feat_obs,
						alternative = "greater",
						conf.level = conf_level,
						conf.int = TRUE
					)
				}

				data.table(
					feature = feat,
					importance = mean(feat_obs),
					se = sd(feat_obs) / sqrt(length(feat_obs)),
					statistic = htest_result$statistic,
					p.value = htest_result$p.value,
					conf_lower = htest_result$conf.int[1],
					conf_upper = Inf # One-sided test upper bound is infinity
				)
			})

			rbindlist(result_list, fill = TRUE)
		}
	)
)


#' @title Permutation Feature Importance
#'
#' @description
#' Implementation of Permutation Feature Importance (PFI) using modular sampling approach.
#' PFI measures the importance of a feature by calculating the increase in model error
#' when the feature's values are randomly permuted, breaking the relationship between
#' the feature and the target variable.
#'
#' @details
#' Permutation Feature Importance was originally introduced by Breiman (2001) as part of
#' the Random Forest algorithm. The method works by:
#' 1. Computing baseline model performance on the original dataset
#' 2. For each feature, randomly permuting its values while keeping other features unchanged
#' 3. Computing model performance on the permuted dataset
#' 4. Calculating importance as the difference (or ratio) between permuted and original performance
#'
#' @references
#' `r print_bib("breiman_2001")`
#' `r print_bib("fisher_2019")`
#' `r print_bib("strobl_2008")`
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3learners)
#' task = tgen("xor", d = 5)$generate(n = 100)
#' pfi = PFI$new(
#'   task = task,
#'   learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
#'   measure = msr("classif.ce"),
#'   resampling = rsmp("cv", folds = 3),
#'   n_repeats = 3
#' )
#' pfi$compute()
#' pfi$importance()
#' @export
PFI = R6Class(
	"PFI",
	inherit = PerturbationImportance,
	public = list(
		#' @description
		#' Creates a new instance of the PFI class
		#' @param task,learner,measure,resampling,features,groups,relation,n_repeats,batch_size Passed to [PerturbationImportance]
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			groups = NULL,
			relation = "difference",
			n_repeats = 1L,
			batch_size = NULL
		) {
			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				groups = groups,
				sampler = MarginalPermutationSampler$new(task),
				relation = relation,
				n_repeats = n_repeats,
				batch_size = batch_size
			)

			self$label = "Permutation Feature Importance"
		},

		#' @description
		#' Compute PFI scores
		#' @param n_repeats (`integer(1)`; `NULL`) Number of permutation iterations. If `NULL`, uses stored value.
		#' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. If `NULL`, uses stored value.
		#' @param store_models,store_backends (`logical(1)`: `TRUE`) Whether to store fitted models / data backends, passed to [mlr3::resample] internally
		#' for the initial fit of the learner.
		#' This may be required for certain measures and is recommended to leave enabled unless really necessary.
		compute = function(
			n_repeats = NULL,
			batch_size = NULL,
			store_models = TRUE,
			store_backends = TRUE
		) {
			# PFI uses the MarginalPermutationSampler directly
			private$.compute_perturbation_importance(
				n_repeats = n_repeats,
				batch_size = batch_size,
				store_models = store_models,
				store_backends = store_backends,
				sampler = self$sampler
			)
		}
	)
)

#' @title Conditional Feature Importance
#'
#' @description Implementation of CFI using modular sampling approach
#'
#' @references `r print_bib("blesch_2025")`
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#'
#' # Using default ConditionalARFSampler
#' cfi = CFI$new(
#'   task = task,
#'   learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
#'   measure = msr("classif.ce")
#' )
#' cfi$compute()
#' cfi$importance()
#' \dontrun{
#' # For more control over conditional sampling:
#' custom_sampler = ConditionalARFSampler$new(
#'   task = task,
#'   finite_bounds = "local" # can improve sampling behavior
#' )
#' cfi_custom = CFI$new(
#'   task = task,
#'   learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
#'   measure = msr("classif.ce"),
#'   sampler = custom_sampler
#' )
#' cfi_custom$compute()
#' cfi_custom$importance()
#' }
#' @export
CFI = R6Class(
	"CFI",
	inherit = PerturbationImportance,
	public = list(
		#' @description
		#' Creates a new instance of the CFI class
		#' @param task,learner,measure,resampling,features,groups,relation,n_repeats,batch_size Passed to [PerturbationImportance].
		#' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to instantiating `ConditionalARFSampler` internally with default parameters.
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			groups = NULL,
			relation = "difference",
			n_repeats = 1L,
			batch_size = NULL,
			sampler = NULL
		) {
			# Use ConditionalARFSampler by default for CFI
			if (is.null(sampler)) {
				sampler = ConditionalARFSampler$new(task)
				cli::cli_alert_info(
					"No {.code sampler} provided, using {.cls ConditionalARFSampler} with default settings."
				)
			}
			# checkmate::assert_class would expect sampler to inherit from all clases, but
			# the two are mutually exclusive (for now?)
			if (!inherits(sampler, c("ConditionalSampler", "KnockoffSampler"))) {
				cli::cli_abort(c(
					x = "Provided sampler is of class {.cls {class(sampler)[[1]]}}.",
					"!" = "Either a {.cls ConditionalSampler} or a {.cls KnockoffSampler} is needed for {.cls CFI}.",
					i = "Choose a supported {.cls FeatureSampler}, such as {.cls ConditionalARFSampler} or {.class KnockoffGaussianSampler}."
				))
			}
			if (
				inherits(sampler, "ConditionalSampler") &&
					!is.null(sampler$param_set$values$conditioning_set)
			) {
				cli::cli_warn(c(
					"!" = "Provided sampler has a pre-configured {.code conditioning_set}.",
					i = "To calculate {.cls CFI} correctly, {.code conditioning_set} will be reset such that sampling is performed conditionally on all remaining features."
				))
				sampler$param_set$values$conditioning_set = NULL
			}

			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				groups = groups,
				sampler = sampler,
				n_repeats = n_repeats,
				batch_size = batch_size
			)

			self$label = "Conditional Feature Importance"
		},

		#' @description
		#' Compute CFI scores
		#' @param n_repeats (integer(1)) Number of permutation iterations. If `NULL`, uses stored value.
		#' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. If `NULL`, uses stored value.
		#' @param store_models,store_backends (`logical(1)`: `TRUE`) Whether to store fitted models / data backends, passed to [mlr3::resample] internally
		#' for the initial fit of the learner.
		#' This may be required for certain measures and is recommended to leave enabled unless really necessary.
		compute = function(
			n_repeats = NULL,
			batch_size = NULL,
			store_models = TRUE,
			store_backends = TRUE
		) {
			# CFI expects sampler configured to condition on all other features for each feature
			# Default for ConditionalARFSampler
			private$.compute_perturbation_importance(
				n_repeats = n_repeats,
				batch_size = batch_size,
				store_models = store_models,
				store_backends = store_backends,
				sampler = self$sampler
			)
		}
	)
)

#' @title Relative Feature Importance
#'
#' @description RFI generalizes CFI and PFI with arbitrary conditioning sets and samplers.
#'
#' @references `r print_bib("konig_2021")`
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 200)
#' rfi = RFI$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   conditioning_set = c("important1")
#' )
#' rfi$compute()
#' rfi$importance()
#' @export
RFI = R6Class(
	"RFI",
	inherit = PerturbationImportance,
	public = list(
		#' @description
		#' Creates a new instance of the RFI class
		#' @param task,learner,measure,resampling,features,groups,relation,n_repeats,batch_size Passed to [PerturbationImportance].
		#' @param conditioning_set ([character()]) Set of features to condition on. Can be overridden in `$compute()`.
		#'   Default (`character(0)`) is equivalent to `PFI`. In `CFI`, this would be set to all features except that of interest.
		#' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to `ConditionalARFSampler`.
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			groups = NULL,
			conditioning_set = NULL,
			relation = "difference",
			n_repeats = 1L,
			batch_size = NULL,
			sampler = NULL
		) {
			# Use ConditionalARFSampler by default for RFI
			if (is.null(sampler)) {
				sampler = ConditionalARFSampler$new(task)
				cli::cli_alert_info(
					"No {.cls ConditionalSampler} provided, using {.cls ConditionalARFSampler} with default settings."
				)
			} else {
				checkmate::assert_class(sampler, "ConditionalSampler")
			}

			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				groups = groups,
				sampler = sampler,
				relation = relation,
				n_repeats = n_repeats,
				batch_size = batch_size
			)

			# Validate and set up conditioning set after task is available
			if (!is.null(conditioning_set)) {
				conditioning_set = checkmate::assert_subset(conditioning_set, self$task$feature_names)
			} else {
				# Default to empty set (equivalent(ish) to PFI)
				cli::cli_warn(c(
					"Using empty conditioning set",
					i = "Set {.code conditioning_set} to condition on features."
				))
				conditioning_set = character(0)
			}

			# Configure the sampler with the conditioning_set
			self$sampler$param_set$values$conditioning_set = conditioning_set

			# Create extended param_set for RFI with conditioning_set parameter
			rfi_ps = paradox::ps(
				conditioning_set = paradox::p_uty(default = character(0))
			)
			rfi_ps$values$conditioning_set = conditioning_set
			self$param_set = c(self$param_set, rfi_ps)

			self$label = "Relative Feature Importance"
		},

		#' @description
		#' Compute RFI scores
		#' @param conditioning_set (character()) Set of features to condition on. If `NULL`, uses the stored parameter value.
		#' @param n_repeats (integer(1)) Number of permutation iterations. If `NULL`, uses stored value.
		#' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. If `NULL`, uses stored value.
		#' @param store_models,store_backends (`logical(1)`: `TRUE`) Whether to store fitted models / data backends, passed to [mlr3::resample] internally
		#' for the initial fit of the learner.
		#' This may be required for certain measures and is recommended to leave enabled unless really necessary.
		compute = function(
			conditioning_set = NULL,
			n_repeats = NULL,
			batch_size = NULL,
			store_models = TRUE,
			store_backends = TRUE
		) {
			# Handle conditioning_set parameter override
			if (!is.null(conditioning_set)) {
				# Validate the provided conditioning_set
				conditioning_set = checkmate::assert_subset(conditioning_set, self$task$feature_names)

				# Clear cache and temporarily modify sampler's conditioning_set
				self$scores = NULL
				old_conditioning_set = self$sampler$param_set$values$conditioning_set
				self$sampler$param_set$values$conditioning_set = conditioning_set
				on.exit(self$sampler$param_set$values$conditioning_set <- old_conditioning_set)
			}

			# Use the (potentially modified) sampler
			private$.compute_perturbation_importance(
				n_repeats = n_repeats,
				batch_size = batch_size,
				store_models = store_models,
				store_backends = store_backends,
				sampler = self$sampler
			)
		}
	)
)
