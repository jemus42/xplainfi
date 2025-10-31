#' Feature Importance Method Class
#'
#' @export
FeatureImportanceMethod = R6Class(
	"FeatureImportanceMethod",
	public = list(
		#' @field label (`character(1)`) Method label.
		label = NA_character_,
		#' @field task ([mlr3::Task])
		task = NULL,
		#' @field learner ([mlr3::Learner])
		learner = NULL,
		#' @field measure ([mlr3::Measure])
		measure = NULL,
		#' @field resampling ([mlr3::Resampling]), instantiated upon construction.
		resampling = NULL,
		#' @field resample_result ([mlr3::ResampleResult]) of the original `learner` and `task`, used for baseline scores.
		resample_result = NULL,
		# TODO: list of features, for grouped importance
		#' @field features (`character`: `NULL`) Features of interest. By default, importances will be computed for each feature
		#'   in `task`, but optionally this can be restricted to at least one feature. Ignored if `groups` is specified.
		features = NULL,
		#' @field groups (`list`: `NULL`) A (named) list of features (names or indices as in `task`).
		#'   If `groups` is specified, `features` is ignored.
		#'   Importances will be calculated for group of features at a time, e.g., in [PFI] not one but the group of features will be permuted at each step.
		#'   Analogusly in [WVIM], each group of features will be left out (or in) for each model refit.
		#'   Not all methods support groups (e.g., [SAGE]).
		#'   See FIXME: vignette or examples.
		groups = NULL,
		#' @field param_set ([paradox::ps()])
		param_set = ps(),
		#' @field predictions ([data.table][data.table::data.table]) Feature-specific prediction objects provided for some methods ([PFI], [WVIM]). Contains columns for feature of interest, resampling iteration, refit or perturbation iteration, and [mlr3::Prediction] objects.
		predictions = NULL,

		#' @description
		#' Creates a new instance of this [R6][R6::R6Class] class.
		#' This is typically intended for use by derived classes.
		#' @param task,learner,measure,resampling,features,groups,param_set,label Used to set fields
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			groups = NULL,
			param_set = paradox::ps(),
			label
		) {
			self$task = mlr3::assert_task(task)
			self$learner = mlr3::assert_learner(learner, task = task, task_type = task$task_type)

			if (is.null(measure)) {
				self$measure = switch(
					task$task_type,
					"classif" = mlr3::msr("classif.ce"),
					"regr" = mlr3::msr("regr.mse")
				)
				cli::cli_alert_info(
					"No {.cls Measure} provided, using {.code measure = msr({self$measure$id})}"
				)
			} else {
				self$measure = mlr3::assert_measure(measure, task = task, learner = learner)
			}
			self$param_set = paradox::assert_param_set(param_set)
			self$label = checkmate::assert_string(label, min.chars = 1)

			# Check features / groups
			# Default to using features, unless groups is specified
			if (is.null(groups)) {
				checkmate::assert_subset(features, self$task$feature_names, empty.ok = TRUE)
				self$features = features %||% self$task$feature_names
			} else {
				self$groups = check_groups(groups, all_features = self$task$feature_names)
				# check_groups ensures this produces a unique character vector
				self$features = unlist(groups, use.names = FALSE)
			}

			# resampling: default to holdout with default ratio if NULL
			if (is.null(resampling)) {
				resampling = mlr3::rsmp("holdout")$instantiate(task)
				cli::cli_inform(c(
					i = "No {.cls Resampling} provided",
					"Using {.code resampling = rsmp(\"holdout\")} with default {.code ratio = {round(resampling$param_set$values$ratio, 2)}}."
				))
			} else {
				# Clone the resampling to avoid instantiating the resampling in the user's workspace
				resampling = mlr3::assert_resampling(resampling)$clone()
			}
			if (!resampling$is_instantiated) {
				resampling$instantiate(task)
			}
			self$resampling = resampling
		},

		#' @description
		#' Compute feature importance scores
		#' @param store_backends (`logical(1): TRUE`) Whether to store backends.
		compute = function(store_backends = TRUE) {
			stop("Abstract method. Use a concrete implementation.")
		},

		#' @description
		#' Get aggregated importance scores.
		#' The stored [`measure`][mlr3::Measure] object's `aggregator` (default: `mean`) will be used to aggregated importance scores
		#' across resampling iterations and, depending on the method use, permutations ([PerturbationImportance] or refits [LOCO]).
		#' @param relation (character(1)) How to relate perturbed scores to originals ("difference" or "ratio"). If `NULL`, uses stored parameter value. This is only applicable for methods where importance is based on some
		#' relation between baseline and post-modifcation loss, i.e. [PerturbationImportance] methods such as [PFI] or [WVIM] / [LOCO]. Not available for [SAGE] methods.
		#' @param standardize (`logical(1)`: `FALSE`) If `TRUE`, importances are standardized by the highest score so all scores fall in `[-1, 1]`.
		#' @param ci_method (`character(1)`: `"none"`) Variance estimation method to use, defaulting to omitting variance estimation (`"none"`).
		#'   If `"raw"`, uncorrected variance estimates are provided purely for informative purposes with **invalid** (too narrow) confidence intervals.
		#'   If `"nadeau_bengio"`, variance correction is performed according to Nadeau & Bengio (2003) as suggested by Molnar et al. (2023).
		#'   If `"quantile"`, empirical quantiles are used to construct confidence-like intervals.
		#'   These methods are model-agnostic and rely on suitable `resampling`s, e.g. subsampling with 15 repeats for `"nadeau_bengio"`.
		#'   See details.
		#' @param conf_level (`numeric(1): 0.95`): Conficence level to use for confidence interval construction when `ci_method != "none"`.
		#' @param ... Additional arguments passen to specialized methods, if any.
		#' @return ([data.table][data.table::data.table]) Aggregated importance scores. with variables `"feature", "importance"`
		#' and depending in `ci_method` also `"se", "conf_lower", "conf_upper"`.
		#'
		#' @details
		#' Variance estimates for importance scores are biased due to the resampling procedure. Molnar et al. (2023) suggest to use
		#' the variance correction factor proposed by Nadeau & Bengio (2003) of n2/n1, where n2 and n1 are the sizes of the test- and train set, respectively.
		#' This should then be combined with approx. 15 iterations of either bootstrapping or subsampling.
		#'
		#' The use of bootstrapping in this context can lead to problematic information leakage when combined with learners
		#' that perform bootstrapping themselves, e.g., Random Forest learners.
		#' In such cases, observations may be used as train- and test instances simultaneously, leading to erroneous performance estimates.
		#'
		#' An approach leading to still imperfect, but improved variance estimates could be:
		#'
		#' ```r
		#' PFI$new(
		#'   task = sim_dgp_interactions(n = 1000),
		#'   learner = lrn("regr.ranger", num.trees = 100),
		#'   measure = msr("regr.mse"),
		#'   # Subsampling instead of bootstrapping due to RF
		#'   resampling = rsmp("subsampling", repeats = 15),
		#'   n_repeats = 5
		#' )
		#' ```
		#'
		#' `n_repeats = 5` in this context only improves the stability of the PFI estimate within the resampling iteration, whereas `rsmp("subsampling", repeats = 15)`
		#' is used to accounter for learner variance and neccessitates variance correction factor.
		#'
		#' This appraoch can in principle also be applied to `CFI` and `RFI`, but beware that a conditional sample such as [ConditionalARFSampler] also needs to be trained on data,
		#' which would need to be taken account by the variance estimation method.
		#' Analogously, the `"nadeau_bengio"` correction was recommended for the use with [PFI] by Molnar et al., so its use with other methods like [LOCO] or [SAGE] is experimental.
		#'
		#' Note that even if `measure` uses an `aggregator` function that is not the mean, variance estimation currently will always use [mean()] and [var()].
		#'
		#' @references
		#' `r print_bib("nadaeu_2003")`
		#' `r print_bib("molnar_2023")`
		#'
		importance = function(
			relation = NULL,
			standardize = FALSE,
			ci_method = c("none", "raw", "nadeau_bengio", "quantile"),
			conf_level = 0.95,
			...
		) {
			if (is.null(private$.scores)) {
				cli::cli_inform(c(
					x = "No importances computed yet!"
				))
				return(invisible(NULL))
			}

			# Use registry for validation instead of hardcoded match.arg
			# This allows subclasses to add custom methods
			if (length(ci_method) > 1) {
				ci_method = ci_method[1]
			}
			checkmate::assert_choice(ci_method, choices = private$.ci_methods)
			checkmate::assert_number(conf_level, lower = 0, upper = 1)

			# Get aggregator and scores
			aggregator = self$measure$aggregator %||% mean
			scores = self$scores(relation = relation)

			# FIXME: This had to be disabled to allow CPI with 1 holdout and 1 iter
			# But t-based tests now fail because we get df = self$resample_result$iters - 1 = 0
			# Skip aggregation if only one row per feature anyway
			# if (nrow(scores) == length(unique(scores$feature))) {
			# 	res = scores[, list(feature, importance)]
			# 	setkeyv(res, "feature")
			# 	return(res)
			# }

			# Standardize first so variance calculations use standardized values
			if (standardize) {
				scores[, importance := importance / max(abs(importance), na.rm = TRUE)]
			}

			# Dispatch to appropriate variance method
			# Check if method exists in registry (for extensibility)
			method_name = glue::glue(".importance_{ci_method}")
			available_methods = private$.ci_methods

			if (!ci_method %in% available_methods) {
				cli::cli_abort(c(
					"Variance method {.val {ci_method}} not found.",
					i = "Available methods: {.val {available_methods}}"
				))
			}

			# Call the appropriate private method
			agg_importance = private[[method_name]](
				scores = scores,
				aggregator = aggregator,
				conf_level = conf_level,
				...
			)

			setkeyv(agg_importance, "feature")
			agg_importance[]
		},

		#' @description
		#' Calculate observation-wise importance scores.
		#'
		#' Requires that `$compute()` was run and that `measure` is decomposable and
		#' has an observation-wise loss (`Measure$obs_loss()`) associated with it.
		#' This is not the case for measure like `classif.auc`, which is not decomposable.
		#'
		#' @param relation (character(1)) How to relate perturbed scores to originals ("difference" or "ratio"). If `NULL`, uses stored parameter value. This is only applicable for methods where importance is based on some
		#' relation between baseline and post-modifcation loss, i.e. [PerturbationImportance] methods such as [PFI] or [WVIM] / [LOCO]. Not available for [SAGE] methods.
		#'
		obs_loss = function(relation = NULL) {
			if (is.null(self$measure$obs_loss)) {
				cli::cli_warn(c(
					x = "{.cls Measure} {.val {self$measure$id}} does not have an observation-wise loss:",
					i = "Is it decomposable?"
				))
				return(invisible(NULL))
			}
			if (is.null(private$.obs_losses)) {
				cli::cli_warn(c(
					x = "No observation-wise losses stored!",
					i = "Did you run {.fun $compute}?",
					i = "Not all methods support observation-wise losses"
				))
				return(invisible(NULL))
			}

			relation = resolve_param(relation, self$param_set$values$relation, "difference")

			# Prepare baseline losses
			obs_loss_baseline = self$resample_result$obs_loss(measures = self$measure)
			# obs_loss_baseline[, let(truth = NULL, response = NULL)]
			setnames(
				obs_loss_baseline,
				old = c("iteration", self$measure$id),
				new = c("iter_rsmp", "loss_baseline")
			)

			obs_loss_combined = obs_loss_baseline[
				private$.obs_losses,
				on = .(iter_rsmp, row_ids),
				allow.cartesian = TRUE
			]

			obs_loss_combined[,
				obs_importance := private$.compute_score(
					loss_baseline,
					loss_post,
					relation = relation
				)
			]

			# Select / reorder column names, some may be
			# specific to methods and may not be present
			names_to_keep = c(
				"feature",
				"iter_rsmp",
				"iter_repeat",
				"row_ids",
				"loss_baseline",
				"loss_post",
				"obs_importance"
			)
			names_to_keep = intersect(names_to_keep, colnames(obs_loss_combined))

			obs_loss_combined[, .SD, .SDcols = names_to_keep][]
		},

		#' @description
		#' Resets all stored fields populated by `$compute`: `$resample_result`, `$scores`, `$obs_losses`, and `$predictions`.
		reset = function() {
			self$resample_result = NULL
			private$.scores = NULL
			private$.obs_losses = NULL
			self$predictions = NULL
			# SAGE-specific fields (only reset if they exist)
			if ("n_permutations_used" %in% names(self)) {
				self$n_permutations_used = NULL
			}
		},

		#' @description
		#' Print importance scores
		#'
		#' @param ... Passed to `print()`
		print = function(...) {
			cli::cli_h2(self$label)
			cli::cli_ul()
			cli::cli_li("Learner: {.val {self$learner$id}}")
			cli::cli_li("Task: {.val {self$task$id}}")
			if (is.null(self$groups)) {
				cli::cli_li(
					"{.emph {length(self$features)}} feature{?s} of interest: {.val {self$features}}"
				)
			} else {
				cli::cli_li("{.emph {length(self$groups)}} feature group{?s} of interest:")
				ol = cli::cli_ol()
				for (i in seq_along(groups)) {
					cli::cli_li("{.strong {names(groups)[i]}}: {.val {groups[[i]]}}")
				}
				cli::cli_end(ol)
			}
			cli::cli_li("Resampling: {.val {self$resampling$id}} ({.val {self$resampling$iters}} iters)")

			cli::cli_li("Parameters:")

			pv = self$param_set$values
			pidx = seq_along(pv)
			sapply(pidx, \(i) {
				cli::cli_ul("{.code {names(pv)[i]}}: {.val {pv[i]}}")
			})
			cli::cli_end()
			self$importance()
		},

		#' @description
		#' Calculate importance scores for each resampling iteration and sub-iterations
		#' (`iter_rsmp` in [PFI] for example).
		#'
		#' Iteration-wise importance are computed on the fly depending on the chosen relation
		#' (`difference` or `ratio`) to avoid re-computation if only a different relation is needed.
		#'
		#' @param relation (character(1)) How to relate perturbed scores to originals ("difference" or "ratio"). If `NULL`, uses stored parameter value. This is only applicable for methods where importance is based on some
		#' relation between baseline and post-modifcation loss, i.e. [PerturbationImportance] methods such as [PFI] or [WVIM] / [LOCO]. Not available for [SAGE] methods.
		#'
		scores = function(relation = NULL) {
			if (is.null(private$.scores)) {
				cli::cli_warn(c(
					x = "No importances computed yet!",
					i = "Did you run {.fun $compute}?"
				))
				return(invisible(NULL))
			}
			if ("importance" %in% colnames(private$.scores)) {
				# If there is already an importance variable in the stored scores like in SAGE,
				# we can't calculate pre/post scores like in PFI, LOCO etc,
				# individual "scores" would have different meaning there
				return(private$.scores)
			}

			relation = resolve_param(relation, self$param_set$values$relation, "difference")

			scores = data.table::copy(private$.scores)[,
				importance := private$.compute_score(
					score_baseline,
					score_post,
					relation = relation
				)
			]

			setnames(
				scores,
				old = c("score_baseline", "score_post"),
				new = c(paste0(self$measure$id, c("_baseline", "_post")))
			)

			scores[]
		}
	),
	private = list(
		# Registry of available variance methods
		.ci_methods = c("none", "raw", "nadeau_bengio", "quantile"),

		# Variance estimation methods
		# Each method takes scores, aggregator, and conf_level as parameters
		# Returns a data.table with feature and importance columns
		# plus optionally se, conf_lower, conf_upper

		# No variance estimation - just aggregated performance
		# @param scores data.table with feature and importance columns
		# @param aggregator function to aggregate importance scores
		# @param conf_level ignored for this method
		.importance_none = function(scores, aggregator, conf_level) {
			agg_importance = scores[,
				list(importance = aggregator(importance)),
				by = feature
			]
			agg_importance
		},

		# Raw variance estimation without correction (too narrow CIs)
		# @param scores data.table with feature and importance columns
		# @param aggregator function to aggregate importance scores
		# @param conf_level confidence level for intervals
		# TODO: the aggregator / mean inconsistency here is weird, maybe using the
		# measure's aggregator is not a good idea because the measure is passed
		# on initialization before $compute() but here, post-compute, we might want to change it
		.importance_raw = function(scores, aggregator, conf_level) {
			agg_importance = scores[,
				list(importance = aggregator(importance)),
				by = feature
			]

			# Aggregate within resamplings first to get one row per resampling iter
			means_rsmp = scores[,
				list(importance = mean(importance)),
				by = c("iter_rsmp", "feature")
			]

			sds = means_rsmp[,
				list(se = sqrt(var(importance) / self$resample_result$iters)),
				by = feature
			]

			agg_importance = agg_importance[sds, on = "feature"]

			alpha = 1 - conf_level
			quant = qt(1 - alpha / 2, df = self$resample_result$iters - 1)

			agg_importance[, let(
				conf_lower = importance - quant * se,
				conf_upper = importance + quant * se
			)]

			agg_importance
		},

		# Nadeau & Bengio (2003) corrected variance estimation
		# @param scores data.table with feature and importance columns
		# @param aggregator function to aggregate importance scores
		# @param conf_level confidence level for intervals
		.importance_nadeau_bengio = function(scores, aggregator, conf_level) {
			# Validate resampling type
			if (!(self$resampling$id %in% c("bootstrap", "subsampling")) | self$resampling$iters < 10) {
				cli::cli_warn(c(
					"Resampling is of type {.val {self$resampling$id}} with {.val {self$resampling$iters}} iterations.",
					i = "The Nadeau & Bengio corrected t-test is recommended for resampling types {.val {c('bootstrap', 'subsampling')}} with >= 10 iterations"
				))
			}

			if (self$resampling$id == "bootstrap") {
				test_train_ratio = 0.632
			} else {
				# Calculate test/train ratio for subsampling
				ratio = self$resampling$param_set$values$ratio
				n = self$resampling$task_nrow
				n1 = round(ratio * n)
				n2 = n - n1
				test_train_ratio = n2 / n1
			}

			# Nadeau & Bengio adjustment factor
			adjustment_factor = 1 / self$resample_result$iters + test_train_ratio

			agg_importance = scores[,
				list(importance = aggregator(importance)),
				by = feature
			]

			# Aggregate within resamplings first
			means_rsmp = scores[,
				list(importance = mean(importance)),
				by = c("iter_rsmp", "feature")
			]

			sds = means_rsmp[,
				list(se = sqrt(adjustment_factor * var(importance))),
				by = feature
			]

			agg_importance = agg_importance[sds, on = "feature"]

			alpha = 1 - conf_level
			quant = qt(1 - alpha / 2, df = self$resample_result$iters - 1)

			agg_importance[, let(
				conf_lower = importance - quant * se,
				conf_upper = importance + quant * se
			)]

			agg_importance
		},

		# Empirical quantile-based confidence intervals
		# Uses quantile() to construct confidence-like intervals from resampling distribution
		# @param scores data.table with feature and importance columns
		# @param aggregator function to aggregate importance scores (used for point estimate)
		# @param conf_level confidence level for intervals
		.importance_quantile = function(scores, aggregator, conf_level) {
			# Aggregate within resamplings first to get one value per resampling iteration
			means_rsmp = scores[,
				list(importance = aggregator(importance)),
				by = c("iter_rsmp", "feature")
			]

			# For each feature, compute quantiles
			result_list = lapply(unique(means_rsmp$feature), function(feat) {
				feat_scores = means_rsmp[feature == feat, importance]

				# Point estimate using aggregator
				point_est = aggregator(feat_scores)

				# Compute empirical quantiles for CI
				alpha = 1 - conf_level
				probs = c(alpha / 2, 1 - alpha / 2)
				ci_vals = quantile(feat_scores, probs = probs, na.rm = TRUE)

				data.table(
					feature = feat,
					importance = point_est,
					conf_lower = ci_vals[1],
					conf_upper = ci_vals[2]
				)
			})

			rbindlist(result_list)
		},

		# Take the raw predictions as returned by $predict_newdata_fast and convert to Prediction object fitting the task type to simplify type-specific handling
		# @param raw_prediction `list` with elements `reponse` (vector) or `prob` (matrix) depending on task type.
		# @param test_row_ids `integer()` test set row ids, important to ensure predictions can be matched with original observations / baseline predictions
		# .construct_pred = function(raw_prediction, test_row_ids) {
		# 	truth = self$task$truth(rows = test_row_ids)

		# 	switch(
		# 		self$task$task_type,
		# 		classif = PredictionClassif$new(
		# 			row_ids = test_row_ids,
		# 			truth = truth,
		# 			response = raw_prediction$response, # vector of class names or NULL
		# 			prob = raw_prediction$prob # matrix for predict_type prob or NULL
		# 		),
		# 		regr = PredictionRegr$new(
		# 			row_ids = test_row_ids,
		# 			truth = truth,
		# 			response = raw_prediction$response # numeric
		# 		)
		# 	)
		# },

		# Utility to convert named list of groups of features into data.table to
		# make it a little easier to match group names and features in list columns etc
		# Used in WVIM where mlr3fselect stores "left in" features as list columns
		.groups_tbl = function() {
			group_tbl = data.table::data.table(
				group = names(self$groups),
				features_lst = unname(self$groups)
			)
			group_tbl[, features := vapply(features_lst, \(x) paste0(x, collapse = ";"), character(1))]
			group_tbl
		},

		# Scoring utility for computing importances
		#
		# Computes the `relation` of score before a change (e.g. PFI, LOCO, ...) and after.
		# If `minimize == TRUE`, then `scores_post - scores_pre` is computed for
		# `relation == "difference"`, otherwise `scores_pre - scores_post` is given.
		# If `minimize == FALSE`, then the order is flipped, insuring that "higher value" means "more important".
		# @param scores_pre,scores_post (`numeric()`) Vector of scores or loss values at baseline / before (`_pre`) a modification, and after (`_post`) a modification (e.g., permutation or refit).
		# @param relation (`character(1)`: `"difference"`) Calculate the difference or `"ratio"` between pre and post modification value.
		.compute_score = function(
			scores_pre,
			scores_post,
			relation = c("difference", "ratio")
		) {
			checkmate::assert_numeric(scores_pre, any.missing = FALSE)
			checkmate::assert_numeric(scores_post, any.missing = FALSE)
			checkmate::assert_true(length(scores_pre) == length(scores_post))
			relation = match.arg(relation)
			minimize = self$measure$minimize

			# General idea assuming a important feature:
			# For PFI and MSE (minimize == TRUE): post - baseline gives large value -> high importance
			# =="== and classif.acc (minimize == FALSE) -> post - baseline  = negative, so we flip
			# For WVIM when we "leave-in", the baseline scores are the empty model,
			# so we need to flip directions of the comparison as well to ensure "higher importance value" -> "more important"
			if (identical(self$direction, "leave-in")) {
				minimize = !minimize
			}

			# I know this could be more concise but for the time I prefer it to be very obvious in what happens when
			# General expectation -> higher score => more important
			if (minimize) {
				# Lower is better, e.g. ce, where scores_pre is expected to be smaller and scores_post larger
				switch(relation, difference = scores_post - scores_pre, ratio = scores_post / scores_pre)
			} else {
				# Higher is better, e.g. accuracy, where scores_pre is expected to be larger and scores_post smaller
				switch(relation, difference = scores_pre - scores_post, ratio = scores_pre / scores_post)
			}
		},

		# @field .scores ([data.table][data.table::data.table]) Iteration-wise importances scores. Essentially an aggregated form of .obs_losses (which may not be available), used as basis for the calculation in `$importance() `and `$scores()`.
		.scores = NULL,

		# @field .obs_losses ([data.table][data.table::data.table]) Observation-wise losses when available. Contains columns for row_ids, feature, iteration indices, individual loss values, and observation-wise losses for baseline and modified case.
		.obs_losses = NULL
	)
)
