#' @title Williamson's Variable Importance Measure (WVIM)
#'
#' @description
#' Base class generalizazing refit-based variable importance measures.
#' Default corresponds to leaving out each feature `iters_refit` times, which
#' corresponds to LOCO (Leave One Covariate Out).
#'
#' @keywords internal
WVIM = R6Class(
	"WVIM",
	inherit = FeatureImportanceMethod,
	public = list(
		#' @field direction (`character(1)`) Either "leave-out" or "leave-in".
		direction = NULL,

		#' @description
		#' Creates a new instance of this [R6][R6::R6Class] class.
		#' @param task,learner,measure,resampling,features Passed to `FeatureImportanceMethod` for construction.
		#' @param direction (`character(1)`) Either "leave-out" or "leave-in".
		#' @param label (`character(1)`) Method label.
		#' @param iters_refit (`integer(1)`) Number of refit iterations per resampling iteration.
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			direction = c("leave-out", "leave_in"),
			label = "Williamson's Variable Importance Measure (WVIM)",
			iters_refit = 1L
		) {
			require_package("mlr3fselect")

			# Should this go in the param_set?
			direction = match.arg(direction)
			self$direction = direction
			checkmate::assert_int(iters_refit, lower = 1L)

			# params
			ps = ps(
				relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
				iters_refit = paradox::p_int(lower = 1L, default = 1L)
			)
			ps$values = list(
				relation = "difference",
				iters_refit = iters_refit
			)

			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				param_set = ps,
				label = label
			)
		},

		#' @description
		#' Computes leave-out or leave-in feature importance.
		# @param design (`data.table`) A design matrix indicating which features are to be left in or out
		# at each step. Can be created with [wvim_design_matrix].
		#' `wvim_design_matrix(task$feature_names, "leave-out")` corresponds to LOCO.
		#' @param store_backends (`logical(1)`) Passed to [mlr3::resample] to store
		#'   backends in resample result.
		#'   Required for some measures, but may increase memory footprint.
		compute = function(store_backends = TRUE) {
			design = wvim_design_matrix(
				all_features = self$task$feature_names,
				feature_names = self$features,
				direction = self$direction
			)

			design_dt = data.table::rbindlist(replicate(
				n = self$param_set$values$iters_refit,
				design,
				simplify = FALSE
			))

			private$.compute_wvim(design, store_backends)
		}
	),

	private = list(
		.compute_wvim = function(design, store_backends) {
			# Initial resampling
			self$resample_result = resample(
				self$task,
				self$learner,
				self$resampling,
				store_models = TRUE,
				store_backends = store_backends
			)
			# Prepare baseline scores
			scores_baseline = self$resample_result$score(self$measure)[,
				.SD,
				.SDcols = c("iteration", self$measure$id)
			]
			setnames(scores_baseline, old = self$measure$id, "score_baseline")
			setnames(scores_baseline, old = "iteration", "iter_rsmp")

			# Fselect section
			instance = mlr3fselect::fselect(
				fselector = mlr3fselect::fs("design_points", design = design),
				task = self$task,
				learner = self$learner,
				resampling = self$resampling,
				measure = self$measure
			)

			archive_dt = as.data.table(instance$archive)
			archive_base = copy(archive_dt)
			if (self$direction == "leave-out") {
				# FIXME: Needs to account for grouped features
				archive_base[,
					feature := vapply(
						features,
						\(x) {
							setdiff(task$feature_names, x)
						},
						FUN.VALUE = character(1)
					)
				]
			}
			archive_base = archive_base[, .(batch_nr, feature)]

			# Scores ---
			scores = instance$archive$benchmark_result$score(self$measure)
			scores = scores[, .SD, .SDcols = c("nr", "iteration", self$measure$id)]
			setnames(scores, self$measure$id, "score_post")
			setnames(scores, "iteration", "iter_rsmp")
			setnames(scores, "nr", "batch_nr")

			# merge baseline scores and post-modification scores
			scores = scores[scores_baseline, on = "iter_rsmp"]
			# join with batch_nr to identify the foi for eatch iteration
			scores = archive_base[scores, on = "batch_nr"]
			# Regain iters_refit (hacky but kind of works I guess)
			scores[, iter_refit := batch_nr %% (self$param_set$values$iters_refit) + 1]
			private$.scores = scores[, .(feature, iter_rsmp, iter_refit, score_baseline, score_post)]

			# obs losses ----
			if (!is.null(self$measure$obs_loss)) {
				obs_loss_vals = instance$archive$benchmark_result$obs_loss(self$measure)
				setnames(obs_loss_vals, "resample_result", "batch_nr")
				# add iter_refit to keep track
				obs_loss_vals[, iter_refit := (batch_nr %% (self$param_set$values$iters_refit) + 1)]

				obs_loss_vals = archive_base[obs_loss_vals, on = "batch_nr"]

				setnames(obs_loss_vals, "iteration", "iter_rsmp")
				setnames(obs_loss_vals, self$measure$id, "loss_post")

				private$.obs_losses = obs_loss_vals[, .(feature, iter_rsmp, iter_refit, row_ids, loss_post)]
			}
		}
	)
)

#' @title Leave-One-Covariate-Out (LOCO)
#'
#' @description
#' Calculates Leave-One-Covariate-Out (LOCO) scores.
#'
#' @details
#' LOCO measures feature importance by comparing model performance with and without
#' each feature. For each feature, the model is retrained without that feature and
#' the performance difference (reduced_model_loss - full_model_loss) indicates the
#' feature's importance. Higher values indicate more important features.
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", uietly = TRUE)
#' @export
#'
#' @references `r print_bib("lei_2018")`
LOCO = R6Class(
	"LOCO",
	inherit = WVIM,
	public = list(
		#' @description
		#' Creates a new instance of this [R6][R6::R6Class] class.
		#' @param task ([mlr3::Task]) Task to compute importance for.
		#' @param learner ([mlr3::Learner]) Learner to use for prediction.
		#' @param measure ([mlr3::Measure]) Measure to use for scoring.
		#' @param resampling ([mlr3::Resampling]) Resampling strategy. Defaults to holdout.
		#' @param features (`character()`) Features to compute importance for. Defaults to all features.
		#' @param iters_refit (`integer(1)`: `1L`) Number of refit iterations per resampling iteration.
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			iters_refit = 1L
		) {
			if (!is.null(features)) {
				# LOCO specifically does not "allow" grouped features
				checkmate::assert_character(features)
				checkmate::assert_subset(features, choices = task$feature_names)
			}

			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				direction = "leave-out",
				label = "Leave-One-Covariate-Out (LOCO)",
				iters_refit = iters_refit
			)
		},

		#' @description
		#' Compute LOCO importances.
		#'
		#' @param store_backends (`logical(1)`) Passed to [mlr3::resample] to store
		#'   backends in resample result.
		#'   Required for some measures, but may increase memory footprint.
		compute = function(store_backends = TRUE) {
			design = wvim_design_matrix(
				all_features = self$task$feature_names,
				feature_names = self$features,
				direction = self$direction
			)

			design_dt = data.table::rbindlist(replicate(
				n = self$param_set$values$iters_refit,
				design,
				simplify = FALSE
			))

			private$.compute_wvim(design, store_backends)
		}
	)
)
