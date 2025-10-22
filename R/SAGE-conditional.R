#' @title Conditional SAGE
#'
#' @description [SAGE] with conditional sampling (features are "marginalized" conditionally).
#' Uses [ARFSampler] as default [ConditionalSampler].
#'
#' @seealso [MarginalSAGE]
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sage = ConditionalSAGE$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   n_permutations = 3L,
#'   n_samples = 20
#' )
#' sage$compute()
#' @export
ConditionalSAGE = R6Class(
	"ConditionalSAGE",
	inherit = SAGE,
	public = list(
		#' @field sampler ([ConditionalSampler]) Sampler for conditional marginalization.
		sampler = NULL,

		#' @description
		#' Creates a new instance of the ConditionalSAGE class.
		#' @param task,learner,measure,resampling,features,n_permutations,batch_size,n_samples,early_stopping,convergence_threshold,se_threshold,min_permutations,check_interval Passed to [SAGE].
		#' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to [ARFSampler].
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			n_permutations = 10L,
			sampler = NULL,
			batch_size = 5000L,
			n_samples = 100L,
			early_stopping = FALSE,
			convergence_threshold = 0.01,
			se_threshold = Inf,
			min_permutations = 10L,
			check_interval = 2L
		) {
			# Use ARFSampler by default
			if (is.null(sampler)) {
				sampler = ARFSampler$new(task)
				cli::cli_alert_info(
					"No {.cls ConditionalSampler} provided, using {.cls ARFSampler} with default settings."
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
				n_permutations = n_permutations,
				batch_size = batch_size,
				n_samples = n_samples,
				early_stopping = early_stopping,
				convergence_threshold = convergence_threshold,
				se_threshold = se_threshold,
				min_permutations = min_permutations,
				check_interval = check_interval
			)

			# Store sampler (specific to ConditionalSAGE)
			self$sampler = sampler

			self$label = "Conditional SAGE"
		}
	),

	private = list(
		# Conditional-specific data expansion: Sample from conditional distribution
		# For each coalition, sample marginalized features conditioned on coalition features
		# Uses n_samples to create multiple samples per test instance for averaging
		.expand_coalitions_data = function(test_dt, all_coalitions) {
			n_test = nrow(test_dt)
			all_coalition_data = vector("list", length(all_coalitions))

			# For each coalition, sample from conditional distribution P(X_{-S} | X_S)
			for (i in seq_along(all_coalitions)) {
				coalition = all_coalitions[[i]]
				marginalize_features = setdiff(self$features, coalition)

				if (length(marginalize_features) > 0) {
					# Expand test data to sample multiple times for averaging
					# Each test instance is replicated n_samples times
					test_dt_expanded = test_dt[rep(
						seq_len(n_test),
						each = self$param_set$values$n_samples
					)]

					# Sample conditionally using the conditional sampler
					# Returns test_dt with marginalized features replaced by conditional samples
					# ARF sampler: P(marginalize_features | coalition_features)
					marginalized_test = self$sampler$sample_newdata(
						feature = marginalize_features,
						newdata = test_dt_expanded,
						conditioning_set = coalition
					)

					# Add test instance ID for tracking which original test instance each row belongs to
					# Must be done AFTER sampling since sampler does not preserve extra columns
					marginalized_test[,
						.test_instance_id := rep(
							seq_len(n_test),
							each = self$param_set$values$n_samples
						)
					]
				} else {
					# Empty coalition (all features marginalized) - just use original test data
					# No conditional sampling needed since there are no conditioning features
					marginalized_test = copy(test_dt)
					marginalized_test[, .test_instance_id := seq_len(n_test)]
				}

				# Add coalition ID for tracking which coalition this data belongs to
				marginalized_test[, .coalition_id := i]

				all_coalition_data[[i]] = marginalized_test
			}

			# Combine all coalitions into single data.table for batch prediction
			rbindlist(all_coalition_data)
		}
	)
)
