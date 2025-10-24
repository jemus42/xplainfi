#' @title Conditional Inference Tree Conditional Sampler
#'
#' @description Implements conditional sampling using conditional inference trees (ctree).
#' Builds a tree predicting target features from conditioning features, then samples
#' from the terminal node corresponding to each test observation.
#'
#' @details
#' This sampler approximates the conditional distribution \eqn{P(X_B | X_A = x_A)} by:
#' 1. Building a conditional inference tree with \eqn{X_B} as response and \eqn{X_A} as predictors
#' 2. For each test observation, finding its terminal (leaf) node in the tree
#' 3. Sampling uniformly from training observations in that same terminal node
#'
#' Conditional inference trees (ctree) use permutation tests to determine splits,
#' which helps avoid overfitting and handles mixed feature types naturally. The tree
#' partitions the feature space based on the conditioning variables, creating local
#' neighborhoods that respect the conditional distribution structure.
#'
#' **Key advantages over other samplers:**
#' - Handles mixed feature types (continuous and categorical)
#' - Non-parametric (no distributional assumptions)
#' - Automatic feature selection (splits only on informative features)
#' - Can capture non-linear conditional relationships
#' - Statistically principled splitting criteria
#'
#' **Hyperparameters** control tree complexity:
#' - `mincriterion`: Significance level for splits (higher = fewer splits)
#' - `minsplit`: Minimum observations required for a split
#' - `minbucket`: Minimum observations in terminal nodes
#'
#' This implementation is inspired by shapr's ctree approach but simplified for our
#' use case (we build trees on-demand rather than pre-computing all subsets).
#'
#' **Advantages:**
#' - Works with any feature types
#' - Robust to outliers
#' - Interpretable tree structure
#' - Handles high-dimensional conditioning
#'
#' **Limitations:**
#' - Requires model fitting (slower than kNN)
#' - Can produce duplicates if terminal nodes are small
#' - Tree building time increases with data size
#'
#' @examples
#' \dontrun{
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#'
#' # Create sampler with default parameters
#' sampler = CtreeConditionalSampler$new(task)
#'
#' # Sample features conditioned on others
#' test_data = task$data(rows = 1:5)
#' sampled = sampler$sample_newdata(
#'   feature = c("important2", "important3"),
#'   newdata = test_data,
#'   conditioning_set = "important1"
#' )
#' }
#'
#' @references
#' - Hothorn, T., Hornik, K., & Zeileis, A. (2006). Unbiased recursive partitioning: A conditional inference framework. Journal of Computational and Graphical statistics, 15(3), 651-674.
#' - Aas, K., Jullum, M., & Løland, A. (2021). Explaining individual predictions when features are dependent: More accurate approximations to Shapley values. Artificial Intelligence, 298, 103502.
#'
#' @export
CtreeConditionalSampler = R6Class(
	"CtreeConditionalSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field tree_cache (`environment`) Cache for fitted ctree models.
		tree_cache = NULL,

		#' @description
		#' Creates a new CtreeConditionalSampler.
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param mincriterion (`numeric(1)`: `0.95`) Significance level threshold for splitting (1 - p-value).
		#'   Higher values result in fewer splits (simpler trees).
		#' @param minsplit (`integer(1)`: `20L`) Minimum number of observations required for a split.
		#' @param minbucket (`integer(1)`: `7L`) Minimum number of observations in terminal nodes.
		#' @param use_cache (`logical(1)`: `TRUE`) Whether to cache fitted trees.
		initialize = function(
			task,
			mincriterion = 0.95,
			minsplit = 20L,
			minbucket = 7L,
			use_cache = TRUE
		) {
			require_package("partykit")
			super$initialize(task)

			# Initialize tree cache
			self$tree_cache = new.env(parent = emptyenv())

			# Define param_set with ctree-specific parameters
			self$param_set = paradox::ps(
				conditioning_set = paradox::p_uty(default = NULL),
				mincriterion = paradox::p_dbl(lower = 0, upper = 1, default = 0.95),
				minsplit = paradox::p_int(lower = 1L, default = 20L),
				minbucket = paradox::p_int(lower = 1L, default = 7L),
				use_cache = paradox::p_lgl(default = TRUE)
			)

			self$param_set$set_values(
				mincriterion = mincriterion,
				minsplit = minsplit,
				minbucket = minbucket,
				use_cache = use_cache
			)

			self$label = "Conditional Inference Tree Conditional Sampler"
		}
	),

	private = list(
		# Core ctree sampling logic implementing conditional inference tree sampling
		.sample_conditional = function(data, feature, conditioning_set, ...) {
			# Get training data from task
			training_data = self$task$data(cols = self$task$feature_names)

			# Handle marginal case (no conditioning)
			if (is.null(conditioning_set) || length(conditioning_set) == 0) {
				# Simple random sampling from training data
				for (feat in feature) {
					data[, (feat) := sample(training_data[[feat]], .N, replace = TRUE)]
				}
				return(data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)])
			}

			# Get or build tree for this feature/conditioning set combination
			tree = private$.get_or_build_tree(feature, conditioning_set)

			# Add temporary ID to track original rows
			data[, ..eval_id := seq_len(.N)]

			# Predict terminal node for each observation
			# Use type = "node" to get node IDs
			node_ids = predict(tree, newdata = data, type = "node")
			data[, ..node_id := node_ids]

			# Get terminal node memberships for training data
			train_nodes = predict(tree, newdata = training_data, type = "node")
			train_data_with_nodes = copy(training_data)
			train_data_with_nodes[, ..node_id := train_nodes]

			# For each evaluation observation, sample from training observations in same node
			result = data[, {
				# Get training observations in this node
				node_obs = train_data_with_nodes[..node_id == ..node_id[1]]

				if (nrow(node_obs) == 0) {
					cli::cli_warn("No training observations in terminal node, using marginal sample")
					sampled_values = lapply(feature, function(feat) {
						sample(training_data[[feat]], 1)
					})
				} else {
					# Sample one observation from this node
					sampled_idx = sample(seq_len(nrow(node_obs)), 1)
					sampled_values = lapply(feature, function(feat) {
						node_obs[[feat]][sampled_idx]
					})
				}

				# Create result row
				result_row = .SD[1]
				for (i in seq_along(feature)) {
					set(result_row, j = feature[i], value = sampled_values[[i]])
				}
				result_row
			}, by = ..eval_id]

			# Clean up temporary columns
			result[, ..eval_id := NULL]
			result[, ..node_id := NULL]

			result[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		},

		# Get cached tree or build a new one
		.get_or_build_tree = function(feature, conditioning_set) {
			# Create cache key from feature names and conditioning set
			cache_key = paste(
				paste(sort(feature), collapse = "+"),
				"given",
				paste(sort(conditioning_set), collapse = "+")
			)

			use_cache = self$param_set$values$use_cache

			# Check cache
			if (use_cache && exists(cache_key, envir = self$tree_cache)) {
				return(get(cache_key, envir = self$tree_cache))
			}

			# Build new tree
			tree = private$.build_tree(feature, conditioning_set)

			# Cache if enabled
			if (use_cache) {
				assign(cache_key, tree, envir = self$tree_cache)
			}

			tree
		},

		# Build a ctree model
		.build_tree = function(feature, conditioning_set) {
			# Get training data from task
			training_data = self$task$data(cols = self$task$feature_names)

			# Create formula: features ~ conditioning_set
			# For multiple features, use cbind on LHS
			if (length(feature) == 1) {
				formula_str = sprintf("`%s` ~ %s",
					feature,
					paste(sprintf("`%s`", conditioning_set), collapse = " + ")
				)
			} else {
				formula_str = sprintf("cbind(%s) ~ %s",
					paste(sprintf("`%s`", feature), collapse = ", "),
					paste(sprintf("`%s`", conditioning_set), collapse = " + ")
				)
			}

			formula = as.formula(formula_str)

			# Extract control parameters
			mincriterion = self$param_set$values$mincriterion
			minsplit = self$param_set$values$minsplit
			minbucket = self$param_set$values$minbucket

			# Build tree with partykit::ctree
			tree = partykit::ctree(
				formula = formula,
				data = training_data,
				control = partykit::ctree_control(
					mincriterion = mincriterion,
					minsplit = minsplit,
					minbucket = minbucket
				)
			)

			tree
		}
	)
)
