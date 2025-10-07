#' @title Create Feature Selection Design Matrix
#'
#' @description
#' Creates a logical design matrix for leave-in or leave-out feature evaluation.
#' Used internally with mlr3fselect to evaluate feature subsets.
#'
#' @param all_features (character()) All available feature names from the task.
#' @param feature_names (character() | list of character()) Features or feature
#'   groups to evaluate. Can be a vector for individual features or a named list
#'   for grouped features. Defaults to `all_features` if unspecified.
#' @param direction (character(1)) Either `"leave-in"` or `"leave-out"` (default).
#'   Controls which features are selected in the design matrix.
#'   `"leave-out"` sets features of interest to `FALSE`, and `"leave-in"` analogously
#'   sets them to `TRUE`.
#'
#' @return data.table with logical columns for each feature in `all_features` and
#'   `length(feature_names)` rows, one for each entry in `feature_names`
#'
#' @examples
#' task = mlr3::tsk("mtcars")
#'
#' # Individual features
#' feature_names = task$feature_names[1:3]
#' wvim_design_matrix(task$feature_names, feature_names, "leave-in")
#' wvim_design_matrix(task$feature_names, feature_names, "leave-out")
#'
#' # Feature groups
#' feature_groups = list(
#'   A = task$feature_names[1:2],
#'   B = task$feature_names[3:5]
#' )
#' wvim_design_matrix(task$feature_names, feature_groups, "leave-out")
#'
#' @export
wvim_design_matrix = function(
	all_features,
	feature_names = all_features,
	direction = c("leave-out", "leave-in")
) {
	if (is.list(feature_names)) {
		lapply(feature_names, \(x) checkmate::assert_subset(x, all_features))
	} else {
		checkmate::assert_subset(feature_names, all_features)
	}

	direction = match.arg(direction)
	# Initialize all-FALSE matrix
	# one column for each feature in original task (must match)
	# one row for feature or group of features of interest (vector or list)
	design_mat = matrix(
		FALSE,
		nrow = length(feature_names),
		ncol = length(all_features),
		dimnames = list(NULL, all_features)
	)

	# feature_names can be vector or list, e.g.
	# feature_names = c("x1", "x2")
	# feature_names = list(A = c("x1", "x2"), B = c("x3"))
	for (feature_vec_idx in seq_along(feature_names)) {
		# Get matching index for original task feature names to ensure correct order
		# user might supply feature_names with arbitrary order
		fidx = match(feature_names[[feature_vec_idx]], all_features)
		# Flip value for corresponding feature index
		design_mat[feature_vec_idx, fidx] = TRUE
	}

	# When leaving features out we flip the logic
	# -> FALSE features are left out, TRUE are left in
	if (direction == "leave-out") {
		design_mat = !design_mat
	}

	design_dt = as.data.table(design_mat)

	stopifnot(nrow(design_dt) == length(feature_names))
	stopifnot(ncol(design_dt) == length(all_features))
	design_dt
}
