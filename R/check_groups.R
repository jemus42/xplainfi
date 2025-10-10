#' Check group specification
#' @param groups (`list`) A (named) list of groups
#' @param task ([Task][mlr3::Task]) Used to look up valid features.
#'
#' @return `group`, with each element now named.
#' @examples
#' task <- sim_dgp_interactions(n = 100)
#' task$feature_names
#'
#' # Intended use
#' groups1 = list(effects = c("x1", "x2", "x3"), noise = c("noise1", "noise2"))
#' check_groups(groups1, task)
#'
#' # Names are auto-generated where needed
#' check_groups(list(a = "x1",  c("x2", "x1")), task)
#'
#' \dontrun{
#' # Unexpected features
#' groups2 = list(effects = c("x1", "foo", "bar", "x1"))
#' check_groups(groupos1, task)
#' # Too deeply nested
#' groups3 = list(effects = c("x1", "x2", "x3"), noise = c("noise1", list(c("noise2"))))
#' check_groups(groupos1, task)
#' }
check_groups = function(groups, task) {
	# Unlist non-recursively so we can fail if the result is not a vector
	group_features = unlist(groups, use.names = FALSE, recursive = FALSE)

	if (is.list(group_features)) {
		cli::cli_abort(c(
			x = "Group{?s} is nested too deeply: {.val {names(which(sapply(groups, is.list)))}}",
			i = "Ensure each group consists of a vector of features, not a list."
		))
	}

	# TODO: Figure out whether it's an error or just weird if groups have overlapping features (interpreation changes at least)
	if (anyDuplicated(group_features) > 0) {
		cli::cli_alert_warning(
			"Feature{?s} is specified in multiple groups: {.val {group_features[anyDuplicated(group_features)]}}"
		)
	}

	extra_feature = setdiff(group_features, task$feature_names)
	if (length(extra_feature) > 0) {
		cli::cli_abort(
			"Feature{?s} specified in {.code groups} not in provided {.cls Task}: {.val {extra_feature}}"
		)
	}

	# Check all are named
	nm = names(groups)
	unnamed = which(nm == "")
	if (length(unnamed) > 0) {
		cli::cli_inform(c(
			"Not all groups are named",
			i = "Group{?s} {.val {as.character(unnamed)}} {?is/are} named automatically"
		))
		# Override only names where there are non and use k-th letter of alphabet corresponding
		# to position of groups with missing name
		names(groups)[unnamed] = paste0(c("Group", LETTERS[unnamed]), collapse = "")
	}

	groups
}
