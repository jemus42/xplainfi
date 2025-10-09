#' Scoring utility for computing importances
#'
#' Computes the `relation` of score before a change (e.g. PFI, LOCO, ...) and after.
#' If `minimize == TRUE`, then `scores_post - scores_pre` is computed for
#' `relation == "difference"`, otherwise `scores_pre - scores_post` is given.
#' If `minimize == FALSE`, then the order is flipped, insuring that "higher value" means "more important".
#' @param scores_pre,scores_post (`numeric()`) Vector of scores or loss values at baseline / before (`_pre`) a modification, and after (`_post`) a modification (e.g., permutation or refit).
#' @param relation (`character(1)`: `"difference"`) Calculate the difference or `"ratio"` between pre and post modification value.
#' @param minimize (`logical(1)`: `TRUE`) Flips the order of the comparison, typically taken from a [mlr3::Measure]'s `$minimize` field.
compute_score = function(
	scores_pre,
	scores_post,
	relation = c("difference", "ratio"),
	minimize = TRUE
) {
	checkmate::assert_numeric(scores_pre, any.missing = FALSE)
	checkmate::assert_numeric(scores_post, any.missing = FALSE)
	checkmate::assert_true(length(scores_pre) == length(scores_post))
	checkmate::assert_flag(minimize)
	relation = match.arg(relation)

	# I know this could be more concise but for the time I prefer it to be very obvious in what happens when
	# General expectation -> higher score => more important
	if (minimize) {
		# Lower is better, e.g. ce, where scores_pre is expected to be smaller and scores_post larger
		switch(relation, difference = scores_post - scores_pre, ratio = scores_post / scores_pre)
	} else {
		# Higher is better, e.g. accuracy, where scores_pre is expected to be larger and scores_post smaller
		switch(relation, difference = scores_pre - scores_post, ratio = scores_pre / scores_post)
	}
}
