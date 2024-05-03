#' Scoring utility
#'
#' Computes the relation of score before a change (e.g. permutation, LOCO, ...) and after.
#'
#' If `minimize == TRUE`, then `scores_post - scores_pre` is computed for `relation == "difference"`,
#' otherwise `scores_pre - scores_post` is given.
#'
#' @param scores_pre (numeric()) Score before change.
#' @param scores_post (numeric()) Score after change.
#' @param relation (character(1), `"difference"`) Either `"difference"` or `"ratio"`.
#' @param minimize (logical(1), `TRUE`) Whether the score needs to be minimized (e.g. RMSE) or
#' maximized (e.g. AUC).
#'
#' @return A numeric vector of the same length as `scores_pre` and `scores_post`
#' @export
#'
#' @examples
#'
#' pre = rnorm(10)
#' post = pre + runif(10)
#'
#' compute_score_relation(pre, post)
#' compute_score_relation(pre, post, "ratio")
#' compute_score_relation(pre, post, minimize = FALSE)
compute_score_relation = function(scores_pre, scores_post, relation = "difference", minimize = TRUE) {
  checkmate::assert_numeric(scores_pre)
  checkmate::assert_numeric(scores_post)
  checkmate::assert_true(length(scores_pre) == length(scores_post))
  checkmate::assert_flag(minimize)
  checkmate::assert_subset(relation, c("difference", "ratio"))

  if (minimize) {
    # Lower is better, e.g. ce
    switch(relation,
           difference = scores_post - scores_pre,
           ratio = scores_post / scores_pre
    )
  } else {
    # Higher is better, e.g. accuracy
    switch(relation,
           difference = scores_pre - scores_post,
           ratio = scores_pre / scores_post
    )
  }
}
