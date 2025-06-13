#' @title Shapley Additive Global Importance (SAGE) Base Class
#'
#' @description Base class for SAGE (Shapley Additive Global Importance)
#' feature importance based on Shapley values with marginalization.
#' This is an abstract class - use MarginalSAGE or ConditionalSAGE.
#'
#' @details
#' SAGE uses Shapley values to fairly distribute the total prediction
#' performance among all features. Unlike perturbation-based methods,
#' SAGE marginalizes features by integrating over their distribution.
#' This is approximated by averaging predictions over a reference dataset.
#'
#' @references
#' Covert, I., Lundberg, S. M., & Lee, S. I. (2020).
#' Understanding global feature contributions through game-theoretic interpretations
#' of black-box models. arXiv preprint arXiv:2010.12012.
#'
#' @export
SAGE = R6Class(
  "SAGE",
  inherit = FeatureImportanceMeasure,
  public = list(
    #' @field n_permutations (integer(1)) Number of permutations to sample.
    n_permutations = NULL,
    #' @field reference_data (data.table) Reference dataset for marginalization.
    reference_data = NULL,
    #' @field sampler ([FeatureSampler]) Sampler object for marginalization.
    sampler = NULL,

    #' @description
    #' Creates a new instance of the SAGE class.
    #' @param task,learner,measure,resampling,features Passed to FeatureImportanceMeasure.
    #' @param n_permutations (integer(1)) Number of permutations to sample for Shapley value estimation.
    #' @param reference_data (data.table) Optional reference dataset. If NULL, uses training data.
    #' @param sampler ([FeatureSampler]) Sampler for marginalization.
    #' @param max_reference_size (integer(1)) Maximum size of reference dataset. If reference is larger, it will be subsampled.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      sampler = NULL,
      max_reference_size = 100L
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        label = "Shapley Additive Global Importance"
      )

      self$n_permutations = checkmate::assert_int(n_permutations, lower = 1L)

      # Store sampler
      if (!is.null(sampler)) {
        self$sampler = sampler
      }

      # Use training data as reference if not provided
      if (is.null(reference_data)) {
        self$reference_data = self$task$data(cols = self$task$feature_names)
      } else {
        self$reference_data = checkmate::assert_data_table(reference_data)
      }

      # Subsample reference data if it's too large for efficiency
      if (nrow(self$reference_data) > max_reference_size) {
        sample_idx = sample(nrow(self$reference_data), size = max_reference_size)
        self$reference_data = self$reference_data[sample_idx, ]
      }

      # Set parameters
      ps = ps(
        n_permutations = paradox::p_int(lower = 1L, default = 10L)
      )
      ps$values$n_permutations = n_permutations
      self$param_set = ps
    },

    #' @description
    #' Compute SAGE values.
    #' @param store_backends (logical(1)) Whether to store backends.
    compute = function(store_backends = TRUE) {
      # Check if already computed
      if (!is.null(self$importance)) {
        return(self$importance)
      }

      # Initial resampling to get trained models
      rr = resample(
        self$task,
        self$learner,
        self$resampling,
        store_models = TRUE,
        store_backends = store_backends
      )

      # Compute SAGE values for each resampling iteration
      sage_scores = lapply(seq_len(self$resampling$iters), \(iter) {
        private$.compute_sage_scores(
          learner = rr$learners[[iter]],
          test_dt = self$task$data(rows = rr$resampling$test_set(iter))
        )
      })

      # Combine results across resampling iterations
      scores = rbindlist(sage_scores, idcol = "iter_rsmp")

      # Aggregate by feature
      scores_agg = scores[, list(importance = mean(sage_value)), by = feature]

      # Store results
      self$resample_result = rr
      self$scores = scores
      self$importance = scores_agg

      return(self$importance)
    }
  ),

  private = list(
    .compute_sage_scores = function(learner, test_dt) {
      # Initialize SAGE values for each feature
      sage_values = numeric(length(self$features))
      names(sage_values) = self$features

      # Pre-generate all permutations for efficiency
      all_permutations = replicate(self$n_permutations, sample(self$features), simplify = FALSE)

      # Collect ALL coalitions that need to be evaluated across all permutations
      all_coalitions = list()
      coalition_map = list() # Maps coalition index to (perm_idx, step)

      # Add empty coalition
      all_coalitions[[1]] = character(0)
      coalition_map[[1]] = list(perm_idx = 0, step = 0) # Special case for baseline

      coalition_idx = 2
      for (perm_idx in seq_len(self$n_permutations)) {
        perm_features = all_permutations[[perm_idx]]

        for (i in seq_along(perm_features)) {
          coalition = perm_features[seq_len(i)]
          all_coalitions[[coalition_idx]] = coalition
          coalition_map[[coalition_idx]] = list(perm_idx = perm_idx, step = i)
          coalition_idx = coalition_idx + 1
        }
      }

      # Batch evaluate ALL coalitions at once
      all_losses = private$.evaluate_coalitions_batch(learner, test_dt, all_coalitions)

      # Extract baseline loss
      baseline_loss = all_losses[1]

      # Process permutations using pre-computed losses
      for (perm_idx in seq_len(self$n_permutations)) {
        perm_features = all_permutations[[perm_idx]]
        prev_loss = baseline_loss

        for (i in seq_along(perm_features)) {
          feature = perm_features[i]

          # Find the loss for this coalition
          coalition_lookup_idx = which(sapply(coalition_map, function(x) {
            x$perm_idx == perm_idx && x$step == i
          }))
          current_loss = all_losses[coalition_lookup_idx]

          # Calculate marginal contribution
          marginal_contribution = prev_loss - current_loss
          sage_values[feature] = sage_values[feature] + marginal_contribution

          prev_loss = current_loss
        }
      }

      # Average over permutations
      sage_values = sage_values / self$n_permutations

      # Return as data.table
      data.table(
        feature = names(sage_values),
        sage_value = unname(sage_values)
      )
    },

    .evaluate_empty_coalition = function(learner, test_dt) {
      # Marginalize all features by averaging predictions over reference data
      predictions = private$.marginalize_features(
        learner = learner,
        test_dt = test_dt,
        coalition = character(0) # Empty coalition
      )

      # Calculate loss
      if (self$task$task_type == "classif") {
        pred = PredictionClassif$new(
          row_ids = seq_len(nrow(test_dt)),
          truth = test_dt[[self$task$target_names]],
          prob = predictions
        )
      } else {
        pred = PredictionRegr$new(
          row_ids = seq_len(nrow(test_dt)),
          truth = test_dt[[self$task$target_names]],
          response = predictions
        )
      }

      # Return loss
      pred$score(self$measure)
    },

    .evaluate_coalition = function(learner, test_dt, coalition) {
      # Marginalize features not in coalition
      predictions = private$.marginalize_features(
        learner = learner,
        test_dt = test_dt,
        coalition = coalition
      )

      # Calculate loss
      if (self$task$task_type == "classif") {
        pred = PredictionClassif$new(
          row_ids = seq_len(nrow(test_dt)),
          truth = test_dt[[self$task$target_names]],
          prob = predictions
        )
      } else {
        pred = PredictionRegr$new(
          row_ids = seq_len(nrow(test_dt)),
          truth = test_dt[[self$task$target_names]],
          response = predictions
        )
      }

      # Return loss
      pred$score(self$measure)
    },

    .marginalize_features = function(learner, test_dt, coalition) {
      # Features to marginalize (not in coalition)
      marginalize_features = setdiff(self$features, coalition)

      # If no features to marginalize, just predict normally
      if (length(marginalize_features) == 0) {
        pred_result = learner$predict_newdata(newdata = test_dt, task = self$task)
        if (self$task$task_type == "classif" && learner$predict_type == "prob") {
          # For binary classification, return probability of positive class
          if (ncol(pred_result$prob) == 2) {
            return(pred_result$prob[, self$task$positive])
          } else {
            # For multiclass, this would need different handling
            stop("Multiclass classification not yet fully supported in SAGE")
          }
        } else {
          return(pred_result$response)
        }
      }

      # SAGE marginalization according to Ewald et al. paper (vectorized):
      # Marginalize by expectation over reference distribution
      # For marginal SAGE: E_{X_{-S}}[f(x_S, X_{-S})]
      # This approximates the expectation by averaging over reference data

      n_test = nrow(test_dt)
      n_reference = nrow(self$reference_data)

      # Vectorized approach: create all test-reference combinations at once
      # Each test instance gets paired with each reference instance
      test_expanded = test_dt[rep(seq_len(n_test), each = n_reference)]
      reference_expanded = self$reference_data[rep(seq_len(n_reference), times = n_test)]

      # Add test instance ID for later aggregation
      test_expanded[, .test_instance_id := rep(seq_len(n_test), each = n_reference)]

      # Replace marginalized features with reference values (vectorized)
      if (length(marginalize_features) > 0) {
        test_expanded[,
          (marginalize_features) := reference_expanded[, .SD, .SDcols = marginalize_features]
        ]
      }

      # Single prediction call for all combinations (much faster!)
      pred_result = learner$predict_newdata(newdata = test_expanded, task = self$task)

      # Extract predictions
      if (self$task$task_type == "classif" && learner$predict_type == "prob") {
        # For binary classification
        if (ncol(pred_result$prob) == 2) {
          all_predictions = pred_result$prob[, self$task$positive]
        } else {
          stop("Multiclass classification not yet fully supported in SAGE")
        }
      } else {
        all_predictions = pred_result$response
      }

      # Add predictions to the expanded data and aggregate by test instance
      test_expanded[, .prediction := all_predictions]
      avg_predictions_dt = test_expanded[, .(avg_pred = mean(.prediction)), by = .test_instance_id]

      # Return in original test instance order
      return(avg_predictions_dt$avg_pred)
    },

    .evaluate_coalitions_batch = function(learner, test_dt, all_coalitions) {
      # Batch evaluate multiple coalitions by creating one large dataset
      n_coalitions = length(all_coalitions)
      n_test = nrow(test_dt)
      n_reference = nrow(self$reference_data)

      # Create expanded data for all coalitions at once
      # Each coalition gets test_dt replicated n_reference times
      all_expanded_data = list()
      coalition_ids = numeric()

      for (i in seq_along(all_coalitions)) {
        coalition = all_coalitions[[i]]

        # Create test-reference combinations for this coalition
        test_expanded = test_dt[rep(seq_len(n_test), each = n_reference)]
        reference_expanded = self$reference_data[rep(seq_len(n_reference), times = n_test)]

        # Add coalition and test instance IDs for tracking
        test_expanded[, .coalition_id := i]
        test_expanded[, .test_instance_id := rep(seq_len(n_test), each = n_reference)]

        # Marginalize features not in coalition
        marginalize_features = setdiff(self$features, coalition)
        if (length(marginalize_features) > 0) {
          test_expanded[,
            (marginalize_features) := reference_expanded[, .SD, .SDcols = marginalize_features]
          ]
        }

        all_expanded_data[[i]] = test_expanded
        coalition_ids = c(coalition_ids, rep(i, nrow(test_expanded)))
      }

      # Combine ALL data into one big dataset
      combined_data = rbindlist(all_expanded_data)

      # SINGLE prediction call for everything!
      pred_result = learner$predict_newdata(newdata = combined_data, task = self$task)

      # Extract predictions
      if (self$task$task_type == "classif") {
        if (learner$predict_type == "prob") {
          if (ncol(pred_result$prob) == 2) {
            all_predictions = pred_result$prob[, self$task$positive]
            # Handle any NAs in probability predictions
            all_predictions[is.na(all_predictions)] = 0.5
          } else {
            stop("Multiclass classification not yet fully supported in SAGE")
          }
        } else {
          # For classification with response predictions
          if (length(self$task$class_names) == 2) {
            # Binary classification - convert to 0/1
            all_predictions = as.numeric(pred_result$response == self$task$positive)
            # Replace NAs with 0.5 (neutral probability for classification)
            all_predictions[is.na(all_predictions)] = 0.5
          } else {
            # Multiclass classification - not yet fully supported, use neutral score
            stop(
              "Multiclass classification with response predictions not yet supported in SAGE. Use predict_type = 'prob' instead."
            )
          }
        }
      } else {
        # For regression
        all_predictions = pred_result$response
        # Handle NAs in regression predictions
        all_predictions[is.na(all_predictions)] = 0
      }

      # Add predictions and aggregate by coalition and test instance
      combined_data[, .prediction := all_predictions]

      # Aggregate: mean prediction per test instance per coalition
      # Now all predictions are numeric (probabilities or 0/1 for classification, values for regression)
      # Use na.rm = TRUE to handle any remaining NAs in the aggregation
      avg_preds_by_coalition = combined_data[,
        .(
          avg_pred = mean(.prediction, na.rm = TRUE)
        ),
        by = .(.coalition_id, .test_instance_id)
      ]

      # Calculate loss for each coalition
      coalition_losses = numeric(n_coalitions)
      for (i in seq_len(n_coalitions)) {
        coalition_preds = avg_preds_by_coalition[.coalition_id == i]$avg_pred

        # Create prediction object and calculate loss
        if (self$task$task_type == "classif") {
          if (learner$predict_type == "prob") {
            # coalition_preds contains probabilities for positive class
            pred_obj = PredictionClassif$new(
              row_ids = seq_len(n_test),
              truth = test_dt[[self$task$target_names]],
              prob = matrix(
                c(1 - coalition_preds, coalition_preds),
                ncol = 2,
                dimnames = list(NULL, self$task$class_names)
              )
            )
          } else {
            # This should only happen for binary classification with response predictions
            if (length(self$task$class_names) == 2) {
              # coalition_preds contains averaged 0/1 values, convert back to factor responses
              # Handle NAs by replacing them with 0.5 (neutral probability)
              coalition_preds[is.na(coalition_preds)] = 0.5

              # Additional safeguard: ensure coalition_preds is finite
              coalition_preds[!is.finite(coalition_preds)] = 0.5

              pred_responses = factor(
                ifelse(coalition_preds >= 0.5, self$task$positive, self$task$negative),
                levels = self$task$class_names
              )

              pred_obj = PredictionClassif$new(
                row_ids = seq_len(n_test),
                truth = test_dt[[self$task$target_names]],
                response = pred_responses
              )
            } else {
              stop("Multiclass classification with response predictions not supported")
            }
          }
        } else {
          pred_obj = PredictionRegr$new(
            row_ids = seq_len(n_test),
            truth = test_dt[[self$task$target_names]],
            response = coalition_preds
          )
        }

        coalition_losses[i] = pred_obj$score(self$measure)
      }

      return(coalition_losses)
    }
  )
)

#' @title Marginal SAGE
#'
#' @description SAGE with marginal sampling (features are marginalized independently).
#' This is the standard SAGE implementation.
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sage = MarginalSAGE$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   n_permutations = 3L
#' )
#' sage$compute()
#' @export
MarginalSAGE = R6Class(
  "MarginalSAGE",
  inherit = SAGE,
  public = list(
    #' @description
    #' Creates a new instance of the MarginalSAGE class.
    #' @param task,learner,measure,resampling,features Passed to [SAGE].
    #' @param n_permutations (integer(1)) Number of permutations to sample.
    #' @param reference_data (data.table) Optional reference dataset.
    #' @param max_reference_size (integer(1)) Maximum size of reference dataset.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      max_reference_size = 100L
    ) {
      # Create marginal sampler
      sampler = MarginalSampler$new(task)

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        n_permutations = n_permutations,
        reference_data = reference_data,
        sampler = sampler,
        max_reference_size = max_reference_size
      )

      self$label = "Marginal SAGE"
    }
  )
)

#' @title Conditional SAGE
#'
#' @description SAGE with conditional sampling (features are marginalized conditionally).
#' Uses ARF by default for conditional marginalization.
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sage = ConditionalSAGE$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   n_permutations = 3L
#' )
#' sage$compute()
#' @export
ConditionalSAGE = R6Class(
  "ConditionalSAGE",
  inherit = SAGE,
  public = list(
    #' @description
    #' Creates a new instance of the ConditionalSAGE class.
    #' @param task,learner,measure,resampling,features Passed to [SAGE].
    #' @param n_permutations (integer(1)) Number of permutations to sample.
    #' @param reference_data (data.table) Optional reference dataset.
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to ARFSampler.
    #' @param max_reference_size (integer(1)) Maximum size of reference dataset.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      sampler = NULL,
      max_reference_size = 100L
    ) {
      # Use ARFSampler by default
      if (is.null(sampler)) {
        sampler = ARFSampler$new(task)
      }

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        n_permutations = n_permutations,
        reference_data = reference_data,
        sampler = sampler,
        max_reference_size = max_reference_size
      )

      self$label = "Conditional SAGE"
    }
  )
)
