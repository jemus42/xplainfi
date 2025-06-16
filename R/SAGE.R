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
  inherit = FeatureImportanceMethod,
  public = list(
    #' @field n_permutations (integer(1)) Number of permutations to sample.
    n_permutations = NULL,
    #' @field reference_data (data.table) Reference dataset for marginalization.
    reference_data = NULL,
    #' @field sampler ([FeatureSampler]) Sampler object for marginalization.
    sampler = NULL,

    #' @description
    #' Creates a new instance of the SAGE class.
    #' @param task,learner,measure,resampling,features Passed to FeatureImportanceMethod.
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

      # For classification tasks, require predict_type = "prob"
      if (self$task$task_type == "classif") {
        if (learner$predict_type != "prob") {
          cli::cli_abort(c(
            "Classification learners must use predict_type = 'prob' for SAGE.",
            "i" = "Please set learner$predict_type = 'prob' before using SAGE."
          ))
        }
      }

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
      scores_agg = private$.aggregate_importances(scores)

      # Store results
      self$resample_result = rr
      self$scores = scores
      self$importance = scores_agg

      copy(self$importance)
    }
  ),

  private = list(
    .compute_sage_scores = function(learner, test_dt) {
      # Initialize SAGE values for each feature
      sage_values = numeric(length(self$features))
      names(sage_values) = self$features

      # Pre-generate all permutations for efficiency
      all_permutations = replicate(self$n_permutations, sample(self$features), simplify = FALSE)

      # Pre-allocate lists for coalitions
      # Total coalitions = 1 (empty) + n_permutations * n_features
      n_total_coalitions = 1 + self$n_permutations * length(self$features)
      all_coalitions = vector("list", n_total_coalitions)
      coalition_map = vector("list", n_total_coalitions)

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
        importance = unname(sage_values)
      )
    },

    .evaluate_coalitions_batch = function(learner, test_dt, all_coalitions) {
      # Batch evaluate multiple coalitions by creating one large dataset
      n_coalitions = length(all_coalitions)
      n_test = nrow(test_dt)
      n_reference = nrow(self$reference_data)

      # Pre-allocate list for expanded data
      all_expanded_data = vector("list", n_coalitions)
      
      # Pre-calculate total rows for coalition_ids
      total_rows = n_coalitions * n_test * n_reference
      coalition_ids = numeric(total_rows)
      row_offset = 0

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
        
        # Fill pre-allocated coalition_ids
        n_rows = nrow(test_expanded)
        coalition_ids[(row_offset + 1):(row_offset + n_rows)] = i
        row_offset = row_offset + n_rows
      }

      # Combine ALL data into one big dataset
      combined_data = rbindlist(all_expanded_data)

      # SINGLE prediction call for everything!
      pred_result = learner$predict_newdata(newdata = combined_data, task = self$task)

      # Extract predictions
      if (self$task$task_type == "classif") {
        # We now enforce predict_type = "prob" for classification
        # For classification, we need to store the full probability matrix
        all_predictions = pred_result$prob
        # Handle any NAs in probability predictions by replacing with uniform probabilities
        if (any(is.na(all_predictions))) {
          n_classes = ncol(all_predictions)
          uniform_prob = 1 / n_classes
          for (j in seq_len(n_classes)) {
            all_predictions[is.na(all_predictions[, j]), j] = uniform_prob
          }
        }
      } else {
        # For regression
        all_predictions = pred_result$response
        # Handle NAs in regression predictions
        all_predictions[is.na(all_predictions)] = 0
      }

      # Add predictions and aggregate by coalition and test instance
      if (self$task$task_type == "classif") {
        # For classification, we need to handle matrix predictions differently
        # Add each class probability as a separate column
        n_classes = ncol(all_predictions)
        class_names = colnames(all_predictions)

        for (j in seq_len(n_classes)) {
          combined_data[, paste0(".pred_class_", j) := all_predictions[, j]]
        }

        # Aggregate: mean probability per class per test instance per coalition
        agg_cols = paste0(".pred_class_", seq_len(n_classes))
        avg_preds_by_coalition = combined_data[,
          lapply(.SD, function(x) mean(x, na.rm = TRUE)),
          .SDcols = agg_cols,
          by = .(.coalition_id, .test_instance_id)
        ]

        # Rename columns to class names
        setnames(avg_preds_by_coalition, agg_cols, class_names)
      } else {
        # For regression, keep the simple approach
        combined_data[, .prediction := all_predictions]

        # Aggregate: mean prediction per test instance per coalition
        avg_preds_by_coalition = combined_data[,
          .(
            avg_pred = mean(.prediction, na.rm = TRUE)
          ),
          by = .(.coalition_id, .test_instance_id)
        ]
      }

      # Calculate loss for each coalition
      coalition_losses = numeric(n_coalitions)
      for (i in seq_len(n_coalitions)) {
        coalition_data = avg_preds_by_coalition[.coalition_id == i]

        # Create prediction object and calculate loss
        if (self$task$task_type == "classif") {
          # For classification, extract the probability matrix
          class_names = self$task$class_names
          prob_matrix = as.matrix(coalition_data[, .SD, .SDcols = class_names])

          pred_obj = PredictionClassif$new(
            row_ids = seq_len(n_test),
            truth = test_dt[[self$task$target_names]],
            prob = prob_matrix
          )
        } else {
          pred_obj = PredictionRegr$new(
            row_ids = seq_len(n_test),
            truth = test_dt[[self$task$target_names]],
            response = coalition_data$avg_pred
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
