#' Batch Predict for SAGE
#'
#' Performs batched prediction on combined data to manage memory usage.
#' Supports both classification (probability predictions) and regression.
#'
#' @param learner Trained mlr3 learner
#' @param combined_data data.table with feature columns
#' @param task mlr3 Task object
#' @param batch_size Optional integer for batch size. If NULL or if total_rows <= batch_size,
#'   processes all data at once.
#' @param task_type Character, either "classif" or "regr"
#'
#' @return For classification: matrix of class probabilities (n_rows x n_classes).
#'   For regression: numeric vector of predictions (length n_rows).
#'
#' @keywords internal
sage_batch_predict = function(learner, combined_data, task, batch_size, task_type) {
	total_rows = nrow(combined_data)

	if (!is.null(batch_size) && total_rows > batch_size) {
		# Batched prediction
		n_batches = ceiling(total_rows / batch_size)
		all_predictions = vector("list", n_batches)

		for (batch_idx in seq_len(n_batches)) {
			start_row = (batch_idx - 1) * batch_size + 1
			end_row = min(batch_idx * batch_size, total_rows)
			batch_data = combined_data[start_row:end_row]

			if (xplain_opt("debug")) {
				cli::cli_inform(
					"Predicting on {.val {nrow(batch_data)}} instances in batch {.val {batch_idx}/{n_batches}}"
				)
			}

			pred_result = if (is.function(learner$predict_newdata_fast)) {
				learner$predict_newdata_fast(newdata = batch_data, task = task)
			} else {
				learner$predict_newdata(newdata = batch_data, task = task)
			}

			all_predictions[[batch_idx]] = if (task_type == "classif") {
				pred_result$prob
			} else {
				pred_result$response
			}
		}

		# Combine predictions from all batches
		if (task_type == "classif") {
			do.call(rbind, all_predictions)
		} else {
			do.call(c, all_predictions)
		}
	} else {
		# Single prediction without batching
		if (xplain_opt("debug")) {
			cli::cli_inform("Predicting on {.val {nrow(combined_data)}} instances at once")
		}

		pred_result = if (is.function(learner$predict_newdata_fast)) {
			learner$predict_newdata_fast(newdata = combined_data, task = task)
		} else {
			learner$predict_newdata(newdata = combined_data, task = task)
		}

		if (task_type == "classif") {
			pred_result$prob
		} else {
			pred_result$response
		}
	}
}

#' Handle NA Predictions for SAGE
#'
#' Replaces NA values in predictions with sensible defaults.
#' For classification: uniform probability across classes.
#' For regression: zero.
#'
#' @param predictions For classification: matrix of probabilities. For regression: numeric vector.
#' @param task_type Character, either "classif" or "regr"
#'
#' @return Predictions with NAs replaced
#'
#' @keywords internal
sage_handle_na_predictions = function(predictions, task_type) {
	if (task_type == "classif") {
		if (any(is.na(predictions))) {
			cli::cli_warn("Encountered missing values in model prediction")
			n_classes = ncol(predictions)
			uniform_prob = 1 / n_classes

			for (j in seq_len(n_classes)) {
				predictions[is.na(predictions[, j]), j] = uniform_prob
			}
		}
	} else {
		if (any(is.na(predictions))) {
			cli::cli_warn("Encountered missing values in model prediction")
			predictions[is.na(predictions)] = 0
		}
	}

	predictions
}

#' Aggregate Predictions by Coalition and Test Instance
#'
#' Averages predictions across multiple samples (reference data or conditional samples)
#' for each unique combination of coalition and test instance.
#'
#' @param combined_data data.table with columns .coalition_id, .test_instance_id, and features
#' @param predictions For classification: matrix of probabilities. For regression: numeric vector.
#' @param task_type Character, either "classif" or "regr"
#' @param class_names Character vector of class names (required for classification, ignored for regression)
#'
#' @return data.table with columns:
#'   - .coalition_id: Coalition identifier
#'   - .test_instance_id: Test instance identifier
#'   - For classification: One column per class with averaged probabilities
#'   - For regression: avg_pred column with averaged predictions
#'
#' @keywords internal
sage_aggregate_predictions = function(combined_data, predictions, task_type, class_names = NULL) {
	if (task_type == "classif") {
		# Add prediction columns to combined_data
		n_classes = ncol(predictions)
		for (j in seq_len(n_classes)) {
			combined_data[, paste0(".pred_class_", j) := predictions[, j]]
		}

		# Aggregate: calculate mean probability for each class, grouped by coalition and test instance
		agg_cols = paste0(".pred_class_", seq_len(n_classes))
		avg_preds = combined_data[,
			lapply(.SD, function(x) mean(x, na.rm = TRUE)),
			.SDcols = agg_cols,
			by = .(.coalition_id, .test_instance_id)
		]

		# Rename aggregated columns to original class names
		setnames(avg_preds, agg_cols, class_names)
		avg_preds
	} else {
		# Regression: add predictions and aggregate
		combined_data[, .prediction := predictions]

		combined_data[,
			.(avg_pred = mean(.prediction, na.rm = TRUE)),
			by = .(.coalition_id, .test_instance_id)
		]
	}
}
