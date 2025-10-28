#' Perform batched predictions with automatic handling of fast vs regular predict methods
#'
#' @param learner mlr3 Learner object
#' @param data_list list of data.tables to predict on
#' @param task mlr3 Task object
#' @param test_row_ids integer vector of test row IDs for constructing predictions
#' @param batch_size integer or NULL. If NULL, predicts all data at once. Otherwise batches by this size.
#'
#' @return list of [Prediction][mlr3::Prediction] objects, one per element in `data_list`
#' @keywords internal
#' @noRd
predict_batched = function(learner, data_list, task, test_row_ids, batch_size = NULL) {
	n_data = length(data_list)
	test_size = length(test_row_ids)
	use_fast = is.function(learner$predict_newdata_fast)

	# Helper to construct Prediction object from raw prediction list
	construct_pred = function(raw_pred) {
		truth = task$truth(rows = test_row_ids)

		switch(
			task$task_type,
			classif = PredictionClassif$new(
				row_ids = test_row_ids,
				truth = truth,
				response = raw_pred$response,
				prob = raw_pred$prob
			),
			regr = PredictionRegr$new(
				row_ids = test_row_ids,
				truth = truth,
				response = raw_pred$response
			)
		)
	}

	# If no batching or only one data element, predict directly
	if (is.null(batch_size) || n_data == 1) {
		combined_data = rbindlist(data_list)

		if (use_fast) {
			# Fast prediction on all data
			pred_raw = learner$predict_newdata_fast(newdata = combined_data, task = task)

			# Split into n_data predictions
			preds = lapply(seq_len(n_data), function(i) {
				start_idx = (i - 1) * test_size + 1
				end_idx = i * test_size

				# Extract slice for this repeat
				# Need to preserve matrix structure for prob predictions
				pred_slice = lapply(pred_raw, function(component) {
					if (!is.null(component) && length(component) > 0) {
						if (is.matrix(component)) {
							component[start_idx:end_idx, , drop = FALSE]
						} else {
							component[start_idx:end_idx]
						}
					} else {
						NULL
					}
				})

				construct_pred(pred_slice)
			})
		} else {
			# Regular prediction per data element
			preds = lapply(data_list, function(data_chunk) {
				learner$predict_newdata(newdata = data_chunk, task = task)
			})
		}

		return(preds)
	}

	# Batched prediction case
	# Calculate how many data elements fit in each batch
	data_per_batch = max(1, floor(batch_size / test_size))
	n_batches = ceiling(n_data / data_per_batch)

	preds = vector("list", n_data)

	for (batch_idx in seq_len(n_batches)) {
		# Determine which data elements are in this batch
		start_data_idx = (batch_idx - 1) * data_per_batch + 1
		end_data_idx = min(batch_idx * data_per_batch, n_data)
		data_indices = start_data_idx:end_data_idx

		# Combine data for this batch
		batch_data = rbindlist(data_list[data_indices])

		if (use_fast) {
			pred_raw = learner$predict_newdata_fast(newdata = batch_data, task = task)

			# Split back into individual predictions
			for (i in seq_along(data_indices)) {
				start_idx = (i - 1) * test_size + 1
				end_idx = i * test_size

				# Need to preserve matrix structure for prob predictions
				pred_slice = lapply(pred_raw, function(component) {
					if (!is.null(component) && length(component) > 0) {
						if (is.matrix(component)) {
							component[start_idx:end_idx, , drop = FALSE]
						} else {
							component[start_idx:end_idx]
						}
					} else {
						NULL
					}
				})

				preds[[data_indices[i]]] = construct_pred(pred_slice)
			}
		} else {
			# Regular prediction: need to predict each data element separately
			for (i in seq_along(data_indices)) {
				preds[[data_indices[i]]] = learner$predict_newdata(
					newdata = data_list[[data_indices[i]]],
					task = task
				)
			}
		}
	}

	preds
}
