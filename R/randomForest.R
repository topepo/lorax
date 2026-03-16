#' Convert randomForest model to party object
#'
#' Convert a single tree from a randomForest model to a party object for use
#' with partykit visualization and analysis tools.
#'
#' @param obj A `randomForest` object from the \pkg{randomForest} package.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). Must be between 1 and the number of trees in the forest.
#' @param data Optional data.frame containing the training data. If NULL,
#'   a placeholder data.frame will be created with correct variable names but
#'   no observations. Providing data enables full party functionality including
#'   predictions.
#' @param ... Not currently used.
#'
#' @return A `party` object from the \pkg{partykit} package.
#'
#' @details
#' ## randomForest tree storage format
#'
#' The randomForest package stores trees in `obj$forest` as parallel matrices:
#' - `leftDaughter[i, tree]`: 1-based row index of left child (0 = no child)
#' - `rightDaughter[i, tree]`: 1-based row index of right child (0 = no child)
#' - `bestvar[i, tree]`: 1-based variable index for split (0 for terminal)
#' - `xbestsplit[i, tree]`: threshold value for split
#' - `nodestatus[i, tree]`: node status (-1 = terminal, -3 = internal)
#' - `nodepred[i, tree]`: prediction at node (for regression) or class (classification)
#'
#' ## Node indexing
#'
#' - randomForest uses 1-based row indices for nodes (root is row 1)
#' - Value 0 in leftDaughter/rightDaughter indicates no child
#' - User-facing `tree` parameter uses 1-based indexing (R convention)
#'
#' ## Split encoding
#'
#' - For numeric variables: left child when feature <= threshold, right child
#'   when feature > threshold
#' - Note: randomForest uses <= for left (different from ranger's <)
#' - partykit split created with `right = FALSE` to match this
#'
#' ## Terminal node identification
#'
#' - nodestatus == -1 indicates terminal node
#' - Alternatively: bestvar == 0 or both daughters == 0
#'
#' The party object will use 1-based node IDs and variable indices as required
#' by partykit.
#'
#' @examples
#' if (rlang::is_installed(c("randomForest", "palmerpenguins"))) {
#'   # Classification example
#'   data(penguins, package = "palmerpenguins")
#'   penguins <- na.omit(penguins)
#'
#'   rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 3)
#'
#'   # Convert first tree
#'   party_tree <- as.party(rf, tree = 1L, data = penguins)
#'   print(party_tree)
#'   plot(party_tree)
#'
#'   # Predictions from party object
#'   predict(party_tree, newdata = penguins[1:5, ])
#'
#'   # Regression example
#'   data(mtcars)
#'   rf_reg <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)
#'   party_tree_reg <- as.party(rf_reg, tree = 1L, data = mtcars)
#'   print(party_tree_reg)
#' }
#'
#' @export
as.party.randomForest <- function(obj, tree = 1L, data = NULL, ...) {
  rlang::check_installed("randomForest")
  # Validate tree parameter
  if (!is.numeric(tree) || length(tree) != 1 || tree != as.integer(tree)) {
    cli::cli_abort(
      "{.arg tree} must be a single integer, not {.obj_type_friendly {tree}}."
    )
  }
  tree <- as.integer(tree)

  if (tree < 1) {
    cli::cli_abort(
      "{.arg tree} must be >= 1, not {tree}."
    )
  }

  # Check that forest exists
  if (is.null(obj$forest)) {
    cli::cli_abort(
      "{.pkg randomForest} model must have {.code keep.forest = TRUE} to extract trees."
    )
  }

  # Validate tree against available trees
  num_trees <- obj$ntree
  if (tree > num_trees) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {num_trees}, not {tree}."
    )
  }

  # Extract tree data using getTree()
  tree_df <- randomForest::getTree(obj, k = tree, labelVar = FALSE)

  # Extract columns (note: column names have spaces)
  left_daughter <- tree_df[, "left daughter"]
  right_daughter <- tree_df[, "right daughter"]
  best_var <- tree_df[, "split var"]
  split_vals <- tree_df[, "split point"]
  node_status <- tree_df[, "status"]
  node_pred <- tree_df[, "prediction"]

  # Get variable names
  if (!is.null(obj$forest$xlevels)) {
    var_names <- names(obj$forest$xlevels)
  } else {
    # For models without xlevels, try to get from object
    var_names <- colnames(obj$importance)
    if (is.null(var_names)) {
      # Last resort: create generic names
      n_vars <- max(best_var[best_var > 0], na.rm = TRUE)
      var_names <- paste0("X", seq_len(n_vars))
    }
  }

  # Build partynode structure recursively starting from root (row 1)
  root_node <- rf_build_partynode(
    row_idx = 1L,
    left_daughter = left_daughter,
    right_daughter = right_daughter,
    best_var = best_var,
    split_vals = split_vals,
    node_status = node_status,
    node_pred = node_pred,
    var_names = var_names,
    obj = obj
  )

  # Assign sequential IDs
  root_node <- assign_node_ids(root_node)$node

  # Get original training data
  orig_data <- NULL
  response_name <- NULL

  if (!is.null(data)) {
    # Data parameter provided - use it
    orig_data <- validate_and_select_data(
      data,
      var_names,
      preserve_extra = TRUE
    )
  } else {
    # Try to extract from model call
    orig_data <- extract_data_from_call(obj, data_param = "data")
    if (!is.null(orig_data)) {
      orig_data <- validate_and_select_data(
        orig_data,
        var_names,
        preserve_extra = TRUE
      )
    }
  }

  # If no data available, create 0-row placeholder
  if (is.null(orig_data)) {
    orig_data <- reconstruct_data(var_names, n_obs = 0)
  }

  # Create terms object
  terms <- NULL
  if (nrow(orig_data) > 0) {
    formula <- stats::as.formula(paste(
      "~",
      paste(var_names, collapse = " + ")
    ))
    terms <- stats::terms(formula, data = orig_data)
  }

  # Create fitted values structure
  fitted <- NULL
  if (!is.null(orig_data) && nrow(orig_data) > 0) {
    # Compute which terminal node each observation belongs to
    fitted_ids <- compute_fitted_node_ids(root_node, orig_data)

    # Get response if available from call
    response <- extract_response_from_call(
      obj,
      response_param = "y",
      expected_length = nrow(orig_data)
    )

    fitted <- create_fitted_dataframe(fitted_ids, response)
  }

  # Create party object
  party_obj <- create_party_object(
    node = root_node,
    data = orig_data,
    fitted = fitted,
    terms = terms,
    info = list(method = "randomForest", tree = tree)
  )

  # Set class to constparty if we have response data
  if (!is.null(fitted) && "(response)" %in% names(fitted)) {
    class(party_obj) <- c("constparty", "party")
  }

  party_obj
}

# Internal helper to recursively build partynode from randomForest tree matrices
#
# @param row_idx Current node's row index (1-based)
# @param left_daughter Vector of left child row indices (1-based, 0 = no child)
# @param right_daughter Vector of right child row indices (1-based, 0 = no child)
# @param best_var Vector of split variable indices (1-based, 0 = terminal)
# @param split_vals Vector of split thresholds
# @param node_status Vector of node status (-1 = terminal, -3 = internal)
# @param node_pred Vector of node predictions
# @param var_names Character vector of variable names
# @param obj The randomForest object
#
# @return A partynode object
rf_build_partynode <- function(
  row_idx,
  left_daughter,
  right_daughter,
  best_var,
  split_vals,
  node_status,
  node_pred,
  var_names,
  obj
) {
  # Validate row_idx
  if (is.na(row_idx) || is.null(row_idx) || length(row_idx) != 1) {
    cli::cli_abort(
      "Invalid row_idx: {.val {row_idx}}"
    )
  }

  # Check if terminal node
  # Terminal: nodestatus == -1, or bestvar == 0, or both daughters == 0/NA
  is_terminal <- isTRUE(node_status[row_idx] == -1) ||
    isTRUE(best_var[row_idx] == 0) ||
    (isTRUE(left_daughter[row_idx] == 0) &&
      isTRUE(right_daughter[row_idx] == 0)) ||
    (isTRUE(is.na(left_daughter[row_idx])) &&
      isTRUE(is.na(right_daughter[row_idx])))

  if (is_terminal) {
    # Terminal node - no info for constparty
    return(partykit::partynode(id = 1L))
  }

  # Internal node - get split info
  var_idx <- best_var[row_idx] # 1-based variable index
  split_val <- split_vals[row_idx]

  # Check validity with safe comparisons
  valid_var_idx <- !is.na(var_idx) &&
    length(var_idx) == 1 &&
    var_idx >= 1 &&
    var_idx <= length(var_names)

  if (!valid_var_idx) {
    cli::cli_abort(
      "Invalid variable index {var_idx} at row {row_idx}."
    )
  }

  # Create split object
  # randomForest uses: left child when feature <= threshold
  # This requires right = FALSE in partykit (left interval is closed)
  split <- build_partysplit(var_idx, split_val, right = FALSE)

  # Get child row indices (1-based, 0 means no child)
  left_child_row <- left_daughter[row_idx]
  right_child_row <- right_daughter[row_idx]

  # Validate children
  left_valid <- !is.na(left_child_row) &&
    length(left_child_row) == 1 &&
    left_child_row > 0
  right_valid <- !is.na(right_child_row) &&
    length(right_child_row) == 1 &&
    right_child_row > 0

  if (!left_valid || !right_valid) {
    cli::cli_abort(
      "Internal node at row {row_idx} has invalid children (left={left_child_row}, right={right_child_row})."
    )
  }

  # Recursively build children
  left_node <- rf_build_partynode(
    left_child_row,
    left_daughter,
    right_daughter,
    best_var,
    split_vals,
    node_status,
    node_pred,
    var_names,
    obj
  )

  right_node <- rf_build_partynode(
    right_child_row,
    left_daughter,
    right_daughter,
    best_var,
    split_vals,
    node_status,
    node_pred,
    var_names,
    obj
  )

  # Return internal node
  partykit::partynode(
    id = 1L, # Will be updated by assign_node_ids
    split = split,
    kids = list(left_node, right_node),
    info = list(row_idx = row_idx)
  )
}
