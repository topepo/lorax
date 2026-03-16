# Helper functions for creating party objects from various model types
#
# These helpers provide common utilities for building partykit structures
# from tree-based models. The main components are:
#
# - `build_partysplit()`: Creates a partysplit object for numeric splits
# - `reconstruct_data()`: Creates placeholder data when training data unavailable
# - `create_party_object()`: Wrapper around [partykit::party()] constructor

# Build a standard numeric partysplit object
#
# Creates a partysplit for numeric variables with a single threshold.
# Standard pattern: left child when var < threshold (or <=), right child
# when var >= threshold (or >).
#
# @param varid Integer, 1-based column index in model.frame
# @param threshold Numeric split point
# @param right Logical, whether right interval is closed. If TRUE, split is:
#   - Kid 1: var < threshold
#   - Kid 2: var >= threshold (right interval closed)
#   If FALSE, split is:
#   - Kid 1: var <= threshold (left interval closed)
#   - Kid 2: var > threshold
#
# @return A partysplit object
build_partysplit <- function(varid, threshold, right = TRUE) {
  # Validate inputs
  if (is.na(varid) || length(varid) == 0) {
    cli::cli_abort(
      "Invalid varid for partysplit: {.val {varid}}"
    )
  }

  if (is.na(threshold) || length(threshold) == 0 || !is.numeric(threshold)) {
    cli::cli_abort(
      "Invalid threshold for partysplit: {.val {threshold}}"
    )
  }

  partykit::partysplit(
    varid = as.integer(varid),
    breaks = as.numeric(threshold),
    index = 1:2,
    right = right
  )
}

# Create placeholder data.frame when training data unavailable
#
# Generates a minimal data.frame with correct variable names and types.
# This allows party objects to be created for structure inspection even
# without original training data.
#
# @param var_names Character vector of variable names
# @param var_types Optional character vector of variable types ("numeric",
#   "factor", etc.). If NULL, all variables are numeric.
# @param n_obs Number of rows to create (default 0)
#
# @return A data.frame with appropriate structure
reconstruct_data <- function(var_names, var_types = NULL, n_obs = 0L) {
  if (is.null(var_types)) {
    var_types <- rep("numeric", length(var_names))
  }

  # Create empty data frame with correct types
  df <- as.data.frame(matrix(
    nrow = n_obs,
    ncol = length(var_names),
    dimnames = list(NULL, var_names)
  ))

  # Set column types
  for (i in seq_along(var_names)) {
    if (var_types[i] == "factor") {
      df[[i]] <- factor(character(n_obs))
    } else if (var_types[i] == "integer") {
      df[[i]] <- integer(n_obs)
    } else {
      # Default to numeric
      df[[i]] <- numeric(n_obs)
    }
  }

  df
}

# Wrapper around partykit::party() constructor
#
# Creates a party object with validation and proper structure.
#
# @param node A partynode object (root of tree)
# @param data A data.frame with training data
# @param fitted Optional data.frame with fitted values
# @param terms Optional terms object from model formula
# @param info Optional list of additional model metadata
#
# @return A party object
create_party_object <- function(
  node,
  data,
  fitted = NULL,
  terms = NULL,
  info = NULL
) {
  # Validate node is a partynode
  if (!inherits(node, "partynode")) {
    cli::cli_abort(
      "{.arg node} must be a partynode object."
    )
  }

  # Validate data is a data.frame
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data.frame."
    )
  }

  # Create party object
  partykit::party(
    node = node,
    data = data,
    fitted = fitted,
    terms = terms,
    info = info
  )
}

# Build partynode recursively from tabular tree representation
#
# Generic builder for tree formats that provide explicit parent-child
# relationships in a data.frame (e.g., ranger, randomForest, xgboost, lightgbm).
#
# @param tree_df Data.frame with tree structure
# @param node_id Current node ID to process
# @param node_id_col Name of column containing node IDs
# @param left_child_col Name of column containing left child IDs
# @param right_child_col Name of column containing right child IDs
# @param split_var_col Name of column containing split variable indices/names
# @param split_val_col Name of column containing split values
# @param is_leaf_col Name of column indicating terminal nodes (or NULL)
# @param is_leaf_value Value that indicates terminal node (e.g., TRUE, -1, 0)
# @param prediction_col Name of column containing terminal node predictions
# @param var_names Character vector of variable names for mapping
# @param zero_indexed Logical, whether node IDs and var indices are 0-based
#
# @return A partynode object
build_partynode_from_tabular <- function(
  tree_df,
  node_id,
  node_id_col,
  left_child_col,
  right_child_col,
  split_var_col,
  split_val_col,
  is_leaf_col = NULL,
  is_leaf_value = TRUE,
  prediction_col,
  var_names,
  zero_indexed = FALSE
) {
  # Find current node's row
  node_row <- tree_df[tree_df[[node_id_col]] == node_id, ]

  if (nrow(node_row) == 0) {
    cli::cli_abort("Node {node_id} not found in tree structure.")
  }

  # Use first matching row if duplicates
  node_row <- node_row[1, ]

  # Determine sequential node ID (1-based for partykit)
  # This will be assigned later in a post-order traversal
  sequential_id <- 1L

  # Check if terminal node
  is_terminal <- FALSE
  if (!is.null(is_leaf_col)) {
    is_terminal <- identical(node_row[[is_leaf_col]], is_leaf_value)
  } else {
    # Check if both children are missing/invalid
    left_child <- node_row[[left_child_col]]
    right_child <- node_row[[right_child_col]]

    if (zero_indexed) {
      # 0 or negative values indicate no child in 0-indexed systems
      is_terminal <- (is.na(left_child) || left_child < 0) &&
        (is.na(right_child) || right_child < 0)
    } else {
      # NA or 0 indicate no child in 1-indexed systems
      is_terminal <- (is.na(left_child) || left_child == 0) &&
        (is.na(right_child) || right_child == 0)
    }
  }

  if (is_terminal) {
    # Terminal node - no info for constparty
    return(partykit::partynode(id = sequential_id))
  } else {
    # Internal node - get split info
    split_var_id <- node_row[[split_var_col]]
    split_val <- node_row[[split_val_col]]

    # Map variable to 1-based index in var_names
    if (is.character(split_var_id)) {
      # Variable name provided directly
      varid <- which(var_names == split_var_id)
      if (length(varid) == 0) {
        cli::cli_abort("Variable {split_var_id} not found in var_names.")
      }
    } else {
      # Variable index provided
      if (zero_indexed) {
        varid <- as.integer(split_var_id) + 1L
      } else {
        varid <- as.integer(split_var_id)
      }
    }

    # Create split (assuming standard < vs >= pattern with right = TRUE)
    split <- build_partysplit(varid, split_val, right = TRUE)

    # Recursively build child nodes
    left_child_id <- node_row[[left_child_col]]
    right_child_id <- node_row[[right_child_col]]

    left_node <- build_partynode_from_tabular(
      tree_df,
      left_child_id,
      node_id_col,
      left_child_col,
      right_child_col,
      split_var_col,
      split_val_col,
      is_leaf_col,
      is_leaf_value,
      prediction_col,
      var_names,
      zero_indexed
    )

    right_node <- build_partynode_from_tabular(
      tree_df,
      right_child_id,
      node_id_col,
      left_child_col,
      right_child_col,
      split_var_col,
      split_val_col,
      is_leaf_col,
      is_leaf_value,
      prediction_col,
      var_names,
      zero_indexed
    )

    return(partykit::partynode(
      id = sequential_id,
      split = split,
      kids = list(left_node, right_node)
    ))
  }
}

# Assign sequential IDs to partynodes in pre-order traversal
#
# partykit requires sequential integer IDs starting from 1. This function
# traverses the tree and assigns IDs in pre-order (parent before children).
#
# @param node A partynode object
# @param next_id Integer, the next ID to assign (default 1L)
#
# @return A list with elements:
#   - node: partynode with updated IDs
#   - next_id: next available ID after this subtree
assign_node_ids <- function(node, next_id = 1L) {
  # Assign current ID
  node$id <- next_id
  next_id <- next_id + 1L

  # Recursively assign to children
  kids <- partykit::kids_node(node)
  if (!is.null(kids) && length(kids) > 0) {
    new_kids <- list()
    for (i in seq_along(kids)) {
      result <- assign_node_ids(kids[[i]], next_id)
      new_kids[[i]] <- result$node
      next_id <- result$next_id
    }
    node$kids <- new_kids
  }

  list(node = node, next_id = next_id)
}

# Count total nodes in a tree rooted at given partynode
#
# @param node A partynode object
# @return Integer count of nodes
count_nodes <- function(node) {
  if (is.null(node)) {
    return(0L)
  }

  count <- 1L
  kids <- partykit::kids_node(node)
  if (!is.null(kids)) {
    for (kid in kids) {
      count <- count + count_nodes(kid)
    }
  }

  count
}

# Compute fitted node IDs for observations by traversing tree
#
# Given a partynode tree and data, determines which terminal node each
# observation belongs to.
#
# @param node A partynode object (root of tree)
# @param data A data.frame with observations to classify
#
# @return Integer vector of terminal node IDs (one per row in data)
compute_fitted_node_ids <- function(node, data) {
  if (nrow(data) == 0) {
    return(integer(0))
  }

  node_ids <- integer(nrow(data))

  # Traverse tree for each observation
  for (i in seq_len(nrow(data))) {
    node_ids[i] <- traverse_to_terminal(node, data[i, , drop = FALSE])
  }

  node_ids
}

# Remove predictions from terminal node info for constparty
#
# For constparty objects, terminal nodes should have NULL info.
# Predictions are computed from fitted values by partykit methods.
#
# @param tree_node A partynode object (root of tree)
#
# @return The tree_node with NULL info in all terminal nodes
remove_terminal_predictions <- function(tree_node) {
  kids <- partykit::kids_node(tree_node)

  if (is.null(kids) || length(kids) == 0) {
    # Terminal node - set info to NULL
    tree_node$info <- NULL
  } else {
    # Internal node - recurse to children
    for (i in seq_along(kids)) {
      kids[[i]] <- remove_terminal_predictions(kids[[i]])
    }
    tree_node$kids <- kids
  }

  tree_node
}

# Traverse tree to find terminal node for a single observation
#
# @param node Current partynode
# @param obs Single-row data.frame with observation data
#
# @return Integer node ID where observation lands
traverse_to_terminal <- function(node, obs) {
  # Check if terminal
  kids <- partykit::kids_node(node)
  if (is.null(kids) || length(kids) == 0) {
    return(partykit::id_node(node))
  }

  # Get split info
  split <- partykit::split_node(node)
  if (is.null(split)) {
    return(partykit::id_node(node))
  }

  # Determine which child to follow
  varid <- partykit::varid_split(split)
  var_value <- obs[[varid]]

  # Handle missing values
  if (is.na(var_value)) {
    # Default to left child for missing values
    child_idx <- 1L
  } else {
    # Get split direction
    breaks <- partykit::breaks_split(split)
    index <- partykit::index_split(split)
    right <- partykit::right_split(split)

    # Check if variable is numeric or can be coerced
    if (!is.null(breaks)) {
      # Numeric split - may need to convert factors to numeric
      if (is.factor(var_value)) {
        # Factor variable: convert to numeric for comparison
        # This handles cases where randomForest encodes factors as numeric
        var_numeric <- as.numeric(var_value)
      } else if (is.numeric(var_value)) {
        var_numeric <- var_value
      } else {
        # Cannot handle this type, default to left child
        child_idx <- 1L
        var_numeric <- NULL
      }

      if (!is.null(var_numeric)) {
        if (right) {
          # Right interval closed: left when < breaks, right when >= breaks
          child_idx <- if (var_numeric < breaks) 1L else 2L
        } else {
          # Left interval closed: left when <= breaks, right when > breaks
          child_idx <- if (var_numeric <= breaks) 1L else 2L
        }
      }
    } else {
      # Categorical split with index
      # index maps factor levels to child indices
      level_idx <- match(as.character(var_value), levels(obs[[varid]]))
      if (is.na(level_idx) || level_idx > length(index)) {
        child_idx <- 1L # Default
      } else {
        child_idx <- index[level_idx]
      }
    }
  }

  # Recursively traverse to child
  if (child_idx <= length(kids)) {
    traverse_to_terminal(kids[[child_idx]], obs)
  } else {
    # Shouldn't happen, but return current node as fallback
    partykit::id_node(node)
  }
}

# REMOVED: extract_response_from_call
# This function relied on evaluating objects from the model's call, which is
# problematic as it depends on objects being available in specific environments.
# Instead, extract response directly from:
# - Model object (e.g., randomForest$y)
# - Data parameter (extract using Terms or variable names)
# - Or don't create fitted values if response not available

# Validate and select predictor columns from data
#
# Ensures that data contains the required predictor variables and selects
# them along with any additional columns (like response variables).
#
# @param data Data.frame to validate and subset
# @param var_names Character vector of required predictor names
# @param preserve_extra Logical, whether to preserve columns beyond predictors
#
# @return Data.frame with predictors (and optionally additional columns)
validate_and_select_data <- function(data, var_names, preserve_extra = TRUE) {
  # Check that data has all required variables
  if (!all(var_names %in% names(data))) {
    missing <- setdiff(var_names, names(data))
    cli::cli_abort(
      "{.arg data} must contain variables: {.field {missing}}."
    )
  }

  # Select predictor columns
  result <- data[, var_names, drop = FALSE]

  # Preserve any additional columns if requested
  if (preserve_extra) {
    extra_cols <- setdiff(names(data), var_names)
    for (col in extra_cols) {
      result[[col]] <- data[[col]]
    }
  }

  result
}

# Create fitted dataframe for party object
#
# Creates the fitted dataframe expected by party objects, containing
# fitted node IDs and optionally the response variable.
#
# @param fitted_ids Integer vector of terminal node IDs
# @param response Optional response vector
#
# @return Data.frame with "(fitted)" column and optionally "(response)" column
create_fitted_dataframe <- function(fitted_ids, response = NULL) {
  if (is.null(response)) {
    data.frame(
      "(fitted)" = fitted_ids,
      check.names = FALSE
    )
  } else {
    data.frame(
      "(fitted)" = fitted_ids,
      "(response)" = response,
      check.names = FALSE
    )
  }
}

# REMOVED: extract_data_from_call
# This function relied on evaluating objects from the model's call, which is
# problematic as it depends on objects being available in specific environments.
# Instead:
# - Require data parameter to be passed explicitly
# - Or create placeholder data with reconstruct_data() for structure inspection

# Validate party object has proper node information
#
# Checks if a party object will display proper node summaries (not asterisks).
# A party object needs fitted values with response data to compute terminal
# node summaries for constparty objects.
#
# @param party_obj A party object to validate
# @param action Character: "error" to abort, "warn" to warn, "silent" to return logical
#
# @return Logical indicating if party object is valid (only when action = "silent")
validate_party_node_info <- function(
  party_obj,
  action = c("error", "warn", "silent")
) {
  action <- match.arg(action)

  # Check if this will be a constparty (has fitted response)
  has_fitted <- !is.null(party_obj$fitted)
  has_response <- has_fitted && "(response)" %in% names(party_obj$fitted)

  # If we have fitted data, we need response for proper summaries
  is_valid <- !has_fitted || has_response

  if (!is_valid) {
    msg <- c(
      "Party object will show asterisks (*) in terminal node summaries.",
      "i" = "The party object has fitted values but no response variable.",
      "i" = "Provide the {.arg data} parameter with response variable included."
    )

    if (action == "error") {
      cli::cli_abort(msg)
    } else if (action == "warn") {
      cli::cli_warn(msg)
    }
  }

  invisible(is_valid)
}
