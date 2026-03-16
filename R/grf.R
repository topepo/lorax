#' Convert grf model to party object
#'
#' Convert a single tree from a grf (generalized random forests) model to a
#' party object for use with partykit visualization and analysis tools.
#'
#' @param obj A grf object (e.g., `regression_forest`, `causal_forest`) from
#'   the grf package.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). Must be between 1 and the number of trees in the forest.
#' @param data Optional data.frame containing the training data. If NULL,
#'   will attempt to extract from the grf object (`obj$X.orig`), or create a
#'   placeholder data.frame. Providing data enables full party functionality.
#' @param ... Not currently used.
#'
#' @return A `party` object from the partykit package.
#'
#' @details
#' ## GRF tree storage format
#'
#' The grf package stores trees in a nested list structure, typically accessed
#' via `grf::get_tree(obj, tree)`. Each tree is represented as nested lists:
#' - `is_leaf`: Logical, TRUE for terminal nodes
#' - `split_variable`: 0-based index of variable to split on (internal nodes)
#' - `split_value`: Numeric threshold for split (internal nodes)
#' - `left_child`: Nested list for left subtree (internal nodes)
#' - `right_child`: Nested list for right subtree (internal nodes)
#' - Leaf nodes contain prediction information
#'
#' ## Node indexing
#'
#' - Internally, grf uses 0-based variable indices
#' - User-facing `tree` parameter uses 1-based indexing (R convention)
#' - Trees use 0-based indexing internally but we access with 1-based tree number
#'
#' ## Split encoding
#'
#' - For numeric variables: left child when feature < threshold, right child
#'   when feature >= threshold
#' - partykit split created with `right = TRUE` (right interval closed)
#'
#' ## Tree structure
#'
#' - grf provides nested list structure (not flattened)
#' - This is the most direct representation for recursive conversion
#' - Each node is a list with is_leaf flag and split info
#'
#' The party object will use 1-based node IDs and variable indices as required
#' by partykit.
#'
#' @examples
#' \dontrun{
#' library(grf)
#'
#' # Regression forest
#' rf <- regression_forest(X = penguins[, ], Y = penguins$bill_length_mm, num.trees = 10)
#'
#' # Convert first tree
#' party_tree <- as.party(rf, tree = 1)
#' print(party_tree)
#' plot(party_tree)
#'
#' # Can also work with other grf forest types
#' cf <- causal_forest(X = penguins[, 3:6], Y = penguins$bill_length_mm,
#'                     W = rbinom(150, 1, 0.5), num.trees = 10)
#' party_tree2 <- as.party(cf, tree = 1)
#' }
#'
#' @export
as.party.regression_forest <- function(obj, tree = 1L, data = NULL, ...) {
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

  # Check that tree structure is available
  if (is.null(obj[["_tree_drawn"]]) && is.null(obj[["X.orig"]])) {
    cli::cli_abort(
      "Cannot extract tree structure from grf object. Ensure model was fitted with {.code num.trees > 0}."
    )
  }

  # Validate tree against available trees
  num_trees <- obj$`_num_trees`
  if (is.null(num_trees)) {
    # Try alternative attribute names
    num_trees <- length(obj[["_tree_drawn"]])
    if (is.null(num_trees) || num_trees == 0) {
      num_trees <- 1000 # Default grf value
    }
  }
  if (tree > num_trees) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {num_trees}, not {tree}."
    )
  }

  # Extract tree structure using grf API
  # grf::get_tree returns nested list structure
  # grf uses 1-based tree indexing (R convention)
  tree_list <- grf::get_tree(obj, tree)

  # Get variable names
  if (!is.null(obj$X.orig)) {
    var_names <- colnames(obj$X.orig)
    if (is.null(var_names)) {
      var_names <- paste0("X", seq_len(ncol(obj$X.orig)))
    }
  } else if (!is.null(colnames(obj[["_data"]]))) {
    var_names <- colnames(obj[["_data"]])
  } else {
    # Infer from tree structure
    max_var <- grf_find_max_split_var(tree_list$nodes)
    var_names <- paste0("X", seq_len(max_var)) # grf uses 1-based indexing
  }

  # Build partynode structure recursively from nested list
  # grf returns tree structure in tree_list$nodes, with root at index 1
  if (is.null(tree_list$nodes) || length(tree_list$nodes) == 0) {
    cli::cli_abort("Tree structure is empty or invalid.")
  }

  root_node <- grf_build_partynode(
    tree_node = tree_list$nodes[[1]],
    all_nodes = tree_list$nodes,
    var_names = var_names
  )

  # Assign sequential IDs
  root_node <- assign_node_ids(root_node)$node

  # Get original training data
  orig_data <- NULL

  if (!is.null(data)) {
    # Data parameter provided - use it
    # Validate data has correct variables
    if (!all(var_names %in% names(data))) {
      missing <- setdiff(var_names, names(data))
      cli::cli_abort(
        "{.arg data} must contain variables: {.field {missing}}."
      )
    }
    # Select predictor columns
    orig_data <- data[, var_names, drop = FALSE]
    # Preserve any additional columns beyond features (e.g., response)
    extra_cols <- setdiff(names(data), var_names)
    for (col in extra_cols) {
      orig_data[[col]] <- data[[col]]
    }
  } else if (!is.null(obj$X.orig)) {
    # Try to extract from model
    orig_data <- as.data.frame(obj$X.orig)
    if (is.null(colnames(orig_data))) {
      colnames(orig_data) <- var_names
    }
    # Try to add response from Y.orig if available
    if (!is.null(obj$Y.orig)) {
      orig_data[["response"]] <- obj$Y.orig
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
    fitted_ids <- compute_fitted_node_ids(
      root_node,
      orig_data[, var_names, drop = FALSE]
    )

    # Get response if available
    response <- NULL
    if ("response" %in% names(orig_data)) {
      response <- orig_data[["response"]]
    } else if (!is.null(obj$Y.orig)) {
      response <- obj$Y.orig
      if (length(response) != nrow(orig_data)) {
        response <- NULL
      }
    }

    if (!is.null(response)) {
      fitted <- data.frame(
        "(fitted)" = fitted_ids,
        "(response)" = response,
        check.names = FALSE
      )
    } else {
      fitted <- data.frame(
        "(fitted)" = fitted_ids,
        check.names = FALSE
      )
    }
  }

  # Create party object
  party_obj <- create_party_object(
    node = root_node,
    data = orig_data,
    fitted = fitted,
    terms = terms,
    info = list(method = "grf", tree = tree)
  )

  # Set class to constparty if we have response data
  if (!is.null(fitted) && "(response)" %in% names(fitted)) {
    class(party_obj) <- c("constparty", "party")
  }

  party_obj
}

#' @rdname as.party.regression_forest
#' @export
as.party.grf <- function(obj, tree = 1L, data = NULL, ...) {
  # Generic method that delegates to regression_forest method
  as.party.regression_forest(obj, tree = tree, data = data, ...)
}

# Internal helper to recursively build partynode from grf tree structure
#
# @param tree_node Current node from grf::get_tree()$nodes list
# @param all_nodes Complete nodes list from grf::get_tree()$nodes
# @param var_names Character vector of variable names
#
# @return A partynode object
grf_build_partynode <- function(tree_node, all_nodes, var_names) {
  # Check if terminal node
  if (tree_node$is_leaf) {
    # Terminal node - no info for constparty
    return(partykit::partynode(id = 1L))
  }

  # Internal node - get split info
  split_var_id <- tree_node$split_variable # 1-based variable index (grf convention)
  split_val <- tree_node$split_value

  # grf uses 1-based variable indexing, same as partykit
  varid <- as.integer(split_var_id)

  if (varid < 1 || varid > length(var_names)) {
    cli::cli_abort(
      "Invalid variable index {split_var_id} in tree structure."
    )
  }

  # Create split object
  # grf uses: left child when feature < threshold
  split <- build_partysplit(varid, split_val, right = TRUE)

  # Get child nodes using 1-based indices
  left_child_idx <- as.integer(tree_node$left_child)
  right_child_idx <- as.integer(tree_node$right_child)

  # Recursively build children
  left_node <- grf_build_partynode(
    all_nodes[[left_child_idx]],
    all_nodes,
    var_names
  )
  right_node <- grf_build_partynode(
    all_nodes[[right_child_idx]],
    all_nodes,
    var_names
  )

  # Return internal node
  partykit::partynode(
    id = 1L, # Will be updated by assign_node_ids
    split = split,
    kids = list(left_node, right_node)
  )
}

# Internal helper to find maximum split variable index in tree
#
# Used when variable names not available to infer number of variables
#
# @param all_nodes List of all nodes from grf::get_tree()$nodes
#
# @return Integer, maximum 1-based variable index found
grf_find_max_split_var <- function(all_nodes) {
  max_var <- -1L

  for (node in all_nodes) {
    if (!node$is_leaf && !is.null(node$split_variable)) {
      max_var <- max(max_var, node$split_variable, na.rm = TRUE)
    }
  }

  max_var
}
