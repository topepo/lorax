#' Convert ranger model to party object
#'
#' Convert a single tree from a ranger random forest model to a party object
#' for use with partykit visualization and analysis tools.
#'
#' @param obj A `ranger` object from the ranger package.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). Must be between 1 and the number of trees in the forest.
#' @param data Optional data.frame containing the training data. If NULL,
#'   a placeholder data.frame will be created with correct variable names but
#'   no observations. Providing data enables full party functionality including
#'   predictions.
#' @param ... Not currently used.
#'
#' @return A `party` object from the partykit package.
#'
#' @details
#' ## Ranger tree storage format
#'
#' The ranger package stores trees in `obj$forest` with parallel vectors:
#' - `split.varIDs[[tree]]`: 0-based variable indices for splits
#' - `split.values[[tree]]`: threshold values for splits
#' - `child.nodeIDs[[tree]]`: matrix with 2 columns (left, right child IDs)
#' - `is.ordered[[tree]]`: whether split variable is ordered (for categoricals)
#' - All node IDs are 0-based (root = 0)
#'
#' ## Node indexing
#'
#' - Internally, ranger uses 0-based node indices (root is node 0)
#' - User-facing `tree` parameter uses 1-based indexing (R convention)
#' - Leaf nodes have split.varIDs entry of NA or large sentinel value
#'
#' ## Split encoding
#'
#' - For numeric variables: left child when feature < threshold, right child
#'   when feature >= threshold
#' - partykit split created with `right = TRUE` (right interval closed)
#'
#' ## Child node references
#'
#' - `child.nodeIDs` is a matrix with 2 columns: left_child, right_child
#' - Value 0 indicates no child (terminal node)
#' - Both children 0 means current node is terminal
#'
#' The party object will use 1-based node IDs and variable indices as required
#' by partykit.
#'
#' @examples
#' if (rlang::is_installed(c("ranger", "palmerpenguins"))) {
#'   data(penguins, package = "palmerpenguins")
#'   penguins <- na.omit(penguins)
#'
#'   rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 10)
#'
#'   # Convert first tree
#'   party_tree <- as.party(rf, tree = 1, data = penguins)
#'   print(party_tree)
#'   plot(party_tree)
#'
#'   # Predictions from party object
#'   predict(party_tree, newdata = penguins[1:5, ])
#' }
#'
#' @export
as.party.ranger <- function(obj, tree = 1L, data = NULL, ...) {
  rlang::check_installed("ranger")
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
      "ranger model must have {.code write.forest = TRUE} to extract trees."
    )
  }

  # Validate tree against available trees
  num_trees <- obj$num.trees
  if (tree > num_trees) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {num_trees}, not {tree}."
    )
  }

  # Use ranger::treeInfo for reliable tree extraction
  # This properly handles all the indexing and structure
  tree_info <- ranger::treeInfo(obj, tree = tree)

  # Get variable names
  var_names <- obj$forest$independent.variable.names

  # Build partynode structure from tree_info starting at root (nodeID = 0)
  root_node <- ranger_build_partynode_from_info(
    node_id = 0,
    tree_info = tree_info,
    var_names = var_names
  )

  # Assign sequential IDs
  root_node <- assign_node_ids(root_node)$node

  # Get original training data
  # Try to extract from model call or use provided data parameter
  orig_data <- NULL
  response_name <- obj$dependent.variable.name

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
    # Add response if available
    if (!is.null(response_name) && response_name %in% names(data)) {
      orig_data[[response_name]] <- data[[response_name]]
    }
  } else if (!is.null(obj$call$data)) {
    # Try to extract from model call
    orig_data <- try(
      eval(obj$call$data, envir = parent.frame(2)),
      silent = TRUE
    )
    if (inherits(orig_data, "try-error")) {
      orig_data <- NULL
    } else {
      # Select predictor columns
      orig_data_pred <- orig_data[, var_names, drop = FALSE]
      # Add response if available
      if (!is.null(response_name) && response_name %in% names(orig_data)) {
        orig_data_pred[[response_name]] <- orig_data[[response_name]]
      }
      orig_data <- orig_data_pred
    }
  }

  # If no data available, create 0-row placeholder
  if (is.null(orig_data)) {
    orig_data <- reconstruct_data(var_names, n_obs = 0)
  }

  # Create terms object
  terms <- NULL
  if (!is.null(orig_data) && nrow(orig_data) > 0) {
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
    if (!is.null(response_name) && response_name %in% names(orig_data)) {
      response <- orig_data[[response_name]]
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
    info = list(method = "ranger", tree = tree)
  )

  # Set class to constparty if we have response data
  if (!is.null(fitted) && "(response)" %in% names(fitted)) {
    class(party_obj) <- c("constparty", "party")
  }

  party_obj
}

# Internal helper to recursively build partynode from ranger::treeInfo output
#
# @param node_id Current node ID (0-based, from treeInfo)
# @param tree_info Data.frame from ranger::treeInfo()
# @param var_names Character vector of variable names
#
# @return A partynode object
ranger_build_partynode_from_info <- function(node_id, tree_info, var_names) {
  # Find current node's row in tree_info
  node_row <- tree_info[tree_info$nodeID == node_id, ]

  if (nrow(node_row) == 0) {
    cli::cli_abort("Node {node_id} not found in tree info.")
  }

  node_row <- node_row[1, ]

  # Check if terminal node
  if (node_row$terminal) {
    # Terminal node - no info for constparty
    return(partykit::partynode(id = 1L))
  }

  # Internal node - get split info
  # splitvarID in treeInfo is 0-based, convert to 1-based for partykit
  varid <- as.integer(node_row$splitvarID) + 1L
  split_val <- node_row$splitval

  if (varid < 1 || varid > length(var_names)) {
    cli::cli_abort(
      "Invalid variable index {node_row$splitvarID} at node {node_id}."
    )
  }

  # Create split object
  # ranger uses: left child when feature < threshold
  split <- build_partysplit(varid, split_val, right = TRUE)

  # Get child node IDs (0-based)
  left_child_id <- node_row$leftChild
  right_child_id <- node_row$rightChild

  # Recursively build children
  left_node <- ranger_build_partynode_from_info(
    left_child_id,
    tree_info,
    var_names
  )

  right_node <- ranger_build_partynode_from_info(
    right_child_id,
    tree_info,
    var_names
  )

  # Return internal node
  partykit::partynode(
    id = 1L, # Will be updated by assign_node_ids
    split = split,
    kids = list(left_node, right_node),
    info = list(node_id = node_id)
  )
}
