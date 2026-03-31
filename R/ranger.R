#' Convert ranger model to party object
#'
#' Convert a single tree from a \pkg{ranger} random forest model to a party object
#' for use with \pkg{partykit} visualization and analysis tools.
#'
#' @param obj A `ranger` object from the \pkg{ranger} package.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). Must be between 1 and the number of trees in the forest.
#' @param data Data.frame containing the training data, including both predictors
#'   and response variable. Required for proper party object creation with fitted
#'   values and node summaries.
#' @param ... Not currently used.
#'
#' @return A `party` object from the \pkg{partykit} package.
#'
#' @details
#' ## Ranger tree storage format
#'
#' The \pkg{ranger} package stores trees in `obj$forest` with parallel vectors:
#' - `split.varIDs[[tree]]`: 0-based variable indices for splits
#' - `split.values[[tree]]`: threshold values for splits
#' - `child.nodeIDs[[tree]]`: matrix with 2 columns (left, right child IDs)
#' - `is.ordered[[tree]]`: whether split variable is ordered (for categoricals)
#' - All node IDs are 0-based (root = 0)
#'
#' ## Node indexing
#'
#' - Internally, \pkg{ranger} uses 0-based node indices (root is node 0)
#' - User-facing `tree` parameter uses 1-based indexing (R convention)
#' - Leaf nodes have `split.varIDs` entry of `NA` or large sentinel value
#'
#' ## Split encoding
#'
#' - For numeric variables: left child when feature < threshold, right child
#'   when feature >= threshold
#' - \pkg{partykit} split created with `right = TRUE` (right interval closed)
#'
#' ## Child node references
#'
#' - `child.nodeIDs` is a matrix with 2 columns: `left_child`, `right_child`
#' - Value 0 indicates no child (terminal node)
#' - Both children 0 means current node is terminal
#'
#' The party object will use 1-based node IDs and variable indices as required
#' by \pkg{partykit}.
#'
#' @examples
#' if (rlang::is_installed(c("ranger", "palmerpenguins"))) {
#'   # Classification example
#'   data(penguins, package = "palmerpenguins")
#'   penguins <- na.omit(penguins)
#'
#'   set.seed(2847)
#'   rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 3)
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
#'   set.seed(5193)
#'   rf_reg <- ranger::ranger(mpg ~ ., data = mtcars, num.trees = 3)
#'   party_tree_reg <- as.party(rf_reg, tree = 1L, data = mtcars)
#'   print(party_tree_reg)
#' }
#'
#' @export
as.party.ranger <- function(obj, tree = 1L, data = NULL, ...) {
  rlang::check_installed("ranger")

  # Require data parameter
  if (is.null(data)) {
    cli::cli_abort(
      "{.arg data} is required for {.fn as.party.ranger}.",
      "i" = "Provide the training data to create a valid party object with fitted values."
    )
  }

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
      "{.pkg ranger} model must have {.code write.forest = TRUE} to extract trees."
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
  # Data is required, so we can directly validate and select
  response_name <- obj$dependent.variable.name
  orig_data <- validate_and_select_data(
    data,
    var_names,
    preserve_extra = TRUE
  )

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

    fitted <- create_fitted_dataframe(fitted_ids, response)
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

# ------------------------------------------------------------------------------

# Internal helper: extract active predictors for one tree and wrap in constructor
ranger_extract_one <- function(tree_num, x) {
  # Extract tree using treeInfo
  tree_info <- ranger::treeInfo(x, tree = tree_num)

  # Filter to non-terminal nodes
  split_nodes <- tree_info[!tree_info$terminal, ]

  # Extract splitvarName column (already as character)
  active_vars <- split_nodes$splitvarName

  # Return using constructor (handles uniqueness and sorting)
  new_active_predictors(active_vars, tree = tree_num)
}

#' @rdname active_predictors
#' @param tree Integer vector specifying which trees to extract active
#'   predictors from. Default is `1L` for the first tree. Values must be
#'   between 1 and the number of trees in the forest.
#' @export
active_predictors.ranger <- function(x, tree = 1L, ...) {
  rlang::check_installed("ranger")

  # Validate tree argument
  if (!is.numeric(tree) || !all(tree == as.integer(tree))) {
    cli::cli_abort(
      "{.arg tree} must be an integer vector, not {.obj_type_friendly {tree}}.",
      call = rlang::caller_env()
    )
  }

  tree <- as.integer(tree)

  # Check that forest exists
  if (is.null(x$forest)) {
    cli::cli_abort(
      "{.pkg ranger} model must be fitted with {.code write.forest = TRUE} to extract active predictors.",
      call = rlang::caller_env()
    )
  }

  # Validate tree range
  if (any(tree < 1L) || any(tree > x$num.trees)) {
    cli::cli_abort(
      "{.arg tree} values must be between 1 and {x$num.trees}.",
      call = rlang::caller_env()
    )
  }

  # Extract for each tree
  results <- lapply(tree, ranger_extract_one, x = x)

  # Combine and sort by tree
  dplyr::bind_rows(results) |>
    dplyr::arrange(tree)
}

# ------------------------------------------------------------------------------
# Variable importance Wrapper

#' @export
#' @rdname lorax_var_imp
var_imp.ranger <- function(object, complete = TRUE, ...) {
  rlang::check_installed("ranger")

  # Check if variable importance was calculated
  if (is.null(object$variable.importance)) {
    cli::cli_abort(
      c(
        "{.pkg ranger} model must be fitted with {.code importance} parameter to compute variable importance.",
        "i" = "Use {.code importance = 'impurity'} or {.code importance = 'permutation'} when calling {.fn ranger}."
      )
    )
  }

  # Get variable importance from ranger object
  imp <- object$variable.importance

  # Convert to tibble
  res <- tibble::enframe(imp)
  names(res) <- c("term", "estimate")

  if (complete) {
    # Get all predictor names from the model
    pred_names <- object$forest$independent.variable.names
    res <- complete_results(res, pred_names)
  }

  res
}
