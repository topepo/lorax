#' Extract rules from an xgb.Booster model
#'
#' Extract interpretable decision rules from a single tree in an \pkg{xgboost}
#' boosted tree model. Each terminal node (leaf) becomes one rule representing
#' the path from root to that leaf.
#'
#' @param x An `xgb.Booster` object from the \pkg{xgboost} package.
#' @param tree Integer specifying which tree to extract rules from. Uses
#'   1-based indexing (default is `1L`). For multiclass models with `num_class`
#'   classes and `nrounds` boosting rounds, there are `num_class * nrounds`
#'   total trees.
#' @param ... Not currently used.
#'
#' @return A tibble with class `c("rule_set_xgb.Booster", "rule_set")` and
#'   columns:
#'   * `tree`: integer, the tree number (matches input parameter).
#'   * `rules`: list of R expressions, one per terminal node.
#'   * `id`: integer, terminal node ID (1-based).
#'
#' @details
#' \pkg{xgboost} uses 0-based indexing internally, but this function uses 1-based
#' indexing for the `tree` parameter and output `id` column (R convention).
#'
#' Split conditions in \pkg{xgboost} follow the pattern: Yes branch when feature <
#' threshold, No branch when feature >= threshold. Rules are combinations of
#' these conditions using AND logic.
#'
#' Note: This function does not work with \pkg{xgboost} models containing categorical
#' features or non-tree boosters (`gblinear`).
#'
#' @examples
#' if (rlang::is_installed("xgboost")) {
#'   data(agaricus.train, package = "xgboost")
#'
#'   # Binary classification
#'   set.seed(2847)
#'   bst <- xgboost::xgb.train(
#'     data = xgboost::xgb.DMatrix(agaricus.train$data, label = agaricus.train$label),
#'     nrounds = 3,
#'     max_depth = 3,
#'     objective = "binary:logistic"
#'   )
#'
#' # Extract rules from first tree
#' rules <- extract_rules(bst, tree = 1L)
#'
#' # View as text
#' rule_text(rules$rules[[1]])
#'
#'   # Regression example
#'   data(mtcars)
#'   set.seed(8472)
#'   bst_reg <- xgboost::xgb.train(
#'     data = xgboost::xgb.DMatrix(as.matrix(mtcars[, -1]), label = mtcars$mpg),
#'     nrounds = 3,
#'     max_depth = 3,
#'     objective = "reg:squarederror"
#'   )
#'   rules_reg <- extract_rules(bst_reg, tree = 1L)
#' }
#'
#' @export
extract_rules.xgb.Booster <- function(x, tree = 1L, ...) {
  rlang::check_installed("xgboost")
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

  # Get tree structure
  # Note: trees parameter uses 1-based indexing (R convention)
  # but the Tree column in output uses 0-based indexing (xgboost convention)
  tree_dt <- xgboost::xgb.model.dt.tree(
    model = x,
    trees = tree,
    use_int_id = TRUE
  )

  # Convert to data.frame for easier indexing (avoid data.table syntax)
  tree_dt <- as.data.frame(tree_dt)

  # Validate tree number against available trees
  if (nrow(tree_dt) == 0) {
    max_tree_num <- max(
      as.data.frame(xgboost::xgb.model.dt.tree(x, use_int_id = TRUE))$Tree
    )
    cli::cli_abort(
      "{.arg tree} must be between 1 and {max_tree_num + 1}, not {tree}."
    )
  }

  # Handle single-node tree (root is leaf)
  if (nrow(tree_dt) == 1 && tree_dt$Feature[1] == "Leaf") {
    return(
      tibble::tibble(
        tree = tree,
        rules = list(rlang::expr(TRUE)),
        id = 1L
      ) |>
        tibble::new_tibble(class = c("rule_set_xgb.Booster", "rule_set"))
    )
  }

  # Get terminal node IDs (0-based in tree_dt)
  terminal_ids <- tree_dt$Node[tree_dt$Feature == "Leaf"]

  # Build rules for each terminal node
  rules_list <- lapply(terminal_ids, function(node_id) {
    # Build path from root (0) to this terminal node
    path <- xgb_build_node_path(node_id, tree_dt)

    # Extract split conditions along the path
    split_exprs <- list()
    for (i in seq_along(path)[-length(path)]) {
      parent <- path[i]
      child <- path[i + 1]
      split_info <- xgb_get_split_info(parent, child, tree_dt)
      split_exprs[[i]] <- rect_split_to_expr(split_info)
    }

    combine_rule_elements(split_exprs)
  })

  # Return tibble with 1-based IDs
  tibble::tibble(
    tree = tree,
    rules = rules_list,
    id = as.integer(terminal_ids) + 1L
  ) |>
    dplyr::arrange(id) |>
    tibble::new_tibble(class = c("rule_set_xgb.Booster", "rule_set"))
}

# Internal helper to build path from root to node
xgb_build_node_path <- function(node_id, tree_dt) {
  path <- integer()
  current <- node_id

  while (TRUE) {
    path <- c(current, path)
    if (current == 0) {
      break
    }
    current <- xgb_find_parent(current, tree_dt)
    if (is.na(current)) {
      break
    }
  }

  path
}

# Internal helper to find parent node
xgb_find_parent <- function(node_id, tree_dt) {
  if (node_id == 0) {
    return(NA_integer_)
  }

  # Find row where Yes or No column matches node_id
  parent_row <- which(tree_dt$Yes == node_id | tree_dt$No == node_id)

  if (length(parent_row) == 0) {
    return(NA_integer_)
  }

  tree_dt$Node[parent_row[1]]
}

# Internal helper to get split information
xgb_get_split_info <- function(parent_id, child_id, tree_dt) {
  # Get parent row
  parent_row <- tree_dt[tree_dt$Node == parent_id, ]

  # Determine operator based on branch direction
  # XGBoost convention: Yes branch when feature < threshold
  if (child_id == parent_row$Yes) {
    operator <- "<"
  } else {
    operator <- ">="
  }

  list(
    column = parent_row$Feature,
    value = parent_row$Split,
    operator = operator
  )
}

# ------------------------------------------------------------------------------

#' Convert xgb.Booster model to party object
#'
#' Convert a single tree from an \pkg{xgboost} boosted tree model to a party object
#' for use with \pkg{partykit} visualization and analysis tools.
#'
#' @param obj An `xgb.Booster` object from the \pkg{xgboost} package.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). For multiclass models with `num_class` classes and
#'   `nrounds` boosting rounds, there are `num_class * nrounds` total trees.
#' @param data data.frame containing the training data **with the response
#'   variable included** (required). XGBoost models do not store the original
#'   training data or response values. You must provide the original data frame
#'   that includes both the predictor variables and the response variable.
#' @param ... Not currently used.
#'
#' @return A `constparty` object from the \pkg{partykit} package.
#'
#' @details
#' ## Important note on data
#'
#' XGBoost models do not store the original training data or response values.
#' You **must** provide the original data frame (including the response variable)
#' via the `data` parameter for correct terminal node statistics, bar charts,
#' and other visualizations.
#'
#' ## XGBoost tree storage format
#'
#' \pkg{xgboost} stores trees in a tabular format accessible via
#' `xgboost::xgb.model.dt.tree()`. Each tree is represented as rows in a table:
#' - `Tree`: 0-based tree index (e.g., 0, 1, 2,  ...)
#' - `Node`: 0-based node ID within tree (e.g., "0-0", "0-1" for tree 0)
#' - `Feature`: Feature name (character) or "Leaf" for terminal nodes
#' - `Split`: Numeric threshold for splits (NA for leaves)
#' - `Yes`: 0-based node ID of yes branch (feature < threshold)
#' - `No`: 0-based node ID of no branch (feature >= threshold)
#' - `Missing`: 0-based node ID for missing values
#' - `Quality`: Prediction value for leaf nodes, gain for internal nodes
#'
#' ## Node indexing
#'
#' - Internally, \pkg{xgboost} uses 0-based tree and node indices
#' - User-facing `tree` parameter uses 1-based indexing (R convention)
#' - When `tree=1` is requested, we filter to `Tree==0` internally
#'
#' ## Split encoding
#'
#' - Yes branch: feature < threshold (left child)
#' - No branch: feature >= threshold (right child)
#' - \pkg{partykit} split created with `right = TRUE` (right interval closed)
#'
#' ## Child node references
#'
#' - `Yes` column: node ID for left child (< condition)
#' - `No` column: node ID for right child (>= condition)
#' - Leaf nodes have `Feature == "Leaf"`
#'
#' ## Variable names
#'
#' - `Feature` column contains actual feature names (not indices)
#' - Must map to column positions in data.frame
#' - If numeric indices used (`f0`, `f1`, ...), map to data columns
#'
#' The party object will use 1-based node IDs and variable indices as required
#' by \pkg{partykit}.
#'
#' @examples
#' if (rlang::is_installed("xgboost")) {
#'   data(agaricus.train, package = "xgboost")
#'
#'   # Binary classification example
#'   train_data <- as.data.frame(as.matrix(agaricus.train$data))
#'   train_data$label <- agaricus.train$label
#'
#'   dtrain <- xgboost::xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
#'
#'   set.seed(3691)
#'   bst <- xgboost::xgb.train(
#'     data = dtrain,
#'     max_depth = 3,
#'     nrounds = 3,
#'     objective = "binary:logistic",
#'     verbose = 0
#'   )
#'
#'   # Convert first tree - data parameter is required
#'   party_tree <- as.party(bst, tree = 1L, data = train_data)
#'   print(party_tree)
#'   plot(party_tree)
#'
#'   # Regression example
#'   data(mtcars)
#'   reg_data <- mtcars
#'   dtrain_reg <- xgboost::xgb.DMatrix(as.matrix(mtcars[, -1]), label = mtcars$mpg)
#'
#'   set.seed(9158)
#'   bst_reg <- xgboost::xgb.train(
#'     data = dtrain_reg,
#'     max_depth = 3,
#'     nrounds = 3,
#'     objective = "reg:squarederror",
#'     verbose = 0
#'   )
#'
#'   party_tree_reg <- as.party(bst_reg, tree = 1L, data = reg_data)
#'   print(party_tree_reg)
#' }
#'
#' @export
as.party.xgb.Booster <- function(obj, tree = 1L, data, ...) {
  rlang::check_installed("xgboost")
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

  # Extract all trees using xgboost API
  # This returns a data.table/data.frame with all trees
  tree_dt <- xgboost::xgb.model.dt.tree(
    model = obj,
    use_int_id = TRUE
  )

  # Convert to data.frame to avoid data.table syntax
  tree_dt <- as.data.frame(tree_dt)

  # Validate tree number against available trees
  # Tree column uses 0-based indexing
  max_tree_internal <- max(tree_dt$Tree)
  num_trees <- max_tree_internal + 1L # Convert to 1-based count
  if (tree > num_trees) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {num_trees}, not {tree}."
    )
  }

  # Filter to selected tree (convert 1-based input to 0-based internal)
  tree_internal <- tree - 1L
  tree_df <- tree_dt[tree_dt$Tree == tree_internal, ]

  if (nrow(tree_df) == 0) {
    cli::cli_abort(
      "Tree {tree} not found in model."
    )
  }

  # Get feature names from ALL trees (not just selected tree)
  # This ensures we have the complete feature set
  feature_names <- unique(tree_dt$Feature[tree_dt$Feature != "Leaf"])
  # Filter out numeric-only feature names like "f0", "f1"
  # These are default XGBoost names when no feature names provided
  if (length(feature_names) > 0 && all(grepl("^f[0-9]+$", feature_names))) {
    # Use generic names but extract count
    n_features <- length(feature_names)
    var_names <- paste0("X", seq_len(n_features))
  } else if (length(feature_names) > 0) {
    var_names <- feature_names
  } else {
    # No features found (e.g., single-node tree with no splits)
    var_names <- character(0)
  }

  # Handle single-node tree (root is leaf)
  if (nrow(tree_df) == 1 && tree_df$Feature[1] == "Leaf") {
    root_node <- partykit::partynode(id = 1L)
  } else {
    # Build partynode structure recursively from root node (Node == 0)
    root_node <- xgb_build_partynode(
      node_id = 0,
      tree_df = tree_df,
      var_names = var_names
    )

    # Assign sequential IDs
    root_node <- assign_node_ids(root_node)$node
  }

  # Validate and extract data
  # Map XGBoost feature names to data columns
  if (all(grepl("^f[0-9]+$", feature_names))) {
    # XGBoost used default names f0, f1, .. use first n columns
    if (ncol(data) < length(var_names)) {
      cli::cli_abort(
        "{.arg data} must have at least {length(var_names)} columns."
      )
    }
    orig_data <- data[, seq_len(length(var_names)), drop = FALSE]
    colnames(orig_data) <- var_names
    # Preserve any additional columns beyond features (e.g., response)
    if (ncol(data) > length(var_names)) {
      extra_cols <- names(data)[(length(var_names) + 1):ncol(data)]
      for (col in extra_cols) {
        orig_data[[col]] <- data[[col]]
      }
      response_cols <- extra_cols
    } else {
      response_cols <- character(0)
    }
  } else {
    # Use actual feature names
    if (!all(feature_names %in% names(data))) {
      missing <- setdiff(feature_names, names(data))
      cli::cli_abort(
        "{.arg data} must contain features: {.field {missing}}."
      )
    }
    # Select feature columns
    orig_data <- data[, var_names, drop = FALSE]
    # Preserve any additional columns beyond features (e.g., response)
    response_cols <- setdiff(names(data), var_names)
    for (col in response_cols) {
      orig_data[[col]] <- data[[col]]
    }
  }

  # Check that response is included
  if (length(response_cols) == 0) {
    cli::cli_abort(
      "{.arg data} must contain the response variable in addition to predictors."
    )
  }

  # Use first non-predictor column as response
  response_name <- response_cols[1]

  # Create terms object
  if (length(var_names) > 0) {
    # Quote variable names with backticks to handle special characters
    quoted_names <- paste0("`", var_names, "`")
    formula <- stats::as.formula(paste(
      "~",
      paste(quoted_names, collapse = " + ")
    ))
    terms <- stats::terms(formula, data = orig_data)
  } else {
    # No features - single-node tree
    terms <- NULL
  }

  # Create fitted values structure
  # Compute which terminal node each observation belongs to
  fitted_ids <- compute_fitted_node_ids(
    root_node,
    orig_data[, var_names, drop = FALSE]
  )

  # Get response (will be in original format)
  response <- orig_data[[response_name]]

  fitted <- create_fitted_dataframe(fitted_ids, response)

  # Create constparty object
  party_obj <- create_party_object(
    node = root_node,
    data = orig_data,
    fitted = fitted,
    terms = terms,
    info = list(method = "xgboost", tree = tree)
  )

  class(party_obj) <- c("constparty", "party")

  party_obj
}

# Internal helper to recursively build partynode from XGBoost tree table
#
# @param node_id Current node ID (0-based, XGBoost convention)
# @param tree_df Data.frame with tree structure from xgb.model.dt.tree
# @param var_names Character vector of variable names
#
# @return A partynode object
xgb_build_partynode <- function(node_id, tree_df, var_names) {
  # Find current node's row
  node_row <- tree_df[tree_df$Node == node_id, ]

  if (nrow(node_row) == 0) {
    cli::cli_abort("Node {node_id} not found in tree structure.")
  }

  # Use first matching row if duplicates
  node_row <- node_row[1, ]

  # Check if terminal node
  if (node_row$Feature == "Leaf") {
    # Terminal node - no info for constparty
    return(partykit::partynode(id = 1L))
  }

  # Internal node - get split info
  feature_name <- node_row$Feature
  split_val <- as.numeric(node_row$Split)

  # Map feature name to variable index (1-based)
  varid <- which(var_names == feature_name)
  if (length(varid) == 0) {
    # Try matching XGBoost default names to generic names
    if (grepl("^f[0-9]+$", feature_name)) {
      feature_idx <- as.integer(sub("^f", "", feature_name))
      varid <- feature_idx + 1L # Convert 0-based to 1-based
    } else {
      cli::cli_abort(
        "Feature {feature_name} not found in variable names."
      )
    }
  }

  if (varid < 1 || varid > length(var_names)) {
    cli::cli_abort(
      "Invalid variable index {varid} for feature {feature_name}."
    )
  }

  # Create split object
  # XGBoost: Yes branch when feature < threshold, No branch when feature >= threshold
  split <- build_partysplit(varid, split_val, right = TRUE)

  # Get child node IDs (0-based)
  yes_child_id <- node_row$Yes # Left child (< condition)
  no_child_id <- node_row$No # Right child (>= condition)

  # Recursively build children
  left_node <- xgb_build_partynode(yes_child_id, tree_df, var_names)
  right_node <- xgb_build_partynode(no_child_id, tree_df, var_names)

  # Return internal node
  partykit::partynode(
    id = 1L, # Will be updated by assign_node_ids
    split = split,
    kids = list(left_node, right_node),
    info = list(node_id = node_id)
  )
}
