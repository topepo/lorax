#' Extract rules from an lgb.Booster model
#'
#' Extract interpretable decision rules from a single tree in a LightGBM
#' boosted tree model. Each terminal node (leaf) becomes one rule representing
#' the path from root to that leaf.
#'
#' @param x An `lgb.Booster` object from the \pkg{lightgbm} package.
#' @param tree Integer specifying which tree to extract rules from. Uses
#'   1-based indexing (default is `1L`). For multiclass models with `num_class`
#'   classes and `nrounds` boosting rounds, there are `num_class * nrounds`
#'   total trees.
#' @param ... Not currently used.
#'
#' @return A tibble with class `c("rule_set_lgb.Booster", "rule_set")` and
#'   columns:
#'   * `tree`: integer, the tree number (matches input parameter).
#'   * `rules`: list of R expressions, one per terminal node.
#'   * `id`: integer, terminal node ID (1-based).
#'
#' @details
#' LightGBM uses 0-based indexing internally, but this function uses 1-based
#' indexing for the `tree` parameter and output `id` column (R convention).
#'
#' Split conditions in LightGBM follow the pattern: left child when feature <=
#' threshold, right child when feature > threshold. Rules are combinations of
#' these conditions using AND logic.
#'
#' Note: This function does not work with LightGBM models containing categorical
#' features.
#'
#' @examples
#' \dontrun{
#' library(lightgbm)
#' data(agaricus.train, package = "lightgbm")
#'
#' dtrain <- lgb.Dataset(agaricus.train$data, label = agaricus.train$label)
#' bst <- lgb.train(
#'   params = list(objective = "binary", max_depth = 3),
#'   data = dtrain,
#'   nrounds = 5
#' )
#'
#' # Extract rules from first tree
#' rules <- extract_rules(bst, tree = 1L)
#'
#' # View as text
#' rule_text(rules$rules[[1]])
#' }
#'
#' @export
extract_rules.lgb.Booster <- function(x, tree = 1L, ...) {
  rlang::check_installed("lightgbm")
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

  # Get tree structure for all trees
  tree_dt_all <- lightgbm::lgb.model.dt.tree(x)
  tree_dt_all <- as.data.frame(tree_dt_all)

  # Validate tree number against available trees
  max_tree_index <- max(tree_dt_all$tree_index)
  if (tree > max_tree_index + 1) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {max_tree_index + 1}, not {tree}."
    )
  }

  # Filter to selected tree (convert to 0-based indexing)
  tree_dt <- tree_dt_all[tree_dt_all$tree_index == (tree - 1L), ]

  # Validate tree exists
  if (nrow(tree_dt) == 0) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {max_tree_index + 1}, not {tree}."
    )
  }

  # Handle single-node tree (root is leaf)
  if (nrow(tree_dt) == 1 && !is.na(tree_dt$leaf_index[1])) {
    return(
      tibble::tibble(
        tree = tree,
        rules = list(rlang::expr(TRUE)),
        id = 1L
      ) |>
        tibble::new_tibble(class = c("rule_set_lgb.Booster", "rule_set"))
    )
  }

  # Get terminal node IDs (0-based in tree_dt)
  terminal_ids <- tree_dt$leaf_index[!is.na(tree_dt$leaf_index)]

  # Build rules for each terminal node
  rules_list <- lapply(terminal_ids, function(leaf_id) {
    # Build path from root to this terminal node
    path <- lgb_build_node_path(leaf_id, tree_dt)

    # Extract split conditions along the path
    split_exprs <- list()
    for (i in seq_along(path)[-length(path)]) {
      parent <- path[i]
      child <- path[i + 1]
      is_child_leaf <- (i == length(path) - 1)
      split_info <- lgb_get_split_info(parent, child, tree_dt, is_child_leaf)
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
    tibble::new_tibble(class = c("rule_set_lgb.Booster", "rule_set"))
}

# Internal helper to build path from root to leaf
lgb_build_node_path <- function(leaf_index, tree_dt) {
  path <- integer()
  current <- leaf_index
  is_leaf <- TRUE

  # Start with the leaf
  path <- c(current, path)

  # Get parent of leaf
  leaf_row <- tree_dt[
    !is.na(tree_dt$leaf_index) & tree_dt$leaf_index == leaf_index,
  ]
  if (nrow(leaf_row) == 0 || is.na(leaf_row$leaf_parent[1])) {
    return(path)
  }

  current <- leaf_row$leaf_parent[1]
  is_leaf <- FALSE

  # Traverse up to root using node_parent
  while (TRUE) {
    path <- c(current, path)

    # Find the internal node row
    node_row <- tree_dt[
      !is.na(tree_dt$split_index) & tree_dt$split_index == current,
    ]

    if (nrow(node_row) == 0 || is.na(node_row$node_parent[1])) {
      # Reached root (node_parent is NA)
      break
    }

    current <- node_row$node_parent[1]
  }

  path
}

# Internal helper to get split information
lgb_get_split_info <- function(
  parent_split_index,
  child_node,
  tree_dt,
  is_child_leaf
) {
  # Get parent row (must be an internal node)
  parent_row <- tree_dt[
    !is.na(tree_dt$split_index) & tree_dt$split_index == parent_split_index,
  ]

  if (nrow(parent_row) == 0) {
    cli::cli_abort("Could not find parent node {parent_split_index}.")
  }

  # Check for categorical splits (not supported)
  decision_type <- parent_row$decision_type[1]
  if (decision_type == "==") {
    cli::cli_abort(
      "Categorical features are not currently supported."
    )
  }

  # Get all children of this parent at the next depth
  parent_depth <- parent_row$depth[1]
  children_internal <- tree_dt[
    !is.na(tree_dt$split_index) &
      tree_dt$node_parent == parent_split_index &
      tree_dt$depth == parent_depth + 1,
  ]
  children_leaves <- tree_dt[
    !is.na(tree_dt$leaf_index) &
      tree_dt$leaf_parent == parent_split_index &
      tree_dt$depth == parent_depth + 1,
  ]

  # Combine and get IDs
  all_child_ids <- c(
    children_internal$split_index,
    children_leaves$leaf_index
  )

  # Left child is the one with smaller ID
  is_left_child <- child_node == min(all_child_ids, na.rm = TRUE)

  # Numerical split (decision_type == "<=")
  # LightGBM convention: left child when feature <= threshold
  threshold <- parent_row$threshold[1]
  feature <- parent_row$split_feature[1]

  if (is_left_child) {
    operator <- "<="
  } else {
    operator <- ">"
  }

  list(
    column = feature,
    value = threshold,
    operator = operator
  )
}

# ------------------------------------------------------------------------------

#' Convert lgb.Booster model to party object
#'
#' Convert a single tree from a LightGBM boosted tree model to a party object
#' for use with partykit visualization and analysis tools.
#'
#' @param obj An `lgb.Booster` object from the \pkg{lightgbm} package.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). For multiclass models with `num_class` classes and
#'   `nrounds` boosting rounds, there are `num_class * nrounds` total trees.
#' @param data data.frame containing the training data **with the response
#'   variable included** (required). LightGBM models do not store the original
#'   training data or response values. You must provide the original data frame
#'   that includes both the predictor variables and the response variable.
#' @param ... Not currently used.
#'
#' @return A `constparty` object from the \pkg{partykit} package.
#'
#' @details
#' ## Important note on data
#'
#' LightGBM models do not store the original training data or response values.
#' You **must** provide the original data frame (including the response variable)
#' via the `data` parameter for correct terminal node statistics, bar charts,
#' and other visualizations.
#'
#' ## LightGBM tree storage format
#'
#' LightGBM stores trees in a tabular format accessible via
#' `lightgbm::lgb.model.dt.tree()`. Each tree is represented as rows in a table:
#' - `tree_index`: 0-based tree index
#' - `split_index`: 0-based node ID for internal nodes (NA for leaves)
#' - `leaf_index`: 0-based node ID for leaf nodes (NA for internal)
#' - `split_feature`: Feature name (character) for splits
#' - `threshold`: Numeric threshold for splits
#' - `decision_type`: Split type ("<=", "==", etc.)
#' - `left_child`: 0-based node ID of left child
#' - `right_child`: 0-based node ID of right child
#' - `leaf_value`: Prediction value for leaf nodes
#' - `node_parent`: 0-based parent node ID
#' - `depth`: Depth of node in tree
#'
#' ## Node indexing
#'
#' - Internally, LightGBM uses 0-based tree and node indices
#' - User-facing `tree` parameter uses 1-based indexing (R convention)
#' - When tree=1 is requested, we filter to tree_index==0 internally
#' - Internal nodes use split_index, leaf nodes use leaf_index
#'
#' ## Split encoding
#'
#' - decision_type "<=": left child when feature <= threshold
#' - right child when feature > threshold
#' - partykit split created with `right = FALSE` (left interval closed)
#'
#' ## Child node references
#'
#' - Internal nodes have explicit left_child and right_child IDs
#' - These reference either split_index (internal) or leaf_index (leaf)
#' - Need to look up child in appropriate column based on node type
#'
#' ## Variable names
#'
#' - split_feature column contains actual feature names or "Column_N" defaults
#' - Must map to column positions in data.frame
#'
#' The party object will use 1-based node IDs and variable indices as required
#' by partykit.
#'
#' @examples
#' if (rlang::is_installed("lightgbm")) {
#'   data(agaricus.train, package = "lightgbm")
#'
#'   # Prepare data with response column
#'   train_data <- as.data.frame(as.matrix(agaricus.train$data))
#'   train_data$label <- agaricus.train$label
#'
#'   dtrain <- lightgbm::lgb.Dataset(agaricus.train$data, label = agaricus.train$label)
#'
#'   bst <- lightgbm::lgb.train(
#'     params = list(objective = "binary", max_depth = 3),
#'     data = dtrain,
#'     nrounds = 5,
#'     verbose = -1
#'   )
#'
#'   # Convert first tree - data parameter is required
#'   party_tree <- as.party(bst, tree = 1L, data = train_data)
#'   print(party_tree)
#'   plot(party_tree)
#' }
#'
#' @export
as.party.lgb.Booster <- function(obj, tree = 1L, data, ...) {
  rlang::check_installed("lightgbm")
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

  # Extract all trees using lightgbm API
  tree_dt <- lightgbm::lgb.model.dt.tree(obj)

  # Convert to data.frame to avoid data.table syntax
  tree_dt <- as.data.frame(tree_dt)

  # Validate tree number against available trees
  # tree_index column uses 0-based indexing
  if (nrow(tree_dt) == 0 || all(is.na(tree_dt$tree_index))) {
    cli::cli_abort(
      "Model does not contain any trees."
    )
  }

  max_tree_internal <- max(tree_dt$tree_index, na.rm = TRUE)
  num_trees <- max_tree_internal + 1L # Convert to 1-based count
  if (tree > num_trees) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {num_trees}, not {tree}."
    )
  }

  # Filter to selected tree (convert 1-based input to 0-based internal)
  tree_internal <- tree - 1L
  tree_df <- tree_dt[tree_dt$tree_index == tree_internal, ]

  if (nrow(tree_df) == 0) {
    cli::cli_abort(
      "Tree {tree} not found in model."
    )
  }

  # Get feature names from ALL trees (not just selected tree)
  # This ensures we have the complete feature set
  feature_names <- unique(tree_dt$split_feature[!is.na(tree_dt$split_feature)])
  # Handle LightGBM default names "Column_0", "Column_1", etc.
  if (
    length(feature_names) > 0 && all(grepl("^Column_[0-9]+$", feature_names))
  ) {
    # Extract indices and create generic names
    n_features <- length(feature_names)
    var_names <- paste0("X", seq_len(n_features))
  } else if (length(feature_names) > 0) {
    var_names <- feature_names
  } else {
    # No features found (e.g., single-node tree with no splits)
    var_names <- character(0)
  }

  # Handle single-node tree (root is leaf)
  if (nrow(tree_df) == 1 && !is.na(tree_df$leaf_index[1])) {
    root_node <- partykit::partynode(id = 1L)
  } else {
    # Find root node (node_parent is NA)
    root_row <- tree_df[is.na(tree_df$node_parent), ]
    if (nrow(root_row) == 0) {
      # Fallback: root is at depth 0
      root_row <- tree_df[tree_df$depth == 0, ]
    }

    if (nrow(root_row) == 0) {
      cli::cli_abort("Cannot find root node in tree structure.")
    }

    # Root node is identified by split_index (if internal) or leaf_index (if leaf)
    if (!is.na(root_row$split_index[1])) {
      root_id <- root_row$split_index[1]
      is_root_leaf <- FALSE
    } else {
      root_id <- root_row$leaf_index[1]
      is_root_leaf <- TRUE
    }

    # Build partynode structure recursively from root
    root_node <- lgb_build_partynode(
      node_id = root_id,
      is_leaf = is_root_leaf,
      tree_df = tree_df,
      var_names = var_names
    )

    # Assign sequential IDs
    root_node <- assign_node_ids(root_node)$node
  }

  # Validate and extract data
  # Map LightGBM feature names to data columns
  if (all(grepl("^Column_[0-9]+$", feature_names))) {
    # LightGBM used default names, use first n columns
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

  fitted <- data.frame(
    "(fitted)" = fitted_ids,
    "(response)" = response,
    check.names = FALSE
  )

  # Create constparty object
  party_obj <- create_party_object(
    node = root_node,
    data = orig_data,
    fitted = fitted,
    terms = terms,
    info = list(method = "lightgbm", tree = tree)
  )

  class(party_obj) <- c("constparty", "party")

  party_obj
}

# Internal helper to recursively build partynode from LightGBM tree table
#
# @param node_id Current node ID (0-based, LightGBM convention)
# @param is_leaf Logical, whether current node is a leaf
# @param tree_df Data.frame with tree structure from lgb.model.dt.tree
# @param var_names Character vector of variable names
#
# @return A partynode object
lgb_build_partynode <- function(node_id, is_leaf, tree_df, var_names) {
  # Find current node's row
  if (is_leaf) {
    node_row <- tree_df[
      !is.na(tree_df$leaf_index) & tree_df$leaf_index == node_id,
    ]
  } else {
    node_row <- tree_df[
      !is.na(tree_df$split_index) & tree_df$split_index == node_id,
    ]
  }

  if (nrow(node_row) == 0) {
    cli::cli_abort(
      "Node {node_id} (leaf={is_leaf}) not found in tree structure."
    )
  }

  # Use first matching row if duplicates
  node_row <- node_row[1, ]

  # Check if terminal node
  if (is_leaf) {
    # Terminal node - no info for constparty
    return(partykit::partynode(id = 1L))
  }

  # Internal node - get split info
  feature_name <- node_row$split_feature
  split_val <- as.numeric(node_row$threshold)
  decision_type <- node_row$decision_type

  # Validate split information
  if (is.na(feature_name) || is.na(split_val) || length(split_val) == 0) {
    cli::cli_abort(
      "Internal node {node_id} has invalid split information (feature={feature_name}, threshold={split_val})."
    )
  }

  # Check for categorical splits (not fully supported)
  if (!is.na(decision_type) && decision_type == "==") {
    cli::cli_abort(
      "Categorical features are not currently supported."
    )
  }

  # Map feature name to variable index (1-based)
  varid <- which(var_names == feature_name)
  if (length(varid) == 0) {
    # Try matching LightGBM default names to generic names
    if (grepl("^Column_[0-9]+$", feature_name)) {
      feature_idx <- as.integer(sub("^Column_", "", feature_name))
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
  # LightGBM default: decision_type "<=", left child when feature <= threshold
  # This requires right = FALSE in partykit (left interval closed)
  split <- build_partysplit(varid, split_val, right = FALSE)

  # Find children of this node
  # Children have either node_parent or leaf_parent equal to current split_index
  # Internal children have node_parent == node_id
  child_splits <- tree_df[
    !is.na(tree_df$split_index) &
      !is.na(tree_df$node_parent) &
      tree_df$node_parent == node_id,
  ]
  # Leaf children have leaf_parent == node_id
  child_leaves <- tree_df[
    !is.na(tree_df$leaf_index) &
      !is.na(tree_df$leaf_parent) &
      tree_df$leaf_parent == node_id,
  ]

  # Combine and sort by depth (left child should appear first)
  children <- rbind(child_splits, child_leaves)
  if (nrow(children) != 2) {
    cli::cli_abort(
      "Expected 2 children for split node {node_id}, found {nrow(children)}."
    )
  }

  # Sort by row order (LightGBM stores left child first)
  # Actually, sort by depth then by row order in original dataframe
  children_row_nums <- as.integer(rownames(children))
  children <- children[order(children_row_nums), ]

  # First child is left, second is right
  left_child <- children[1, ]
  right_child <- children[2, ]

  # Determine if each child is a leaf
  left_is_leaf <- !is.na(left_child$leaf_index)
  right_is_leaf <- !is.na(right_child$leaf_index)

  # Get child IDs
  left_child_id <- if (left_is_leaf) {
    left_child$leaf_index
  } else {
    left_child$split_index
  }
  right_child_id <- if (right_is_leaf) {
    right_child$leaf_index
  } else {
    right_child$split_index
  }

  # Recursively build children
  left_node <- lgb_build_partynode(
    left_child_id,
    left_is_leaf,
    tree_df,
    var_names
  )
  right_node <- lgb_build_partynode(
    right_child_id,
    right_is_leaf,
    tree_df,
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
