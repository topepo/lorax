#' Extract rules from an lgb.Booster model
#'
#' Extract interpretable decision rules from a single tree in a LightGBM
#' boosted tree model. Each terminal node (leaf) becomes one rule representing
#' the path from root to that leaf.
#'
#' @param x An `lgb.Booster` object from the lightgbm package.
#' @param tree Integer specifying which tree to extract rules from. Uses
#'   1-based indexing (default is 1). For multiclass models with `num_class`
#'   classes and `nrounds` boosting rounds, there are `num_class * nrounds`
#'   total trees.
#' @param ... Not currently used.
#'
#' @return A tibble with class `c("rule_set_lgb.Booster", "rule_set")` and
#'   columns:
#'   * `tree`: integer, the tree number (matches input parameter)
#'   * `rules`: list of R expressions, one per terminal node
#'   * `id`: integer, terminal node ID (1-based)
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
#' rules <- extract_rules(bst, tree = 1)
#'
#' # View as text
#' rule_text(rules$rules[[1]])
#' }
#'
#' @export
extract_rules.lgb.Booster <- function(x, tree = 1L, ...) {
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
    dplyr::arrange(.data$id) |>
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
