#' Extract rules from an xgb.Booster model
#'
#' Extract interpretable decision rules from a single tree in an XGBoost
#' boosted tree model. Each terminal node (leaf) becomes one rule representing
#' the path from root to that leaf.
#'
#' @param x An `xgb.Booster` object from the xgboost package.
#' @param tree Integer specifying which tree to extract rules from. Uses
#'   1-based indexing (default is 1). For multiclass models with `num_class`
#'   classes and `nrounds` boosting rounds, there are `num_class * nrounds`
#'   total trees.
#' @param ... Not currently used.
#'
#' @return A tibble with class `c("rule_set_xgb.Booster", "rule_set")` and
#'   columns:
#'   * `tree`: integer, the tree number (matches input parameter)
#'   * `rules`: list of R expressions, one per terminal node
#'   * `id`: integer, terminal node ID (1-based)
#'
#' @details
#' XGBoost uses 0-based indexing internally, but this function uses 1-based
#' indexing for the `tree` parameter and output `id` column (R convention).
#'
#' Split conditions in XGBoost follow the pattern: Yes branch when feature <
#' threshold, No branch when feature >= threshold. Rules are combinations of
#' these conditions using AND logic.
#'
#' Note: This function does not work with XGBoost models containing categorical
#' features or non-tree boosters (gblinear).
#'
#' @examples
#' \dontrun{
#' library(xgboost)
#' data(agaricus.train, package = "xgboost")
#'
#' bst <- xgb.train(
#'   data = xgb.DMatrix(agaricus.train$data, label = agaricus.train$label),
#'   nrounds = 5,
#'   max_depth = 3,
#'   objective = "binary:logistic"
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
extract_rules.xgb.Booster <- function(x, tree = 1L, ...) {
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
    dplyr::arrange(.data$id) |>
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
