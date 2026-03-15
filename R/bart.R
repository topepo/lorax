#' Extract rules from a BART model
#'
#' Extract interpretable decision rules from a single tree in a BART
#' (Bayesian Additive Regression Trees) model. Each terminal node (leaf)
#' becomes one rule representing the path from root to that leaf.
#'
#' @param x A `bart` object from the dbarts package fitted with
#'   `keeptrees = TRUE`.
#' @param tree Integer specifying which tree to extract rules from. Uses
#'   1-based indexing (default is 1). BART models contain `n.trees` trees
#'   in the ensemble.
#' @param chain Integer specifying which MCMC chain to extract from. Uses
#'   1-based indexing (default is 1). Only relevant for models fitted with
#'   multiple chains.
#' @param ... Not currently used.
#'
#' @return A tibble with class `c("rule_set_bart", "rule_set")` and
#'   columns:
#'   * `tree`: integer, the tree number (matches input parameter)
#'   * `rules`: list of R expressions, one per terminal node
#'   * `id`: integer, terminal node ID (1-based)
#'
#' @details
#' The BART model must be fitted with `keeptrees = TRUE` to enable tree
#' extraction. This function uses 1-based indexing for the `tree` parameter
#' and output `id` column (R convention).
#'
#' Split conditions in BART follow the pattern: left child when feature <
#' threshold, right child when feature >= threshold. Rules are combinations
#' of these conditions using AND logic.
#'
#' @examples
#' \dontrun{
#' library(dbarts)
#'
#' fit <- bart(
#'   y ~ .,
#'   data = data,
#'   keeptrees = TRUE,
#'   verbose = FALSE
#' )
#'
#' # Extract rules from first tree
#' rules <- extract_rules(fit, tree = 1)
#'
#' # View as text
#' rule_text(rules$rules[[1]])
#' }
#'
#' @export
extract_rules.bart <- function(x, tree = 1L, chain = 1L, ...) {
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

  # Validate chain parameter
  if (!is.numeric(chain) || length(chain) != 1 || chain != as.integer(chain)) {
    cli::cli_abort(
      "{.arg chain} must be a single integer, not {.obj_type_friendly {chain}}."
    )
  }
  chain <- as.integer(chain)

  if (chain < 1) {
    cli::cli_abort(
      "{.arg chain} must be >= 1, not {chain}."
    )
  }

  # Check that model was fitted with keeptrees = TRUE
  if (is.null(x$fit)) {
    cli::cli_abort(
      "BART model must be fitted with {.code keeptrees = TRUE} to extract rules."
    )
  }

  # Extract trees directly from fit object
  trees_df <- x$fit$getTrees()

  # Validate chain against available chains
  if ("chain" %in% names(trees_df)) {
    max_chain <- max(trees_df$chain)
    if (chain > max_chain) {
      cli::cli_abort(
        "{.arg chain} must be between 1 and {max_chain}, not {chain}."
      )
    }
  } else {
    # Single chain model - only chain 1 is valid
    if (chain != 1) {
      cli::cli_abort(
        "{.arg chain} must be 1 for single-chain models, not {chain}."
      )
    }
  }

  # Validate tree against available trees
  max_tree <- max(trees_df$tree)
  if (tree > max_tree) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {max_tree}, not {tree}."
    )
  }

  # Filter to selected tree and chain
  if ("chain" %in% names(trees_df)) {
    tree_df <- trees_df[trees_df$tree == tree & trees_df$chain == chain, ]
  } else {
    tree_df <- trees_df[trees_df$tree == tree, ]
  }

  # If multiple samples exist, use most recent
  if ("sample" %in% names(tree_df)) {
    tree_df <- tree_df[tree_df$sample == max(tree_df$sample), ]
  }

  # Handle single-node tree (root is leaf)
  if (nrow(tree_df) == 1 && tree_df$var[1] == -1) {
    return(
      tibble::tibble(
        tree = tree,
        rules = list(rlang::expr(TRUE)),
        id = 1L
      ) |>
        tibble::new_tibble(class = c("rule_set_bart", "rule_set"))
    )
  }

  # Rebuild tree hierarchy
  tree_hierarchy <- bart_rebuild_tree(tree_df, x)

  # Get terminal node row indices
  terminal_rows <- which(tree_df$var == -1)

  # Build rules for each terminal node
  rules_list <- lapply(terminal_rows, function(leaf_row) {
    # Build path from root to this terminal node
    path <- bart_build_node_path(leaf_row, tree_hierarchy)

    # Extract split conditions along the path
    split_exprs <- list()
    for (i in seq_along(path)[-length(path)]) {
      parent_row <- path[i]
      child_row <- path[i + 1]
      split_info <- bart_get_split_info(
        parent_row,
        child_row,
        tree_df,
        x,
        tree_hierarchy
      )
      split_exprs[[i]] <- rect_split_to_expr(split_info)
    }

    combine_rule_elements(split_exprs)
  })

  # Return tibble with 1-based sequential IDs
  tibble::tibble(
    tree = tree,
    rules = rules_list,
    id = seq_along(terminal_rows)
  ) |>
    dplyr::arrange(.data$id) |>
    tibble::new_tibble(class = c("rule_set_bart", "rule_set"))
}

# Internal helper to rebuild tree hierarchy from depth-first format
bart_rebuild_tree <- function(tree_df, x) {
  bart_rebuild_tree_recurse <- function(tree_df, start_row) {
    if (start_row > nrow(tree_df)) {
      return(NULL)
    }

    node <- list(
      row = start_row,
      var = tree_df$var[start_row],
      value = tree_df$value[start_row],
      n_nodes = 1L
    )

    # Check if node is a leaf
    if (tree_df$var[start_row] == -1) {
      return(node)
    }

    # Recurse down left branch (immediately after current node)
    left <- bart_rebuild_tree_recurse(tree_df, start_row + 1L)
    n_nodes_left <- if (is.null(left)) 0L else left$n_nodes
    node$left <- left

    # Recurse down right branch (after left subtree)
    right <- bart_rebuild_tree_recurse(tree_df, start_row + 1L + n_nodes_left)
    n_nodes_right <- if (is.null(right)) 0L else right$n_nodes
    node$right <- right

    node$n_nodes <- 1L + n_nodes_left + n_nodes_right

    return(node)
  }

  bart_rebuild_tree_recurse(tree_df, 1L)
}

# Internal helper to build path from root to leaf
bart_build_node_path <- function(target_row, tree_hierarchy) {
  path <- integer()

  bart_build_path_recurse <- function(node, current_path) {
    if (is.null(node)) {
      return(NULL)
    }

    new_path <- c(current_path, node$row)

    # Check if we've reached the target
    if (node$row == target_row) {
      return(new_path)
    }

    # Try left subtree
    if (!is.null(node$left)) {
      result <- bart_build_path_recurse(node$left, new_path)
      if (!is.null(result)) {
        return(result)
      }
    }

    # Try right subtree
    if (!is.null(node$right)) {
      result <- bart_build_path_recurse(node$right, new_path)
      if (!is.null(result)) {
        return(result)
      }
    }

    return(NULL)
  }

  bart_build_path_recurse(tree_hierarchy, integer())
}

# Internal helper to get split information
bart_get_split_info <- function(
  parent_row,
  child_row,
  tree_df,
  x,
  tree_hierarchy
) {
  # Determine if child is left or right
  is_left_child <- bart_is_left_child(parent_row, child_row, tree_hierarchy)

  # Get variable index and name
  var_idx <- tree_df$var[parent_row]
  var_names <- colnames(x$fit$data@x)
  var_name <- var_names[var_idx]

  # Get split threshold
  threshold <- tree_df$value[parent_row]

  # Determine operator based on direction
  # BART convention: left child when feature < threshold
  operator <- if (is_left_child) "<" else ">="

  list(
    column = var_name,
    value = threshold,
    operator = operator
  )
}

# Internal helper to determine if child is left or right
bart_is_left_child <- function(parent_row, child_row, tree_hierarchy) {
  bart_is_left_recurse <- function(node) {
    if (is.null(node) || node$row != parent_row) {
      return(NULL)
    }

    # Found the parent node
    # Check if child_row is in left subtree
    if (!is.null(node$left) && bart_contains_row(node$left, child_row)) {
      return(TRUE)
    }

    # Otherwise it must be in right subtree
    return(FALSE)
  }

  # Start search from root
  bart_find_and_check <- function(node) {
    if (is.null(node)) {
      return(NULL)
    }

    if (node$row == parent_row) {
      return(bart_is_left_recurse(node))
    }

    # Search in left subtree
    result <- bart_find_and_check(node$left)
    if (!is.null(result)) {
      return(result)
    }

    # Search in right subtree
    return(bart_find_and_check(node$right))
  }

  bart_find_and_check(tree_hierarchy)
}

# Internal helper to check if a subtree contains a specific row
bart_contains_row <- function(node, target_row) {
  if (is.null(node)) {
    return(FALSE)
  }

  if (node$row == target_row) {
    return(TRUE)
  }

  bart_contains_row(node$left, target_row) ||
    bart_contains_row(node$right, target_row)
}
