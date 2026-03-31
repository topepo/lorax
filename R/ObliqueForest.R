#' Extract rules from an ObliqueForest model
#'
#' Extracts the decision rules for terminal nodes in a specified tree from an
#' aorsf ObliqueForest model. Each rule represents the path from the root node
#' to a terminal node using oblique (linear combination) splits.
#'
#' @param x An `ObliqueForest` object from the \pkg{aorsf} package
#' @param tree Integer specifying which tree to extract rules from (1-based).
#'   Default is `1L` for the first tree. Must be between 1 and the number of
#'   trees in the forest (`x$n_tree`).
#' @param node_id An integer for the node ID to process.
#'
#' @return A tibble with columns:
#'   * `tree`: integer, the tree number (1-based).
#'   * `rules`: list of R expressions, one per terminal node.
#'   * `id`: integer, the terminal node ID (1-based for user convenience).
#'
#' @details
#'
#' ## Known Limitation
#'
#' **Important**: The extracted rules currently do NOT perfectly match aorsf's
#' internal node assignments. This is a known issue being investigated. The
#' rules are structurally correct (using the right splits and operators) but
#' may not evaluate to the exact same terminal nodes as aorsf's predictions.
#'
#' The rules are still useful for:
#' - Understanding the general structure of oblique splits
#' - Seeing which variables and linear combinations are used
#' - Interpreting model behavior qualitatively
#'
#' But should NOT be used for:
#' - Exactly replicating aorsf's predictions
#' - Validating which observations belong to which nodes
#'
#' ## Tree and Node Indexing
#'
#' Both the `tree` parameter and the `id` column use **1-based indexing** for
#' user convenience, matching R's standard indexing convention:
#'
#' - `tree = 1` extracts rules from the first tree
#' - `id = 1` refers to the first terminal node
#'
#' Internally, aorsf uses 0-based indexing (where node 0 is the root), but this
#' is automatically converted to 1-based indexing in the output for consistency
#' with R conventions.
#'
#' ## Factor Variables and One-Hot Encoding
#'
#' The \pkg{aorsf} package internally converts unordered factor variables to binary
#' indicator (dummy) variables during tree building. However, the extracted
#' rules automatically convert these back to factor comparisons for better
#' interpretability:
#'
#' - Instead of `2.1 * county_adams`, rules show `2.1 * (county == "adams")`
#' - This allows rules to be evaluated directly on data with the original factor
#'   columns (no need to create indicator variables)
#' - For example, a factor `color` with levels `["red", "blue", "green"]` will
#'   appear in rules as `(color == "red")`, `(color == "blue")`, etc.
#'
#' Ordered factors are converted to a single integer variable representing the
#' ordinal level, not to multiple indicators.
#'
#' Factor indicators are not scaled since they are binary 0/1 values.
#'
#' ## Predictor Scaling and Unscaling
#'
#' The \pkg{aorsf} package can optionally center and scale numeric predictors when
#' computing linear combinations for splits. This is controlled by the `scale_x`
#' parameter in `orsf_control_*()` functions (default is `TRUE`).
#'
#' When `scale_x = TRUE`, \pkg{aorsf} uses `(x - mean) / sd` for numeric predictors
#' during split computations. The extracted rules automatically **unscale** the
#' coefficients and thresholds back to the original units, so rules can be
#' directly evaluated on the original (unscaled) data.
#'
#' When `scale_x = FALSE`, coefficients are already in the original units and
#' no unscaling is performed.
#'
#' Factor indicator variables (from one-hot encoding) are not scaled since they
#' are binary 0/1 values.
#'
#' Note: `x$get_means()` and `x$get_stdev()` always return the data statistics
#' regardless of the `scale_x` setting. To determine if scaling was used, check
#' `x$control$lincomb_scale`.
#'
#' @examples
#' if (rlang::is_installed(c("aorsf", "palmerpenguins"))) {
#'   # Classification example
#'   penguins <- palmerpenguins::penguins[complete.cases(palmerpenguins::penguins), ]
#'   set.seed(2847)
#'   forest <- aorsf::orsf(species ~ ., data = penguins, n_tree = 3)
#'
#'   # Extract rules from first tree (default)
#'   rules <- extract_rules(forest)
#'
#'   # View rules as text
#'   rules$rules[[1]] |> rule_text(bullets = TRUE) |> cat("\n")
#'
#'   # Extract rules from different tree
#'   rules3 <- extract_rules(forest, tree = 3L)
#'
#'   # Regression example
#'   data(mtcars)
#'   set.seed(5193)
#'   forest_reg <- aorsf::orsf(mpg ~ ., data = mtcars, n_tree = 3)
#'   rules_reg <- extract_rules(forest_reg, tree = 1L)
#' }
#'
# Internal helper: extract rule for a single terminal node
oblique_extract_node_rule <- function(node_id, tree, x) {
  path <- oblique_build_node_path(node_id, tree, x$forest)

  split_exprs <- list()
  for (i in seq_along(path)[-length(path)]) {
    parent_id <- path[i]
    child_id <- path[i + 1]
    split_info <- oblique_get_split_info(parent_id, child_id, tree, x)
    expr <- obliq_split_to_expr(split_info)
    # Replace indicator variables with factor comparisons for interpretability
    split_exprs[[i]] <- oblique_replace_indicators(expr, x)
  }

  combine_rule_elements(split_exprs)
}

#' @export
extract_rules.ObliqueForest <- function(x, tree = 1L, ...) {
  rlang::check_installed("aorsf")
  # Validate tree argument (1-based for user convenience)
  if (!is.numeric(tree) || length(tree) != 1 || tree != as.integer(tree)) {
    cli::cli_abort("{.arg tree} must be a single integer.")
  }
  if (tree < 1 || tree > x$n_tree) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {x$n_tree}, not {tree}."
    )
  }

  # Get terminal node IDs for this tree
  # Note: tree parameter is 1-based and maps directly to R's 1-based list indexing
  # terminal_ids are 0-indexed (aorsf internal convention), converted to 1-based below
  terminal_ids <- oblique_get_terminal_nodes(tree, x$forest)

  # Extract rules for each terminal node (using 0-indexed IDs internally)
  rules_list <- lapply(
    terminal_ids,
    oblique_extract_node_rule,
    tree = tree,
    x = x
  )

  tibble::tibble(
    tree = as.integer(tree),
    rules = rules_list,
    id = as.integer(terminal_ids) + 1L # Convert from 0-indexed to 1-indexed
  ) |>
    dplyr::arrange(id) |>
    tibble::new_tibble(class = c("rule_set_ObliqueForest", "rule_set"))
}

# Internal helper to get terminal nodes for a tree
oblique_get_terminal_nodes <- function(tree_num, forest) {
  # tree_num is 1-based (for R list indexing)
  # Returns 0-indexed node IDs (aorsf convention)
  child_left <- forest$child_left[[tree_num]]

  # Terminal nodes have child_left == 0
  # which() returns 1-indexed positions, subtract 1 to get 0-indexed node IDs
  which(child_left == 0) - 1
}

# Internal helper to find parent node
oblique_find_parent <- function(node_id, tree_num, forest) {
  # node_id is 0-indexed (aorsf convention)
  # tree_num is 1-based (for R list indexing)
  # Returns 0-indexed parent node ID
  if (node_id == 0) {
    return(NA_integer_)
  }

  child_left <- forest$child_left[[tree_num]]

  # Search for parent node
  # i is 1-indexed from seq_along, so we subtract 1 to get 0-indexed node ID
  for (i in seq_along(child_left)) {
    # Check if this node is the parent of node_id
    if (child_left[i] == node_id || child_left[i] + 1 == node_id) {
      return(i - 1)
    }
  }

  NA_integer_
}

# Internal helper to build path from root to target node
oblique_build_node_path <- function(node_id, tree_num, forest) {
  # node_id is 0-indexed (aorsf convention)
  # tree_num is 1-based (for R list indexing)
  # Returns vector of 0-indexed node IDs from root to target
  path <- integer()
  current <- node_id

  while (current >= 0) {
    path <- c(current, path)
    if (current == 0) {
      break
    }

    current <- oblique_find_parent(current, tree_num, forest)
    if (is.na(current)) break
  }

  path
}

# Internal helper to unscale coefficients and threshold
oblique_unscale_split <- function(columns, coef_vals, threshold, x) {
  # Get means and standard deviations for numeric variables
  means <- x$get_means()
  stdevs <- x$get_stdev()

  # Get base predictor names (before one-hot encoding)
  pred_names <- x$get_names_x()

  # For each variable in the split, unscale if numeric
  unscaled_coefs <- coef_vals
  threshold_adjustment <- 0

  for (i in seq_along(columns)) {
    var_name <- columns[i]

    # Check if this is a numeric variable (has mean and sd)
    if (var_name %in% names(means)) {
      # Numeric variable: unscale coefficient and adjust threshold
      # Original: coef * (x - mean) / sd
      # Unscaled: (coef / sd) * x with threshold adjusted by (coef * mean / sd)
      sd_val <- stdevs[[var_name]]
      mean_val <- means[[var_name]]

      unscaled_coefs[i] <- coef_vals[i] / sd_val
      threshold_adjustment <- threshold_adjustment +
        (coef_vals[i] * mean_val / sd_val)
    }
    # Factor indicators (one-hot encoded) are already 0/1, no scaling to reverse
  }

  list(
    columns = columns,
    values = unscaled_coefs,
    threshold = threshold + threshold_adjustment
  )
}

# Internal helper to replace indicator variables with factor comparisons
oblique_replace_indicators <- function(expr, x) {
  # Get factor information
  fctr_info <- x$get_fctr_info()

  # If no factors, return expression as-is
  if (length(fctr_info$cols) == 0) {
    return(expr)
  }

  # Build mapping from indicator name to (factor, level)
  indicator_map <- list()
  for (var_name in fctr_info$cols) {
    indicators <- fctr_info$keys[[var_name]]
    levels <- fctr_info$lvls[[var_name]]
    for (i in seq_along(indicators)) {
      indicator_map[[indicators[i]]] <- list(
        factor = var_name,
        level = levels[i]
      )
    }
  }

  # Recursively replace indicator symbols with factor comparisons
  replace_symbols <- function(e) {
    if (is.symbol(e)) {
      name <- as.character(e)
      if (name %in% names(indicator_map)) {
        # Replace indicator with factor == "level"
        info <- indicator_map[[name]]
        return(rlang::call2("==", rlang::sym(info$factor), info$level))
      }
      return(e)
    } else if (is.call(e)) {
      # Recursively process call arguments
      e[] <- lapply(e, replace_symbols)
      return(e)
    } else {
      return(e)
    }
  }

  replace_symbols(expr)
}

# Internal helper to get expanded variable names including one-hot encoded factors
oblique_get_var_names <- function(x) {
  # Get base predictor names
  pred_names <- x$get_names_x()

  # Get factor information
  fctr_info <- x$get_fctr_info()

  # If no factors, return predictor names as-is
  if (length(fctr_info$cols) == 0) {
    return(pred_names)
  }

  # Build expanded list with one-hot encoded factor names
  expanded_names <- character()

  for (var_name in pred_names) {
    if (var_name %in% fctr_info$cols) {
      # This is a factor - add all one-hot encoded columns
      expanded_names <- c(expanded_names, fctr_info$keys[[var_name]])
    } else {
      # Not a factor - add as-is
      expanded_names <- c(expanded_names, var_name)
    }
  }

  expanded_names
}

# Internal helper to extract split information
oblique_get_split_info <- function(parent_id, child_id, tree_num, x) {
  # parent_id and child_id are 0-indexed (aorsf convention)
  # tree_num is 1-based (for R list indexing)
  forest <- x$forest

  # Determine split direction: left uses <=, right uses >
  # Add 1 to parent_id to convert from 0-indexed node ID to 1-indexed R array position
  is_left <- forest$child_left[[tree_num]][parent_id + 1] == child_id
  operator <- if (is_left) "<=" else ">"

  # Extract split components (add 1 to convert 0-indexed node ID to 1-indexed R array position)
  coef_vals <- forest$coef_values[[tree_num]][[parent_id + 1]]
  coef_idxs <- forest$coef_indices[[tree_num]][[parent_id + 1]]
  threshold <- forest$cutpoint[[tree_num]][parent_id + 1]

  # Handle empty coefficient arrays (shouldn't happen for non-terminal nodes)
  if (length(coef_idxs) == 0) {
    return(list(
      columns = character(0),
      values = numeric(0),
      operator = operator,
      threshold = threshold
    ))
  }

  # Get variable names including one-hot encoded factors
  # aorsf expands factors internally, so we need to reconstruct the full list
  var_names <- oblique_get_var_names(x)

  # coef_idxs are 0-indexed, add 1 to convert to 1-indexed R array positions
  columns <- var_names[coef_idxs + 1]

  # Check for invalid indices that resulted in NA
  if (any(is.na(columns))) {
    cli::cli_abort(
      "Invalid coefficient indices found: some indices are out of bounds."
    )
  }

  # Unscale coefficients and adjust threshold to original units if scaling was used
  # Only unscale if lincomb_scale = TRUE (controlled by scale_x parameter in orsf_control)
  # When lincomb_scale = FALSE, coefficients are already in original units
  if (isTRUE(x$control$lincomb_scale)) {
    unscale_result <- oblique_unscale_split(columns, coef_vals, threshold, x)
  } else {
    unscale_result <- list(
      columns = columns,
      values = coef_vals,
      threshold = threshold
    )
  }

  # Return in format expected by obliq_split_to_expr()
  list(
    columns = unscale_result$columns,
    values = unscale_result$values,
    operator = operator,
    threshold = unscale_result$threshold
  )
}

# Internal helper: extract active predictors from one tree
oblique_active_vars_one_tree <- function(tree_num, x) {
  # Get child_left for this tree (0-indexed internally)
  child_left <- x$forest$child_left[[tree_num]]

  # Find non-terminal nodes (child_left != 0)
  non_terminal_idx <- which(child_left != 0)

  # If no non-terminal nodes, return empty character vector
  if (length(non_terminal_idx) == 0) {
    return(character(0))
  }

  # Extract all variable indices from non-terminal nodes
  all_indices <- integer(0)
  for (idx in non_terminal_idx) {
    coef_idxs <- x$forest$coef_indices[[tree_num]][[idx]]
    if (length(coef_idxs) > 0) {
      all_indices <- c(all_indices, coef_idxs)
    }
  }

  # Get unique indices and convert 0-indexed to 1-indexed
  all_indices <- unique(all_indices) + 1L

  # Map to variable names (includes one-hot encoded factors)
  var_names_expanded <- oblique_get_var_names(x)
  active_vars <- var_names_expanded[all_indices]

  # Collapse factor indicators to base names
  oblique_collapse_factor_names(active_vars, x)
}

# Internal helper: collapse factor indicator names to base factor names
oblique_collapse_factor_names <- function(var_names, x) {
  fctr_info <- x$get_fctr_info()

  # If no factors, return as-is
  if (length(fctr_info$cols) == 0) {
    return(var_names)
  }

  # Build mapping from indicator name to base factor name
  # e.g., "county_adams" -> "county", "county_benton" -> "county"
  indicator_to_base <- list()
  for (factor_name in fctr_info$cols) {
    indicators <- fctr_info$keys[[factor_name]]
    for (ind in indicators) {
      indicator_to_base[[ind]] <- factor_name
    }
  }

  # Replace indicators with base names
  result <- character(length(var_names))
  for (i in seq_along(var_names)) {
    if (var_names[i] %in% names(indicator_to_base)) {
      # This is a factor indicator - use base name
      result[i] <- indicator_to_base[[var_names[i]]]
    } else {
      # Not a factor indicator - keep as-is
      result[i] <- var_names[i]
    }
  }

  # Return unique names (factors may appear multiple times via different indicators)
  unique(result)
}

# Internal helper: extract active predictors for one tree and wrap in constructor
oblique_extract_one <- function(tree_num, x) {
  active_vars <- oblique_active_vars_one_tree(tree_num, x)
  new_active_predictors(active_vars, tree = tree_num)
}

#' @rdname active_predictors
#' @param tree Integer vector specifying which trees to extract active
#'   predictors from. Default is `1L` for the first tree. Values must be
#'   between 1 and the number of trees in the forest (`x$n_tree`).
#' @export
active_predictors.ObliqueForest <- function(x, tree = 1L, ...) {
  rlang::check_installed("aorsf")

  # Validate tree argument
  if (!is.numeric(tree) || !all(tree == as.integer(tree))) {
    cli::cli_abort(
      "{.arg tree} must be an integer vector, not {.obj_type_friendly {tree}}.",
      call = rlang::caller_env()
    )
  }

  tree <- as.integer(tree)

  if (any(tree < 1L) || any(tree > x$n_tree)) {
    cli::cli_abort(
      "{.arg tree} values must be between 1 and {x$n_tree}, not {tree}.",
      call = rlang::caller_env()
    )
  }

  # Extract active predictors for each tree
  results <- lapply(tree, oblique_extract_one, x = x)

  # Combine results and sort by tree
  dplyr::bind_rows(results) |>
    dplyr::arrange(tree)
}

# ------------------------------------------------------------------------------
# Variable importance Wrapper

#' @export
#' @rdname lorax_var_imp
var_imp.ObliqueForest <- function(object, complete = TRUE, ...) {
  rlang::check_installed("aorsf")

  # Get variable importance from aorsf object
  imp <- object$get_importance_clean()

  # Convert to tibble
  res <- tibble::enframe(imp)
  names(res) <- c("term", "estimate")

  if (complete) {
    # Get all predictor names from the model
    pred_names <- object$get_names_x()
    res <- complete_results(res, pred_names)
  }

  res
}
