#' Extract rules from a BART model
#'
#' Extract interpretable decision rules from a single tree in a BART
#' (Bayesian Additive Regression Trees) model. Each terminal node (leaf)
#' becomes one rule representing the path from root to that leaf.
#'
#' @param x A `bart` object from the \pkg{dbarts} package fitted with
#'   `keeptrees = TRUE`.
#' @param tree Integer specifying which tree to extract rules from. Uses
#'   1-based indexing (default is `1L`). BART models contain `n.trees` trees
#'   in the ensemble.
#' @param chain Integer specifying which MCMC chain to extract from. Uses
#'   1-based indexing (default is `1L`). Only relevant for models fitted with
#'   multiple chains.
#' @param ... Not currently used.
#'
#' @return A tibble with class `c("rule_set_bart", "rule_set")` and
#'   columns:
#'   * `tree`: integer, the tree number (matches input parameter).
#'   * `rules`: list of R expressions, one per terminal node.
#'   * `id`: integer, terminal node ID (1-based).
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
#' if (rlang::is_installed(c("dbarts", "palmerpenguins"))) {
#'   # Classification example
#'   data(penguins, package = "palmerpenguins")
#'   penguins <- na.omit(penguins)
#'
#'   train_data <- penguins[, c("bill_length_mm", "bill_depth_mm",
#'                              "flipper_length_mm", "body_mass_g", "species")]
#'
#'   set.seed(2847)
#'   fit <- dbarts::bart(
#'     x.train = train_data[, 1:4],
#'     y.train = train_data$species,
#'     keeptrees = TRUE,
#'     verbose = FALSE,
#'     ntree = 2
#'   )
#'
#'   # Extract rules from first tree
#'   rules <- extract_rules(fit, tree = 1L)
#'
#'   # View as text
#'   rule_text(rules$rules[[1]])
#'
#'   # Regression example
#'   data(mtcars)
#'   set.seed(5193)
#'   fit_reg <- dbarts::bart(
#'     x.train = mtcars[, -1],
#'     y.train = mtcars$mpg,
#'     keeptrees = TRUE,
#'     verbose = FALSE,
#'     ntree = 2
#'   )
#'   rules_reg <- extract_rules(fit_reg, tree = 1L)
#' }
#'
#' @export
extract_rules.bart <- function(x, tree = 1L, chain = 1L, ...) {
  rlang::check_installed("dbarts")
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
      "{.pkg dbarts} model must be fitted with {.code keeptrees = TRUE} to extract rules."
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
    dplyr::arrange(id) |>
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

# ------------------------------------------------------------------------------

#' Convert BART model to party object
#'
#' Convert a single tree from a BART (Bayesian Additive Regression Trees) model
#' to a party object for use with \pkg{partykit} visualization and analysis tools.
#'
#' @param obj A `bart` object from the \pkg{dbarts} package fitted with
#'   `keeptrees = TRUE`.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). BART models contain `n.trees` trees in the ensemble.
#' @param chain Integer specifying which MCMC chain to extract from (1-based
#'   indexing, default is 1). Only relevant for models fitted with multiple
#'   chains.
#' @param data data.frame containing the **original untransformed** training
#'   data with original response values (required). BART internally transforms
#'   data (creating dummy variables for factors and converting responses to
#'   0/1). You must provide the original data frame that includes both the
#'   predictor variables and the response variable in their original formats
#'   (e.g., factors for classification).
#' @param ... Not currently used.
#'
#' @return A `constparty` object from the \pkg{partykit} package.
#'
#' @details
#' ## Important note on data transformation
#'
#' BART internally transforms the training data in ways that make it unsuitable
#' for display in party objects. Specifically, BART creates dummy variables for
#' factor predictors and converts factor responses to 0/1 numeric values. To get
#' correct terminal node statistics, bar charts, and other visualizations, you
#' **must** provide the original untransformed data (including the response
#' variable) via the `data` parameter.
#'
#' ## BART tree storage format
#'
#' The \pkg{dbarts} package stores trees in depth-first traversal order in a
#' data.frame accessible via `obj$fit$getTrees()`. Each row represents one node:
#' - `var`: 1-based variable index for split, or -1 for terminal nodes
#' - `value`: threshold for internal nodes, prediction for terminal nodes
#' - `tree`: 1-based tree number
#' - `chain`: chain number (if multiple chains)
#' - `sample`: MCMC sample number
#'
#' ## Depth-first traversal order
#'
#' - Nodes stored as: parent, left subtree (complete), right subtree (complete)
#' - Example: root at row 1, left child at row 2, right child after left subtree
#' - Must track row consumption to determine subtree boundaries
#'
#' ## Node indexing
#'
#' - User-facing `tree` and `chain` parameters use 1-based indexing (R convention)
#' - Variable indices in `var` column are 1-based (match `obj$varNames`)
#' - Value -1 in `var` indicates terminal node
#'
#' ## Split encoding
#'
#' - Left child: feature < threshold
#' - Right child: feature >= threshold
#' - partykit split created with `right = TRUE` (right interval closed)
#'
#' ## Variable names
#'
#' - Available in `obj$fit$data@x` column names or `obj$varNames`
#' - `var` column provides 1-based index into these names
#'
#' The party object will use 1-based node IDs and variable indices as required
#' by \pkg{partykit}.
#'
#' @examples
#' if (rlang::is_installed(c("dbarts", "palmerpenguins"))) {
#'   # Classification example
#'   data(penguins, package = "palmerpenguins")
#'   penguins <- na.omit(penguins)
#'
#'   # Prepare data with response column
#'   train_data <- penguins[, c("bill_length_mm", "bill_depth_mm",
#'                              "flipper_length_mm", "body_mass_g", "species")]
#'
#'   set.seed(2847)
#'   fit <- dbarts::bart(
#'     x.train = train_data[, 1:4],
#'     y.train = train_data$species,
#'     keeptrees = TRUE,
#'     verbose = FALSE,
#'     ntree = 2
#'   )
#'
#'   # Convert first tree - data parameter is required
#'   # Response will be preserved in original format (e.g., factor for classification)
#'   party_tree <- as.party(fit, tree = 1L, chain = 1L, data = train_data)
#'   print(party_tree)
#'   plot(party_tree)
#'
#'   # Regression example
#'   data(mtcars)
#'   set.seed(5193)
#'   fit_reg <- dbarts::bart(
#'     x.train = mtcars[, -1],
#'     y.train = mtcars$mpg,
#'     keeptrees = TRUE,
#'     verbose = FALSE,
#'     ntree = 2
#'   )
#'   party_tree_reg <- as.party(fit_reg, tree = 1L, chain = 1L, data = mtcars)
#'   print(party_tree_reg)
#' }
#'
#' @export
as.party.bart <- function(obj, tree = 1L, chain = 1L, data, ...) {
  rlang::check_installed("dbarts")
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
  if (is.null(obj$fit)) {
    cli::cli_abort(
      "{.pkg dbarts} model must be fitted with {.code keeptrees = TRUE} to extract trees."
    )
  }

  # Extract trees directly from fit object
  trees_df <- obj$fit$getTrees()

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

  # Get variable names
  var_names <- colnames(obj$fit$data@x)
  if (is.null(var_names)) {
    # Fallback to varNames if available
    var_names <- obj$varNames
    if (is.null(var_names)) {
      # Last resort: create generic names
      max_var <- max(tree_df$var[tree_df$var > 0], na.rm = TRUE)
      var_names <- paste0("X", seq_len(max_var))
    }
  }

  # Handle single-node tree (root is leaf)
  if (nrow(tree_df) == 1 && tree_df$var[1] == -1) {
    root_node <- partykit::partynode(id = 1L)
  } else {
    # Build partynode structure from depth-first format
    # Returns list with node and number of rows consumed
    result <- bart_build_partynode_df(
      tree_df = tree_df,
      start_row = 1L,
      var_names = var_names
    )
    root_node <- result$node

    # Assign sequential IDs
    root_node <- assign_node_ids(root_node)$node
  }

  # BART expands factor variables to dummy variables (one-hot encoding)
  # We need to handle this by expanding the user's data to match
  bart_x <- as.data.frame(obj$fit$data@x)

  if (ncol(bart_x) != length(var_names)) {
    cli::cli_abort(
      "Internal error: BART data dimensions don't match variable names."
    )
  }

  # Check if var_names match columns in user's data
  if (!all(var_names %in% names(data))) {
    # BART likely expanded factors - need to expand user's data
    # Use dbarts' internal expansion by passing through the data

    # Find which columns from user's data were used as predictors
    # (all except response)
    all_cols <- names(data)

    # Try to identify response column
    response_name <- NULL
    if (!is.null(obj$call) && "y.train" %in% names(obj$call)) {
      y_expr <- deparse(obj$call$y.train)
      if (grepl("\\$|\\[\\[", y_expr)) {
        col_match <- regmatches(
          y_expr,
          regexpr("(?<=\\$|\\[\\[\"|\\[\\[')\\w+", y_expr, perl = TRUE)
        )
        if (length(col_match) > 0 && col_match %in% all_cols) {
          response_name <- col_match
        }
      }
    }

    # Separate predictors and response
    if (!is.null(response_name)) {
      predictor_cols <- setdiff(all_cols, response_name)
    } else {
      # Assume last column is response as a fallback
      predictor_cols <- all_cols[-length(all_cols)]
      response_name <- all_cols[length(all_cols)]
    }

    # Expand the predictors to match BART's format
    predictor_data <- data[, predictor_cols, drop = FALSE]

    # Use dbarts' makeModelMatrixFromDataFrame to expand factors the same way
    expanded <- dbarts::makeModelMatrixFromDataFrame(predictor_data)

    # Check if expansion matches
    if (
      ncol(expanded) == length(var_names) &&
        all(colnames(expanded) == var_names)
    ) {
      # Success! Use expanded data
      party_data <- as.data.frame(expanded)
      # Add response column in original format
      party_data[[response_name]] <- data[[response_name]]
    } else {
      cli::cli_abort(
        c(
          "{.arg data} columns don't match BART's internal representation.",
          "i" = "BART expects: {.field {var_names[1:min(3, length(var_names))]}}..",
          "i" = "Found in data: {.field {names(data)[1:min(3, length(names(data)))]}}"
        )
      )
    }
  } else {
    # Simple case: data columns match var_names directly
    response_cols <- setdiff(names(data), var_names)

    if (length(response_cols) == 0) {
      cli::cli_abort(
        "{.arg data} must contain the response variable in addition to predictors."
      )
    }

    # Determine response name
    if (length(response_cols) == 1) {
      response_name <- response_cols[1]
    } else {
      # Multiple candidates - try to identify from call
      if (!is.null(obj$call) && "y.train" %in% names(obj$call)) {
        y_expr <- deparse(obj$call$y.train)
        if (grepl("\\$|\\[\\[", y_expr)) {
          col_match <- regmatches(
            y_expr,
            regexpr("(?<=\\$|\\[\\[\"|\\[\\[')\\w+", y_expr, perl = TRUE)
          )
          if (length(col_match) > 0 && col_match %in% response_cols) {
            response_name <- col_match
          } else {
            cli::cli_warn(
              "Could not identify response. Using first non-predictor column: {.field {response_cols[1]}}."
            )
            response_name <- response_cols[1]
          }
        } else {
          cli::cli_warn(
            "Could not identify response. Using first non-predictor column: {.field {response_cols[1]}}."
          )
          response_name <- response_cols[1]
        }
      } else {
        cli::cli_warn(
          "Could not identify response. Using first non-predictor column: {.field {response_cols[1]}}."
        )
        response_name <- response_cols[1]
      }
    }

    # Build party_data with predictors and response
    party_data <- data[, var_names, drop = FALSE]
    party_data[[response_name]] <- data[[response_name]]
  }

  # Create terms object
  formula <- stats::as.formula(paste(
    "~",
    paste(var_names, collapse = " + ")
  ))
  terms <- stats::terms(formula, data = party_data)

  # Create fitted values structure
  # Compute which terminal node each observation belongs to
  fitted_ids <- compute_fitted_node_ids(
    root_node,
    party_data[, var_names, drop = FALSE]
  )

  # Get response (will be in original format, e.g., factor for classification)
  response <- party_data[[response_name]]

  fitted <- create_fitted_dataframe(fitted_ids, response)

  # Create constparty object
  party_obj <- create_party_object(
    node = root_node,
    data = party_data,
    fitted = fitted,
    terms = terms,
    info = list(method = "bart", tree = tree, chain = chain)
  )

  class(party_obj) <- c("constparty", "party")

  party_obj
}

# Internal helper to recursively build partynode from depth-first BART format
#
# BART stores trees in depth-first order: parent, left subtree, right subtree.
# Each call processes one node and returns both the node and row count consumed.
#
# @param tree_df Data.frame with depth-first tree structure
# @param start_row Row index to start processing (1-based)
# @param var_names Character vector of variable names
#
# @return List with elements:
#   - node: A partynode object
#   - rows_consumed: Integer, number of rows consumed from tree_df
bart_build_partynode_df <- function(tree_df, start_row, var_names) {
  if (start_row > nrow(tree_df)) {
    cli::cli_abort("Row index {start_row} exceeds tree data rows.")
  }

  # Current node information
  var_idx <- tree_df$var[start_row]
  value <- tree_df$value[start_row]

  # Check if terminal node (var == -1)
  if (var_idx == -1) {
    # Terminal node - no info for constparty
    return(list(
      node = partykit::partynode(id = 1L),
      rows_consumed = 1L
    ))
  }

  # Internal node
  # Validate variable index
  if (var_idx < 1 || var_idx > length(var_names)) {
    cli::cli_abort(
      "Invalid variable index {var_idx} at row {start_row}."
    )
  }

  # Create split object
  # BART: left child when feature < threshold
  split <- build_partysplit(var_idx, value, right = TRUE)

  # Recursively build left subtree (immediately after current node)
  left_result <- bart_build_partynode_df(
    tree_df,
    start_row + 1L,
    var_names
  )
  left_node <- left_result$node
  left_rows <- left_result$rows_consumed

  # Recursively build right subtree (after left subtree)
  right_result <- bart_build_partynode_df(
    tree_df,
    start_row + 1L + left_rows,
    var_names
  )
  right_node <- right_result$node
  right_rows <- right_result$rows_consumed

  # Total rows consumed: current + left subtree + right subtree
  total_rows <- 1L + left_rows + right_rows

  return(list(
    node = partykit::partynode(
      id = 1L, # Will be updated by assign_node_ids
      split = split,
      kids = list(left_node, right_node)
    ),
    rows_consumed = total_rows
  ))
}
