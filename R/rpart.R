#' @export
extract_rules.rpart <- function(x, ...) {
  terminal_ids <- rpart_get_terminal_nodes(x)

  rules_list <- lapply(terminal_ids, function(node_id) {
    path <- build_node_path(node_id, max(terminal_ids))

    split_exprs <- list()
    for (i in seq_along(path)[-length(path)]) {
      parent <- path[i]
      child <- path[i + 1]
      split_info <- rpart_get_split_info(parent, child, x)
      split_exprs[[i]] <- rect_split_to_expr(split_info)
    }

    combine_rule_elements(split_exprs)
  })

  tibble::tibble(
    id = as.integer(terminal_ids),
    rules = rules_list
  ) |>
    dplyr::arrange(id) |>
    tibble::new_tibble(class = c("rule_set_rpart", "rule_set"))
}

# rpart-specific helper to get terminal nodes
rpart_get_terminal_nodes <- function(x) {
  terminal_rows <- x$frame$var == "<leaf>"
  as.integer(rownames(x$frame[terminal_rows, ]))
}

# Potentially reusable helper to build path from root to node
build_node_path <- function(node_id, max_node_id) {
  path <- integer()
  current <- node_id

  while (current >= 1) {
    path <- c(current, path)
    if (current == 1) {
      break
    }
    current <- floor(current / 2)
  }

  path
}

# rpart-specific helper to get split info for a node
rpart_get_split_info <- function(node_id, child_id, x) {
  # Determine if we go left or right
  is_left <- child_id == 2 * node_id

  # Get variable name for this node's split
  node_row <- as.character(node_id)
  var_name <- x$frame[node_row, "var"]

  # Find the correct row in splits matrix
  # Splits are ordered by tree traversal, with each primary split
  # followed by ncompete + nsurrogate additional splits
  split_row <- rpart_find_split_row(node_id, x)
  ncat <- x$splits[split_row, "ncat"]
  split_index <- x$splits[split_row, "index"]

  # Handle categorical vs numeric splits
  # ncat = -1: standard numeric split (left: <, right: >=)
  # ncat = 1: ordered categorical/numeric split (left: >=, right: <)
  # ncat > 1: categorical split with ncat levels
  if (ncat == -1) {
    # Standard numeric split
    threshold <- split_index
    if (is_left) {
      list(column = var_name, value = threshold, operator = "<")
    } else {
      list(column = var_name, value = threshold, operator = ">=")
    }
  } else if (ncat == 1) {
    # Ordered categorical or reversed numeric split
    threshold <- split_index
    if (is_left) {
      list(column = var_name, value = threshold, operator = ">=")
    } else {
      list(column = var_name, value = threshold, operator = "<")
    }
  } else {
    # Categorical split (ncat > 1)
    # csplit matrix: row = split_index, columns = factor levels
    # values: 1 = left, 3 = right
    csplit_row <- x$csplit[as.integer(split_index), ]

    # Get factor levels for this variable
    # Check if variable is in ordered vector
    if (!is.null(x$ordered) && var_name %in% names(x$ordered)) {
      # For ordered factors, get levels from the data
      # We need to reconstruct levels from the frame
      # For now, use csplit indices as level numbers
      levels_going_direction <- which(
        if (is_left) csplit_row == 1 else csplit_row == 3
      )
    } else {
      # For unordered factors, same approach
      levels_going_direction <- which(
        if (is_left) csplit_row == 1 else csplit_row == 3
      )
    }

    # Convert level indices to actual level names
    # Try to get factor levels from the model's data
    level_names <- rpart_get_factor_levels(var_name, levels_going_direction, x)

    # Return appropriate operator
    if (length(level_names) == 1) {
      list(column = var_name, value = level_names, operator = "==")
    } else {
      list(column = var_name, value = level_names, operator = "%in%")
    }
  }
}

# rpart-specific helper to get factor level names
rpart_get_factor_levels <- function(var_name, level_indices, x) {
  # First try xlevels (fastest)
  if (!is.null(x$xlevels) && var_name %in% names(x$xlevels)) {
    return(x$xlevels[[var_name]][level_indices])
  }

  # Try to get from the model's data via environment
  if (!is.null(x$terms)) {
    env <- attr(x$terms, ".Environment")
    data_name <- as.character(x$call$data)

    if (length(data_name) > 0 && exists(data_name, envir = env)) {
      data <- get(data_name, envir = env)
      if (var_name %in% names(data) && is.factor(data[[var_name]])) {
        return(levels(data[[var_name]])[level_indices])
      }
    }
  }

  # Fallback: use numeric indices as strings
  as.character(level_indices)
}

# rpart-specific helper to find the row in splits matrix for a given node
rpart_find_split_row <- function(node_id, x) {
  # Get row names from frame (node IDs in tree traversal order)
  frame_nodes <- as.integer(rownames(x$frame))

  # Find position of this node in the frame
  node_pos <- which(frame_nodes == node_id)

  # Count splits used by all previous non-leaf nodes
  splits_used <- 0
  if (node_pos > 1) {
    for (i in seq_len(node_pos - 1)) {
      if (x$frame$var[i] != "<leaf>") {
        # Each non-leaf uses 1 + ncompete + nsurrogate splits
        splits_used <- splits_used +
          1 +
          x$frame$ncompete[i] +
          x$frame$nsurrogate[i]
      }
    }
  }

  # The split for this node is at position splits_used + 1
  splits_used + 1
}
