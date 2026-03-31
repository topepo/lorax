# Internal helper: extract rule for a single terminal node
party_extract_node_rule <- function(node_id, x) {
  path <- party_build_node_path(node_id, x)

  split_exprs <- list()
  for (i in seq_along(path)[-length(path)]) {
    parent_id <- path[i]
    child_id <- path[i + 1]
    split_info <- party_get_split_info(parent_id, child_id, x)
    split_exprs[[i]] <- rect_split_to_expr(split_info)
  }

  combine_rule_elements(split_exprs)
}

#' @export
extract_rules.party <- function(x, ...) {
  terminal_ids <- partykit::nodeids(x, terminal = TRUE)

  rules_list <- lapply(terminal_ids, party_extract_node_rule, x = x)

  tibble::tibble(
    id = as.integer(terminal_ids),
    rules = rules_list
  ) |>
    dplyr::arrange(id) |>
    tibble::new_tibble(class = c("rule_set_party", "rule_set"))
}

# party-specific helper to build path from root to target node
party_build_node_path <- function(node_id, x) {
  # Start with empty path
  path <- integer()
  current <- node_id

  # Work backwards from target node to root
  while (current >= 1) {
    path <- c(current, path)
    if (current == 1) {
      break
    }

    # Find parent of current node
    parent <- party_find_parent(current, x)
    if (is.na(parent)) {
      break
    }
    current <- parent
  }

  path
}

# party-specific helper to find parent node
party_find_parent <- function(node_id, x) {
  # Get all non-terminal node IDs
  all_ids <- partykit::nodeids(x, terminal = FALSE)

  # Check each non-terminal node to see if it's the parent
  for (parent_id in all_ids) {
    node <- partykit::nodeapply(x, ids = parent_id, by_node = TRUE)[[1]]
    kids <- partykit::kids_node(node)

    if (!is.null(kids)) {
      kid_ids <- vapply(kids, partykit::id_node, integer(1))
      if (node_id %in% kid_ids) {
        return(parent_id)
      }
    }
  }

  NA_integer_
}

# party-specific helper to extract split information
party_get_split_info <- function(parent_id, child_id, x) {
  # Get parent node
  parent_node <- partykit::nodeapply(x, ids = parent_id, by_node = TRUE)[[1]]

  # Get split info
  sp <- partykit::split_node(parent_node)

  # Get variable name
  var_idx <- partykit::varid_split(sp)
  var_name <- names(x$data)[var_idx]

  # Get kids to determine position
  kids <- partykit::kids_node(parent_node)
  kid_ids <- vapply(kids, partykit::id_node, integer(1))
  kid_position <- which(kid_ids == child_id)

  # Check if numeric or categorical split
  breaks <- partykit::breaks_split(sp)

  if (!is.null(breaks)) {
    # Numeric split
    threshold <- breaks
    right <- partykit::right_split(sp)

    if (right) {
      # Kid 1 uses <=, Kid 2 uses >
      if (kid_position == 1) {
        list(column = var_name, value = threshold, operator = "<=")
      } else {
        list(column = var_name, value = threshold, operator = ">")
      }
    } else {
      # Kid 1 uses <, Kid 2 uses >=
      if (kid_position == 1) {
        list(column = var_name, value = threshold, operator = "<")
      } else {
        list(column = var_name, value = threshold, operator = ">=")
      }
    }
  } else {
    # Categorical split
    index <- partykit::index_split(sp)

    # Get factor levels that go to this kid (filter out NAs)
    which_levels <- which(index == kid_position)
    levels_for_kid <- levels(x$data[[var_idx]])[which_levels]

    # Return appropriate operator
    if (length(levels_for_kid) == 1) {
      list(column = var_name, value = levels_for_kid, operator = "==")
    } else {
      list(column = var_name, value = levels_for_kid, operator = "%in%")
    }
  }
}

# Internal helper: extract variable name from a node
party_extract_var_name <- function(node, x) {
  sp <- partykit::split_node(node)

  # Only extract variable if split exists (some non-terminal nodes may not have splits)
  if (!is.null(sp)) {
    var_idx <- partykit::varid_split(sp)
    names(x$data)[var_idx]
  } else {
    character(0)
  }
}

#' @rdname active_predictors
#' @export
active_predictors.party <- function(x, ...) {
  rlang::check_installed("partykit")

  # Get all non-terminal nodes (nodes with splits)
  non_terminal_ids <- partykit::nodeids(x, terminal = FALSE)

  # Extract variable names from each split
  if (length(non_terminal_ids) == 0) {
    # No non-terminal nodes means no splits
    active_vars <- character(0)
  } else {
    nodes <- partykit::nodeapply(x, ids = non_terminal_ids, by_node = TRUE)
    active_vars_list <- lapply(nodes, party_extract_var_name, x = x)

    # Combine all variables
    active_vars <- unique(unlist(active_vars_list, use.names = FALSE))
  }

  # Use constructor to create result
  new_active_predictors(active_vars)
}

# ------------------------------------------------------------------------------
# Variable importance for party

#' @export
#' @rdname lorax_var_imp
var_imp.party <- function(object, complete = TRUE, ...) {
  rlang::check_installed("partykit")

  # Use partykit::varimp() to compute variable importance
  imp <- partykit::varimp(object, ...)

  # Convert to tibble
  res <- tibble::enframe(imp)
  names(res) <- c("term", "estimate")

  if (complete) {
    # Get all predictor names from the model
    pred_names <- all.vars(object$terms)[-attr(object$terms, "response")]
    res <- complete_results(res, pred_names)
  }

  res
}
