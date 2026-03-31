# Internal helper: extract rules for one tree from cforest
cforest_extract_rules_one <- function(tree_num, x) {
  # Validate tree number
  if (tree_num < 1L || tree_num > length(x$nodes)) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {length(x$nodes)}.",
      call = rlang::caller_env()
    )
  }

  # Create a party object from the node
  tree_party <- partykit::party(
    node = x$nodes[[tree_num]],
    data = x$data,
    fitted = x$fitted
  )

  # Extract rules using party method
  rules <- extract_rules.party(tree_party)

  # Add tree column
  rules$tree <- tree_num

  rules
}

#' @rdname extract_rules
#' @param tree Integer vector specifying which trees to extract rules from.
#'   Default is `1L` for the first tree. Values must be between 1 and the
#'   number of trees in the forest.
#' @export
extract_rules.cforest <- function(x, tree = 1L, ...) {
  rlang::check_installed("partykit")

  # Validate tree argument
  if (!is.numeric(tree) || !all(tree == as.integer(tree))) {
    cli::cli_abort(
      "{.arg tree} must be an integer vector, not {.obj_type_friendly {tree}}.",
      call = rlang::caller_env()
    )
  }

  tree <- as.integer(tree)

  # Validate tree range
  n_trees <- length(x$nodes)
  if (any(tree < 1L) || any(tree > n_trees)) {
    cli::cli_abort(
      "{.arg tree} values must be between 1 and {n_trees}.",
      call = rlang::caller_env()
    )
  }

  # Extract for each tree
  results <- lapply(tree, cforest_extract_rules_one, x = x)

  # Combine and sort by tree then id
  dplyr::bind_rows(results) |>
    dplyr::arrange(tree, id)
}

# Internal helper: extract active predictors for one tree from cforest
cforest_extract_active_one <- function(tree_num, x) {
  # Validate tree number
  if (tree_num < 1L || tree_num > length(x$nodes)) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {length(x$nodes)}.",
      call = rlang::caller_env()
    )
  }

  # Create a party object from the node
  tree_party <- partykit::party(
    node = x$nodes[[tree_num]],
    data = x$data,
    fitted = x$fitted
  )

  # Extract active predictors using party method
  result <- active_predictors.party(tree_party)

  # Add tree column
  result$tree <- tree_num

  result
}

#' @rdname active_predictors
#' @param tree Integer vector specifying which trees to extract active
#'   predictors from. Default is `1L` for the first tree. Values must be
#'   between 1 and the number of trees in the forest.
#' @export
active_predictors.cforest <- function(x, tree = 1L, ...) {
  rlang::check_installed("partykit")

  # Validate tree argument
  if (!is.numeric(tree) || !all(tree == as.integer(tree))) {
    cli::cli_abort(
      "{.arg tree} must be an integer vector, not {.obj_type_friendly {tree}}.",
      call = rlang::caller_env()
    )
  }

  tree <- as.integer(tree)

  # Validate tree range
  n_trees <- length(x$nodes)
  if (any(tree < 1L) || any(tree > n_trees)) {
    cli::cli_abort(
      "{.arg tree} values must be between 1 and {n_trees}.",
      call = rlang::caller_env()
    )
  }

  # Extract for each tree
  results <- lapply(tree, cforest_extract_active_one, x = x)

  # Combine and sort by tree
  dplyr::bind_rows(results) |>
    dplyr::arrange(tree)
}

# ------------------------------------------------------------------------------
# Variable importance for cforest

#' @export
#' @rdname lorax_var_imp
var_imp.cforest <- function(object, complete = TRUE, ...) {
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
