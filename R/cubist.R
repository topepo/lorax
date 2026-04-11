#' Extract rules from a Cubist model
#'
#' @description
#' Extracts rule conditions from a Cubist regression model as R expressions.
#' Each rule consists of conditions that define when a linear model applies.
#'
#' @param x A `cubist` object from the Cubist package.
#' @param committee An integer vector specifying which committee(s) to extract
#'   rules from. Defaults to `1L` (first committee). Values must be between 1
#'   and the total number of committees in the model.
#' @param ... Not used.
#'
#' @return A tibble with columns:
#'   - `committee`: Integer committee number
#'   - `id`: Integer rule number within the committee
#'   - `rules`: List column containing R expressions for each rule's conditions
#'
#' @details
#' Cubist models use committees (similar to boosting iterations) where each
#' committee contains multiple rules. Each rule has:
#' - Conditions that determine when the rule applies (splits on predictors)
#' - A linear model that makes predictions when conditions are met
#'
#' This function extracts the conditions as R expressions that can be evaluated
#' on data. Rules with no conditions (applying to all data) return `TRUE`.
#'
#' The expressions use standard R operators:
#' - Continuous splits: `>`, `<=`, etc.
#' - Categorical single value: `==`
#' - Categorical multiple values: `%in%`
#' - Missing values: `is.na()`
#'
#' @examples
#' \dontrun{
#' library(Cubist)
#' library(lorax)
#'
#' # Create sample data
#' set.seed(1)
#' n <- 100
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("x", 1:p)
#' y <- X[, 1] + X[, 2]^2 + rnorm(n)
#'
#' # Fit Cubist model with multiple committees
#' mod <- cubist(X, y, committees = 3)
#'
#' # Extract rules from first committee
#' rules <- extract_rules(mod)
#' rules
#'
#' # Extract from multiple committees
#' rules_all <- extract_rules(mod, committee = 1:3)
#'
#' # Convert to readable text
#' rule_text(rules$rules[[1]])
#' }
#'
#' @seealso [rules::tidy.cubist()] for extracting rules as text strings
#' @export
extract_rules.cubist <- function(x, committee = 1L, ...) {
  rlang::check_installed("Cubist")

  # Validate committee parameter
  if (!is.numeric(committee) || !all(committee == as.integer(committee))) {
    cli::cli_abort(
      "{.arg committee} must be an integer vector, not {.obj_type_friendly {committee}}.",
      call = rlang::caller_env()
    )
  }

  committee <- as.integer(committee)
  committee <- unique(committee)

  # Check bounds
  num_committees <- x$committees
  if (any(committee < 1L) || any(committee > num_committees)) {
    cli::cli_abort(
      "{.arg committee} values must be between 1 and {num_committees}, not {committee}.",
      call = rlang::caller_env()
    )
  }

  # Extract rules for each committee
  results <- lapply(
    committee,
    cubist_extract_rules_from_committee,
    x = x
  )

  # Combine results
  result_df <- dplyr::bind_rows(results) |>
    dplyr::arrange(committee, id)

  # Add classes following lorax pattern
  class(result_df) <- c("rule_set_cubist", "rule_set", class(result_df))

  result_df
}

# Internal helper to extract rules from a single committee
cubist_extract_rules_from_committee <- function(x, committee_num) {
  # Get splits for this committee (may be NULL if no splits)
  if (!is.null(x$splits)) {
    # Note: splits$committee is integer, so we can compare directly
    committee_splits <- x$splits[
      x$splits$committee == committee_num,
      ,
      drop = FALSE
    ]
  } else {
    committee_splits <- data.frame()
  }

  # Count rules in this committee
  # Use coefficients to determine number of rules
  # Note: committee and rule columns are character in Cubist
  committee_coefs <- x$coefficients[
    x$coefficients$committee == as.character(committee_num),
    ,
    drop = FALSE
  ]
  rule_nums <- as.integer(unique(committee_coefs$rule))
  num_rules <- length(rule_nums)

  if (num_rules == 0) {
    return(tibble::tibble(
      committee = integer(),
      id = integer(),
      rules = list()
    ))
  }

  # Extract conditions for each rule
  rule_exprs <- lapply(rule_nums, function(rule_num) {
    # Get splits for this rule
    rule_splits <- committee_splits[
      committee_splits$rule == rule_num,
      ,
      drop = FALSE
    ]

    if (nrow(rule_splits) == 0) {
      # No conditions - rule applies to all data
      return(rlang::expr(TRUE))
    }

    # Convert each split to an expression
    split_exprs <- lapply(seq_len(nrow(rule_splits)), function(i) {
      cubist_split_to_expr(rule_splits[i, , drop = FALSE])
    })

    # Combine with AND
    combine_rule_elements(split_exprs)
  })

  tibble::tibble(
    committee = rep(committee_num, num_rules),
    id = rule_nums,
    rules = rule_exprs
  )
}

# Internal helper to convert a single split to an R expression
cubist_split_to_expr <- function(split_row) {
  var_name <- split_row$variable
  # Remove quotes that may be present for factor variables
  var_name <- gsub('"', '', var_name)
  var_sym <- rlang::sym(var_name)

  if (split_row$type == "type2") {
    # Continuous split
    direction <- split_row$dir
    value <- split_row$value

    # Check for missing value condition
    if (is.na(value) && direction == "=") {
      return(rlang::call2("is.na", var_sym))
    }

    # Standard continuous split
    return(rlang::call2(direction, var_sym, value))
  } else if (split_row$type == "type3") {
    # Categorical split
    categories <- split_row$category

    # Parse the categories (remove quotes, split on comma)
    categories <- gsub('"', '', categories)
    categories <- trimws(strsplit(categories, ",")[[1]])

    if (length(categories) == 1) {
      # Single value - use ==
      return(rlang::call2("==", var_sym, categories[1]))
    } else {
      # Multiple values - use %in%
      # Create c(...) call with all categories
      c_call <- rlang::call2("c", !!!categories)
      return(rlang::call2("%in%", var_sym, c_call))
    }
  } else {
    cli::cli_abort(
      "Unknown split type: {split_row$type}",
      call = rlang::caller_env()
    )
  }
}

#' @rdname active_predictors
#' @export
active_predictors.cubist <- function(x, ...) {
  rlang::check_installed("Cubist")

  # Extract active predictors from the model object
  # Need to compute this from splits and coefficients due to a bug in Cubist$vars$used

  # Get variable names from splits (if any)
  split_vars <- if (!is.null(x$splits)) {
    vars <- unique(as.character(x$splits$variable))
    # Remove quotes that may be present for factor variables
    gsub('"', '', vars)
  } else {
    character(0)
  }

  # Get variable names from coefficients (those with non-NA values)
  # Exclude non-predictor columns
  coef_df <- x$coefficients
  exclude_cols <- c("(Intercept)", "committee", "rule", "tmp")
  predictor_cols <- setdiff(names(coef_df), exclude_cols)

  # Find predictors with any non-NA coefficients
  coef_vars <- predictor_cols[sapply(predictor_cols, function(col) {
    any(!is.na(coef_df[[col]]))
  })]

  # Combine splits and coefficient variables
  active_vars <- union(split_vars, coef_vars)

  # Return using lorax constructor
  new_active_predictors(active_vars)
}
