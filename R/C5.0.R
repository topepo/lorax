#' Convert C5.0 model to party object
#'
#' Convert a single tree from a C5.0 decision tree or boosted model to a party
#' object for use with partykit visualization and analysis tools.
#'
#' @param obj A `C5.0` object from the \pkg{C50} package.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). For single tree models, use `tree = 1`. For boosted models
#'   with `trials > 1`, this selects which boosting iteration to extract.
#' @param data Optional data.frame containing the training data. If NULL,
#'   will attempt to reconstruct from model, or create a placeholder.
#'   Providing data enables full party functionality.
#' @param ... Not currently used.
#'
#' @return A `party` object from the \pkg{partykit} package.
#'
#' @details
#' ## C5.0 tree storage format
#'
#' The \pkg{C50} package stores trees in a custom text format in `obj$tree`. This
#' format uses indented lines with key-value pairs:
#' - `type="2"`: Internal node with split
#' - `type="0"`: Terminal/leaf node
#' - `att="VariableName"`: Attribute/variable to split on
#' - `forks="3"`: Number of branches (typically 2 for binary, 3 for ternary)
#' - `cut="threshold"`: Numeric threshold for split
#' - `class="ClassName"`: Predicted class
#' - `freq="n1,n2,n3"`: Frequency of each class at node
#'
#' ## Boosting and trials
#'
#' - Single tree models (`trials = 1`): Only `tree = 1` is valid
#' - Boosted models (`trials > 1`): Multiple sequential trees available
#' - The `tree` parameter maps to trial/iteration number
#' - Each boosting trial produces one tree
#'
#' ## Tree structure
#'
#' - Trees use indentation to indicate hierarchy (similar to Python)
#' - Child nodes are indented relative to parent
#' - Ternary splits: <= threshold, missing, > threshold
#' - Binary splits: <= threshold vs > threshold
#'
#' ## Split encoding
#'
#' - For numeric: typically left/low when <= threshold, right/high when > threshold
#' - C5.0 may use ternary splits with separate handling of missing values
#' - \pkg{partykit} uses binary splits, so we simplify to binary when needed
#'
#' ## Variable names
#'
#' - `obj$predictors` contains ordered list of predictor variable names
#' - `att` attribute in tree text references these by name
#' - Map to 1-based indices for \pkg{partykit}
#'
#' ## Important limitations
#'
#' - \pkg{C50}'s text format is complex and may vary by version
#' - Ternary splits with missing value branches simplified to binary
#' - Rule-based models (`obj$rules != ""`) not supported
#' - Categorical variable splits may need special handling
#'
#' @examples
#' if (rlang::is_installed(c("C50", "palmerpenguins"))) {
#'   data(penguins, package = "palmerpenguins")
#'   penguins <- na.omit(penguins)
#'
#'   # Single tree model
#'   set.seed(2847)
#'   c5_tree <- C50::C5.0(species ~ ., data = penguins)
#'   party_tree <- as.party(c5_tree, tree = 1L, data = penguins)
#'   print(party_tree)
#'   plot(party_tree)
#'
#'   # Boosted model with multiple trials
#'   set.seed(5193)
#'   c5_boost <- C50::C5.0(species ~ ., data = penguins, trials = 3)
#'   # Extract first boosting iteration
#'   party_tree1 <- as.party(c5_boost, tree = 1L, data = penguins)
#'   # Extract third boosting iteration
#'   party_tree3 <- as.party(c5_boost, tree = 3L, data = penguins)
#' }
#'
#' @export
as.party.C5.0 <- function(obj, tree = 1L, data = NULL, ...) {
  rlang::check_installed("C50")
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

  # Check if model uses rules (not supported)
  if (!is.null(obj$rules) && nchar(obj$rules) > 0) {
    cli::cli_abort(
      "{.pkg C50} rule-based models are not supported. Use tree-based models only."
    )
  }

  # Validate tree against trials
  num_trials <- obj$trials["Actual"]
  if (tree > num_trials) {
    cli::cli_warn(
      "{.arg tree} = {tree} exceeds number of trials ({num_trials}). Using {.arg tree} = {num_trials} instead."
    )
    tree <- as.integer(num_trials)
  }

  # Parse tree text format
  tree_text <- obj$tree
  if (is.null(tree_text) || nchar(tree_text) == 0) {
    cli::cli_abort(
      "{.pkg C50} model does not contain tree structure."
    )
  }

  # Parse the C5.0 tree format
  tree_lines <- strsplit(tree_text, "\n")[[1]]

  # Get variable names
  var_names <- obj$predictors

  # Parse tree structure based on model type
  # - Boosted models (num_trials > 1): Multiple trees concatenated in tree_text
  #   Need to locate and extract the specific tree requested
  # - Single tree models (num_trials == 1): Tree text contains only one tree
  #   Can parse directly without needing to locate specific tree position
  if (num_trials > 1) {
    # Boosted model: locate the start of the requested tree
    # Skip header lines and previous trees to find where our tree begins
    tree_start_idx <- c5_find_tree_start(
      tree_lines,
      tree,
      var_names,
      obj$levels
    )

    if (is.null(tree_start_idx)) {
      cli::cli_abort(
        "Could not locate tree {tree} in boosted {.pkg C50} model tree structure."
      )
    }

    # Parse from the located starting position
    root_node <- c5_parse_tree_at_index(
      tree_lines,
      tree_start_idx,
      var_names,
      obj$levels
    )
  } else {
    # Single tree model: parse the entire tree text
    # tree_start_idx not needed since there's only one tree
    root_node <- c5_parse_tree_lines(tree_lines, var_names, obj$levels)
  }

  if (is.null(root_node)) {
    cli::cli_abort(
      "Failed to parse {.pkg C50} tree structure. Tree format may not be supported."
    )
  }

  # Assign sequential IDs
  root_node <- assign_node_ids(root_node)$node

  # Get original training data
  orig_data <- NULL
  response_name <- NULL

  if (!is.null(data)) {
    # Data parameter provided - use it
    orig_data <- validate_and_select_data(
      data,
      var_names,
      preserve_extra = TRUE
    )
  } else {
    # Try to extract from model call
    orig_data <- extract_data_from_call(
      obj,
      data_param = "data",
      eval_env = environment(obj$Terms)
    )
    if (!is.null(orig_data)) {
      orig_data <- validate_and_select_data(
        orig_data,
        var_names,
        preserve_extra = TRUE
      )
    }
  }

  # If no data available, create 0-row placeholder
  if (is.null(orig_data)) {
    orig_data <- reconstruct_data(var_names, n_obs = 0)
  }

  # Create terms object
  terms <- obj$Terms
  if (is.null(terms) && nrow(orig_data) > 0) {
    formula <- stats::as.formula(paste(
      "~",
      paste(var_names, collapse = " + ")
    ))
    terms <- stats::terms(formula, data = orig_data)
  }

  # Create fitted values structure
  fitted <- NULL
  if (!is.null(orig_data) && nrow(orig_data) > 0) {
    # Compute which terminal node each observation belongs to
    fitted_ids <- compute_fitted_node_ids(
      root_node,
      orig_data[, var_names, drop = FALSE]
    )

    # Get response from data
    # C5.0 has Terms which we can use to extract response
    response <- NULL
    if (!is.null(obj$Terms)) {
      # Get response name from terms
      response_name <- all.vars(obj$Terms)[attr(obj$Terms, "response")]
      if (length(response_name) > 0 && response_name %in% names(orig_data)) {
        response <- orig_data[[response_name]]
      }
    }

    fitted <- create_fitted_dataframe(fitted_ids, response)
  }

  # Create party object
  party_obj <- create_party_object(
    node = root_node,
    data = orig_data,
    fitted = fitted,
    terms = terms,
    info = list(method = "C5.0", tree = tree)
  )

  # Set class to constparty if we have response data
  if (!is.null(fitted) && "(response)" %in% names(fitted)) {
    class(party_obj) <- c("constparty", "party")
  }

  party_obj
}

# Internal helper to parse C5.0 tree text format
#
# C5.0 stores trees as indented text with key="value" pairs.
# This parser handles basic tree structures.
#
# @param tree_lines Character vector of tree text lines
# @param var_names Character vector of variable names
# @param class_levels Character vector of class labels
#
# @return A partynode object or NULL if parsing fails
c5_parse_tree_lines <- function(tree_lines, var_names, class_levels) {
  # Remove empty lines and trim
  tree_lines <- tree_lines[nchar(trimws(tree_lines)) > 0]

  # Find the actual tree start (after header)
  start_idx <- 1
  for (i in seq_along(tree_lines)) {
    if (grepl("type=", tree_lines[i])) {
      start_idx <- i
      break
    }
  }

  if (start_idx > length(tree_lines)) {
    return(NULL)
  }

  # Parse recursively from start
  result <- c5_parse_node_recursive(
    tree_lines,
    start_idx,
    indent_level = 0,
    var_names,
    class_levels
  )

  result$node
}

# Internal helper to recursively parse C5.0 tree nodes
#
# @param tree_lines Character vector of all tree lines
# @param line_idx Current line index (1-based)
# @param indent_level Expected indentation level
# @param var_names Character vector of variable names
# @param class_levels Character vector of class labels
#
# @return List with elements:
#   - node: A partynode object
#   - next_line: Integer, index of next line to process
c5_parse_node_recursive <- function(
  tree_lines,
  line_idx,
  indent_level,
  var_names,
  class_levels
) {
  if (line_idx > length(tree_lines)) {
    return(NULL)
  }

  line <- tree_lines[line_idx]

  # Parse attributes from line (key="value" pairs)
  attrs <- c5_parse_attributes(line)

  # Determine node type
  node_type <- attrs$type
  if (is.null(node_type)) {
    return(NULL)
  }

  # Type "0" is terminal, "2" is internal split
  if (node_type == "0") {
    # Terminal node - no info for constparty
    return(list(
      node = partykit::partynode(id = 1L),
      next_line = line_idx + 1L
    ))
  } else if (node_type == "2") {
    # Internal split node
    split_var <- attrs$att
    split_cut <- as.numeric(attrs$cut)
    forks <- as.numeric(attrs$forks)

    if (is.null(split_var) || is.na(split_cut)) {
      cli::cli_abort(
        "Invalid split at line {line_idx}: missing att or cut."
      )
    }

    # Map variable name to index
    varid <- which(var_names == split_var)
    if (length(varid) == 0) {
      cli::cli_abort(
        "Variable {split_var} not found in predictors."
      )
    }

    # Create split (C5.0 typically uses <=)
    split <- build_partysplit(varid, split_cut, right = FALSE)

    # Parse children
    # C5.0 uses ternary splits (forks=3) but we simplify to binary
    # Left child: <= threshold, Right child: > threshold
    # (Skip middle child which is typically for missing values)

    next_line <- line_idx + 1L
    children <- list()

    # Expect 2 or 3 child nodes depending on forks
    num_children <- if (is.na(forks)) 2 else as.integer(forks)

    for (i in seq_len(num_children)) {
      if (next_line > length(tree_lines)) {
        break
      }

      child_result <- c5_parse_node_recursive(
        tree_lines,
        next_line,
        indent_level + 1,
        var_names,
        class_levels
      )

      if (is.null(child_result)) {
        break
      }

      # Only keep first and last child for binary split
      if (i == 1 || i == num_children) {
        children[[length(children) + 1]] <- child_result$node
      }

      next_line <- child_result$next_line
    }

    # Ensure we have exactly 2 children for binary split
    if (length(children) != 2) {
      cli::cli_abort(
        "Expected 2 children for binary split, got {length(children)}."
      )
    }

    return(list(
      node = partykit::partynode(
        id = 1L,
        split = split,
        kids = children
      ),
      next_line = next_line
    ))
  } else {
    # Unknown node type
    return(NULL)
  }
}

# Internal helper to parse key="value" attributes from C5.0 tree line
#
# @param line Character string containing key="value" pairs
#
# @return Named list of attribute values
c5_parse_attributes <- function(line) {
  attrs <- list()

  # Match all key="value" pairs
  matches <- gregexpr('(\\w+)="([^"]*)"', line, perl = TRUE)
  match_data <- regmatches(line, matches)[[1]]

  for (match in match_data) {
    # Extract key and value
    parts <- regmatches(match, regexec('(\\w+)="([^"]*)"', match))[[1]]
    if (length(parts) == 3) {
      key <- parts[2]
      value <- parts[3]
      attrs[[key]] <- value
    }
  }

  attrs
}

# Find the starting line index for a specific tree in boosted C5.0 models
#
# @param tree_lines Character vector of all tree lines
# @param tree_num Which tree to find (1-based)
# @param var_names Character vector of variable names (needed for parsing)
# @param class_levels Character vector of class labels (needed for parsing)
#
# @return Integer line index where the tree starts, or NULL if not found
c5_find_tree_start <- function(tree_lines, tree_num, var_names, class_levels) {
  # Skip header lines (id and entries lines)
  current_line <- 1
  while (
    current_line <= length(tree_lines) &&
      !grepl("type=", tree_lines[current_line])
  ) {
    current_line <- current_line + 1
  }

  if (current_line > length(tree_lines)) {
    return(NULL)
  }

  # If we want tree 1, return the first tree start
  if (tree_num == 1) {
    return(current_line)
  }

  # Otherwise, skip through (tree_num - 1) trees
  trees_skipped <- 0
  while (trees_skipped < (tree_num - 1) && current_line <= length(tree_lines)) {
    # Parse one tree to find where it ends
    result <- c5_parse_node_recursive(
      tree_lines,
      current_line,
      indent_level = 0,
      var_names = var_names,
      class_levels = class_levels
    )

    if (is.null(result)) {
      return(NULL)
    }

    trees_skipped <- trees_skipped + 1
    current_line <- result$next_line

    # Skip any empty lines between trees
    while (
      current_line <= length(tree_lines) &&
        nchar(trimws(tree_lines[current_line])) == 0
    ) {
      current_line <- current_line + 1
    }
  }

  if (
    current_line > length(tree_lines) ||
      !grepl("type=", tree_lines[current_line])
  ) {
    return(NULL)
  }

  current_line
}

# Parse a C5.0 tree starting from a specific line index
#
# @param tree_lines Character vector of all tree lines
# @param start_idx Line index to start parsing from (1-based)
# @param var_names Character vector of variable names
# @param class_levels Character vector of class labels
#
# @return A partynode object or NULL if parsing fails
c5_parse_tree_at_index <- function(
  tree_lines,
  start_idx,
  var_names,
  class_levels
) {
  if (start_idx > length(tree_lines)) {
    return(NULL)
  }

  # Parse recursively from start index
  result <- c5_parse_node_recursive(
    tree_lines,
    start_idx,
    indent_level = 0,
    var_names,
    class_levels
  )

  if (is.null(result)) {
    return(NULL)
  }

  result$node
}
