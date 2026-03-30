#' Convert C5.0 model to party object
#'
#' Convert a single tree from a C5.0 decision tree or boosted model to a party
#' object for use with partykit visualization and analysis tools.
#'
#' @param obj A `C5.0` object from the \pkg{C50} package.
#' @param tree Integer specifying which tree to convert (1-based indexing,
#'   default is 1). For single tree models, use `tree = 1`. For boosted models
#'   with `trials > 1`, this selects which boosting iteration to extract.
#' @param data Data.frame containing the training data, including both predictors
#'   and response variable. Required for proper party object creation with fitted
#'   values and node summaries.
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
#' - `forks="n"`: Number of branches (2+ for numeric, can be 4+ for categorical)
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
#' - Trees stored as sequential lines in pre-order (parent, then children)
#' - No indentation used - hierarchy determined by fork counts
#' - Numeric ternary splits: <= threshold, missing, > threshold
#' - Categorical multiway splits: one branch per level group
#'
#' ## Split encoding
#'
#' - Numeric splits: typically binary (<=, >) or ternary (<=, missing, >)
#' - Ternary numeric splits are simplified to binary by omitting the missing branch
#' - Categorical splits: can have 2+ branches, one for each level group
#' - Multiway categorical splits are preserved in the party object
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
#' - Ternary numeric splits simplified to binary (missing value branch omitted)
#' - Rule-based models (`obj$rules != ""`) not supported
#' - Some terminal nodes may have n=0 (empty branches where no observations fall)
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

  # Require data parameter
  if (is.null(data)) {
    cli::cli_abort(
      "{.arg data} is required for {.fn as.party.C5.0}.",
      "i" = "Provide the training data to create a valid party object with fitted values."
    )
  }

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

  # Get original training data first (needed for factor levels)
  orig_data <- validate_and_select_data(
    data,
    var_names,
    preserve_extra = TRUE
  )

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
      obj$levels,
      orig_data
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
      obj$levels,
      orig_data
    )
  } else {
    # Single tree model: parse the entire tree text
    # tree_start_idx not needed since there's only one tree
    root_node <- c5_parse_tree_lines(
      tree_lines,
      var_names,
      obj$levels,
      orig_data
    )
  }

  if (is.null(root_node)) {
    cli::cli_abort(
      "Failed to parse {.pkg C50} tree structure. Tree format may not be supported."
    )
  }

  # Assign sequential IDs
  root_node <- assign_node_ids(root_node)$node

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
# @param data Data.frame with training data (for factor levels)
#
# @return A partynode object or NULL if parsing fails
c5_parse_tree_lines <- function(tree_lines, var_names, class_levels, data) {
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
    class_levels,
    data
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
# @param data Data.frame with training data (for factor levels)
#
# @return List with elements:
#   - node: A partynode object
#   - next_line: Integer, index of next line to process
c5_parse_node_recursive <- function(
  tree_lines,
  line_idx,
  indent_level,
  var_names,
  class_levels,
  data
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

  # Type "0" is terminal, "1"/"2"/"3" are internal splits
  if (node_type == "0") {
    # Terminal node - no info for constparty
    return(list(
      node = partykit::partynode(id = 1L),
      next_line = line_idx + 1L
    ))
  } else if (node_type %in% c("1", "2", "3")) {
    # Internal split node (numeric or categorical)
    split_var <- attrs$att
    split_cut <- as.numeric(attrs$cut)
    forks <- as.numeric(attrs$forks)

    if (is.null(split_var)) {
      cli::cli_abort(
        "Invalid split at line {line_idx}: missing att."
      )
    }

    # Map variable name to index
    varid <- which(var_names == split_var)
    if (length(varid) == 0) {
      cli::cli_abort(
        "Variable {split_var} not found in predictors."
      )
    }

    # Check if we have elts (categorical split with level groups)
    elts_groups <- attrs$elts

    # Create appropriate split
    if (!is.null(elts_groups) && length(elts_groups) > 0) {
      # Categorical split: create partysplit with index vector
      # elts_groups is a character vector where each element contains
      # comma-separated factor levels for one child

      # Get factor levels from data
      var_levels <- levels(data[[split_var]])
      if (is.null(var_levels)) {
        # Not a factor - treat as numeric split
        if (is.null(split_cut) || length(split_cut) == 0 || is.na(split_cut)) {
          split_cut <- 0
        }
        split <- build_partysplit(varid, split_cut, right = FALSE)
      } else {
        # Parse elts groups and create index vector
        # Initialize with 1s (not 0s) so unassigned levels default to child 1
        split_index <- rep(1L, length(var_levels))

        for (child_num in seq_along(elts_groups)) {
          # Split on commas to get individual levels
          levels_in_group <- strsplit(elts_groups[child_num], ",")[[1]]

          # Find matching levels in var_levels
          for (level in levels_in_group) {
            level_idx <- match(level, var_levels)
            if (!is.na(level_idx)) {
              split_index[level_idx] <- child_num
            }
          }
        }

        # Create categorical partysplit
        split <- partykit::partysplit(
          varid = as.integer(varid),
          breaks = NULL,
          index = split_index
        )
      }
    } else {
      # Numeric split
      if (is.null(split_cut) || length(split_cut) == 0 || is.na(split_cut)) {
        split_cut <- 0
      }
      split <- build_partysplit(varid, split_cut, right = FALSE)
    }

    # Parse children
    # C5.0 can have multiple branches:
    # - Numeric splits (type=2): typically 3 forks (<=, missing, >)
    # - Categorical splits (type=1/3): can have many forks (one per group)
    # For numeric ternary splits, we simplify to binary by skipping the middle child
    # For categorical splits, we keep all children to preserve the multiway split

    next_line <- line_idx + 1L

    # Determine number of children
    num_children <- if (is.na(forks)) 2 else as.integer(forks)

    # Check if this is a categorical split (type 1 or 3)
    is_categorical <- node_type %in% c("1", "3")

    # Parse all children first
    all_children <- list()
    for (i in seq_len(num_children)) {
      if (next_line > length(tree_lines)) {
        break
      }

      child_result <- c5_parse_node_recursive(
        tree_lines,
        next_line,
        indent_level + 1,
        var_names,
        class_levels,
        data
      )

      if (is.null(child_result)) {
        break
      }

      all_children[[i]] <- child_result$node
      next_line <- child_result$next_line
    }

    # Decide which children to keep
    if (is_categorical || num_children > 3) {
      # Categorical splits or multi-way splits: keep all children
      children <- all_children
    } else if (num_children == 3) {
      # Numeric ternary split: simplify to binary by keeping first and last
      # (middle child is typically for missing values)
      if (length(all_children) >= 2) {
        children <- list(
          all_children[[1]],
          all_children[[length(all_children)]]
        )
      } else {
        children <- all_children
      }
    } else {
      # Binary split: keep all (should be 2 children)
      children <- all_children
    }

    # Handle edge cases with fewer children
    # C5.0 sometimes creates degenerate splits where one branch may be missing
    if (length(children) == 0) {
      # No children parsed - treat as terminal
      return(list(
        node = partykit::partynode(id = 1L),
        next_line = next_line
      ))
    } else if (length(children) == 1) {
      # Only one child - this is a degenerate case
      # Create a dummy terminal node for the missing child
      dummy_terminal <- partykit::partynode(id = 1L)
      # Add dummy as second child
      children[[2]] <- dummy_terminal
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
# Note: For "elts" keys (categorical splits), each elts group is stored separately
c5_parse_attributes <- function(line) {
  attrs <- list()

  # First, extract elts groups specially (they have comma-separated quoted strings)
  elts_pattern <- 'elts="[^"]*"(,"[^"]*")*'
  elts_matches <- gregexpr(elts_pattern, line, perl = TRUE)
  elts_data <- regmatches(line, elts_matches)[[1]]

  if (length(elts_data) > 0) {
    # Parse each elts group
    elts_groups <- character(length(elts_data))
    for (i in seq_along(elts_data)) {
      # Extract all quoted strings after elts=
      # Remove "elts=" prefix
      group_str <- sub("^elts=", "", elts_data[i])
      # Extract all quoted strings
      quoted_matches <- gregexpr('"([^"]*)"', group_str, perl = TRUE)
      quoted_data <- regmatches(group_str, quoted_matches)[[1]]
      # Remove quotes
      levels_in_group <- gsub('"', '', quoted_data)
      # Store as comma-separated string
      elts_groups[i] <- paste(levels_in_group, collapse = ",")
    }
    attrs$elts <- elts_groups
  }

  # Now parse other key="value" pairs (excluding elts which we already handled)
  kv_pattern <- '(\\w+)="([^"]*)"'
  matches <- gregexpr(kv_pattern, line, perl = TRUE)
  match_data <- regmatches(line, matches)[[1]]

  for (match in match_data) {
    # Skip if this is part of an elts group
    if (grepl("^elts=", match)) {
      next
    }

    # Extract key and value
    parts <- regmatches(match, regexec(kv_pattern, match))[[1]]
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
# @param data Data.frame with training data (for factor levels)
#
# @return Integer line index where the tree starts, or NULL if not found
c5_find_tree_start <- function(
  tree_lines,
  tree_num,
  var_names,
  class_levels,
  data
) {
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
      class_levels = class_levels,
      data = data
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
# @param data Data.frame with training data (for factor levels)
#
# @return A partynode object or NULL if parsing fails
c5_parse_tree_at_index <- function(
  tree_lines,
  start_idx,
  var_names,
  class_levels,
  data
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
    class_levels,
    data
  )

  if (is.null(result)) {
    return(NULL)
  }

  result$node
}

# Extract active variables from C5.0 rules text
#
# @param rules_text Character string containing C5.0 rules
#
# @return Character vector of unique variable names
c5_extract_active_vars_from_rules <- function(rules_text) {
  # Split into lines
  rules_lines <- strsplit(rules_text, "\n")[[1]]

  # Extract att= from condition lines (type="1" or type="2")
  active_vars <- character(0)
  for (line in rules_lines) {
    attrs <- c5_parse_attributes(line)
    if (!is.null(attrs$type) && attrs$type %in% c("1", "2")) {
      if (!is.null(attrs$att)) {
        active_vars <- c(active_vars, attrs$att)
      }
    }
  }

  unique(active_vars)
}

# Find the starting line index for a specific tree in a simple way
#
# @param tree_lines Character vector of all tree lines
# @param tree_num Which tree to find (1-based)
#
# @return Integer line index where the tree starts, or NULL if not found
c5_find_tree_start_simple <- function(tree_lines, tree_num) {
  # Skip to first type= line
  type_lines <- which(grepl("type=", tree_lines))
  if (length(type_lines) == 0) {
    return(NULL)
  }
  current_line <- type_lines[1]
  if (tree_num == 1) {
    return(current_line)
  }

  # Count trees by detecting boundaries (empty lines between trees)
  trees_found <- 1
  in_tree <- TRUE

  for (i in (current_line + 1):length(tree_lines)) {
    line <- tree_lines[i]
    has_type <- grepl("type=", line)
    is_empty <- nchar(trimws(line)) == 0

    if (is_empty) {
      in_tree <- FALSE
    } else if (has_type && !in_tree) {
      trees_found <- trees_found + 1
      in_tree <- TRUE
      if (trees_found == tree_num) {
        return(i)
      }
    }
  }

  NULL
}

# Find the ending line index for a specific tree
#
# @param tree_lines Character vector of all tree lines
# @param start_idx Starting line index of the tree
# @param tree_num Which tree this is (1-based)
# @param num_trials Total number of trials in the model
#
# @return Integer line index where the tree ends
c5_find_tree_end_simple <- function(
  tree_lines,
  start_idx,
  tree_num,
  num_trials
) {
  if (tree_num == num_trials) {
    return(length(tree_lines))
  }

  # Find next tree start
  next_start <- c5_find_tree_start_simple(tree_lines, tree_num + 1)
  if (is.null(next_start)) {
    return(length(tree_lines))
  }

  next_start - 1
}

# Extract active variables from one C5.0 tree
#
# @param tree_lines Character vector of all tree lines
# @param tree_num Which tree to extract from (1-based)
# @param num_trials Total number of trials in the model
#
# @return Character vector of unique variable names
c5_extract_active_vars_one_tree <- function(tree_lines, tree_num, num_trials) {
  # Locate tree boundaries
  if (num_trials > 1) {
    start_idx <- c5_find_tree_start_simple(tree_lines, tree_num)
    if (is.null(start_idx)) {
      return(character(0))
    }
    end_idx <- c5_find_tree_end_simple(
      tree_lines,
      start_idx,
      tree_num,
      num_trials
    )
  } else {
    start_idx <- 1
    end_idx <- length(tree_lines)
  }

  # Extract att= values from type="2" lines (internal splits)
  active_vars <- character(0)
  for (i in start_idx:end_idx) {
    line <- tree_lines[i]
    # Skip empty or invalid lines
    if (length(line) == 0 || is.null(line)) {
      next
    }
    if (is.na(line)) {
      next
    }
    if (nchar(trimws(line)) == 0) {
      next
    }
    attrs <- c5_parse_attributes(line)
    if (!is.null(attrs$type) && attrs$type == "2" && !is.null(attrs$att)) {
      active_vars <- c(active_vars, attrs$att)
    }
  }

  unique(active_vars)
}

# Wrapper to extract active predictors for one tree
#
# @param tree_num Which tree to extract from (1-based)
# @param tree_lines Character vector of all tree lines
# @param num_trials Total number of trials in the model
#
# @return Tibble with active_predictors and tree columns
c5_extract_one <- function(tree_num, tree_lines, num_trials) {
  active_vars <- c5_extract_active_vars_one_tree(
    tree_lines,
    tree_num,
    num_trials
  )
  new_active_predictors(active_vars, tree = tree_num)
}

# ------------------------------------------------------------------------------
# Extract rules from C5.0

# Internal helper: extract rules for one tree from C5.0
c5_extract_rules_one <- function(tree_num, x, data) {
  # Convert to party
  tree_party <- as.party(x, tree = tree_num, data = data)

  # Extract rules using party method
  rules <- extract_rules.party(tree_party)

  # Add tree column
  rules$tree <- tree_num

  rules
}

#' @rdname extract_rules
#' @param tree Integer vector specifying which trees (boosting trials) to
#'   extract rules from. Default is `1L` for the first tree. Values must be
#'   between 1 and the number of actual trials (`x$trials["Actual"]`).
#' @param data Data.frame containing the training data. Required for C5.0
#'   models to properly parse tree structure with correct factor levels.
#' @export
extract_rules.C5.0 <- function(x, tree = 1L, data = NULL, ...) {
  rlang::check_installed("C50")

  # Require data parameter
  if (is.null(data)) {
    cli::cli_abort(
      "{.arg data} is required for {.fn extract_rules.C5.0}.",
      "i" = "Provide the training data to extract rules correctly."
    )
  }

  # Validate tree argument
  if (!is.numeric(tree) || !all(tree == as.integer(tree))) {
    cli::cli_abort(
      "{.arg tree} must be an integer vector, not {.obj_type_friendly {tree}}.",
      call = rlang::caller_env()
    )
  }

  tree <- as.integer(tree)

  # Get number of trials
  num_trials <- x$trials["Actual"]

  # Validate tree range
  if (any(tree < 1L) || any(tree > num_trials)) {
    cli::cli_abort(
      "{.arg tree} values must be between 1 and {num_trials}.",
      call = rlang::caller_env()
    )
  }

  # Extract for each tree
  results <- lapply(tree, c5_extract_rules_one, x = x, data = data)

  # Combine and sort by tree then id
  dplyr::bind_rows(results) |>
    dplyr::arrange(tree, id)
}

# ------------------------------------------------------------------------------

#' @rdname active_predictors
#' @param tree Integer vector specifying which trees (boosting trials) to
#'   extract active predictors from. Default is `1L` for the first tree.
#'   Values must be between 1 and the number of actual trials
#'   (`x$trials["Actual"]`). Duplicate values are automatically removed.
#'   This parameter is ignored for rule-based models.
#' @export
active_predictors.C5.0 <- function(x, tree = 1L, ...) {
  rlang::check_installed("C50")

  # Detect model type - rule-based models have non-empty rules
  is_rule_model <- !is.null(x$rules) && nchar(x$rules) > 0

  if (is_rule_model) {
    # Extract from rules
    active_vars <- c5_extract_active_vars_from_rules(x$rules)
    return(new_active_predictors(active_vars))
  }

  # Tree-based model - validate tree argument
  if (!is.numeric(tree) || !all(tree == as.integer(tree))) {
    cli::cli_abort(
      "{.arg tree} must be an integer vector, not {.obj_type_friendly {tree}}.",
      call = rlang::caller_env()
    )
  }

  tree <- as.integer(tree)
  tree <- unique(tree)

  num_trials <- as.integer(x$trials["Actual"])
  if (any(tree < 1L) || any(tree > num_trials)) {
    cli::cli_abort(
      "{.arg tree} values must be between 1 and {num_trials}, not {tree}.",
      call = rlang::caller_env()
    )
  }

  # Split tree text into lines and remove empty ones
  tree_lines <- strsplit(x$tree, "\n")[[1]]
  tree_lines <- tree_lines[nchar(trimws(tree_lines)) > 0]

  # Extract active predictors for each requested tree
  results <- lapply(
    tree,
    c5_extract_one,
    tree_lines = tree_lines,
    num_trials = num_trials
  )

  # Combine results and sort by tree
  dplyr::bind_rows(results) |>
    dplyr::arrange(tree)
}
