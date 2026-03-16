# Developer Guide: Implementing as.party() Methods for New Model Types

## Overview

This guide explains how to implement `as.party()` methods for new tree-based model types in the lorax package. It covers the structure of party objects, the conversion process, and testing strategies.

## Understanding Party Objects

### Core Structure

A `party` object from partykit contains:

```r
party_object <- list(
  node = partynode_object,    # The tree structure
  data = data.frame(...),     # Training data
  fitted = data.frame(...),   # Fitted values and response (optional)
  terms = terms_object,       # Formula terms (optional)
  info = list(...)           # Additional metadata (optional)
)
class(party_object) <- "party"  # or c("constparty", "party")
```

### Node Structure (partynode)

The `node` element represents the tree recursively:

```r
# Terminal node (leaf)
terminal_node <- partykit::partynode(
  id = 5L                      # Sequential integer ID
)

# Internal node (split)
internal_node <- partykit::partynode(
  id = 1L,                     # Sequential integer ID
  split = partysplit_object,   # How to split
  kids = list(                 # Child nodes
    partynode(id = 2L),
    partynode(id = 3L)
  )
)
```

**Key properties**:
- IDs must be sequential integers starting from 1
- Root node is always ID 1
- IDs assigned in pre-order traversal (parent before children)
- Terminal nodes have no `split` or `kids`
- Internal nodes must have both `split` and `kids`

### Split Structure (partysplit)

Splits define how to partition data at internal nodes:

```r
# Numeric split: variable < threshold or variable >= threshold
numeric_split <- partykit::partysplit(
  varid = 3L,              # Column index in data (1-based)
  breaks = 5.5,            # Threshold value
  index = 1:2,             # Child indices for left/right
  right = TRUE,            # If TRUE: <, >=; if FALSE: <=, >
  prob = NULL,             # Probability for each child (optional)
  info = NULL              # Additional info (optional)
)

# Categorical split: variable %in% levels
categorical_split <- partykit::partysplit(
  varid = 2L,
  breaks = NULL,           # No threshold for categorical
  index = c(1L, 2L, 1L, 2L),  # Map each level to a child
  right = TRUE,
  prob = NULL,
  info = NULL
)
```

**Key properties**:
- `varid` is 1-based column index in the data
- `right = TRUE`: left child when `< breaks`, right when `>= breaks`
- `right = FALSE`: left child when `<= breaks`, right when `> breaks`
- `index` length equals 2 for binary numeric splits
- `index` length equals number of factor levels for categorical splits
- Most tree models use binary splits (2 children)

### Data Structure

The `data` element should contain:

```r
data <- data.frame(
  predictor1 = numeric_or_factor_vector,
  predictor2 = numeric_or_factor_vector,
  ...
  response = response_vector  # Optional but recommended
)
```

**Key properties**:
- Column order matches variable IDs in splits (column 1 = varid 1)
- Should include all predictors used in the tree
- Factor variables should retain their levels
- Response variable included when available

### Fitted Values Structure

The `fitted` element maps observations to terminal nodes:

```r
fitted <- data.frame(
  "(fitted)" = c(2L, 2L, 3L, 2L, 3L, ...),  # Terminal node IDs
  "(response)" = response_vector,            # Optional
  check.names = FALSE
)
```

**Key properties**:
- One row per observation in training data
- `(fitted)` column contains terminal node IDs
- `(response)` column optional but recommended for constparty
- Column names use parentheses to avoid conflicts
- Use `check.names = FALSE` to preserve special column names

### Terms Object

The `terms` object represents the model formula:

```r
# Extract from model if available
terms <- model$terms

# Or create from formula
formula <- stats::as.formula("response ~ pred1 + pred2")
terms <- stats::terms(formula, data = data)

# Or create minimal version
terms <- stats::terms(~ pred1 + pred2, data = data)
```

**Key properties**:
- Used by predict methods and other partykit functions
- Can be NULL if not available
- Should match the predictors in the data

### Info List

The `info` element stores model metadata:

```r
info <- list(
  method = "randomForest",     # Model type identifier
  tree = 1L,                   # Which tree from ensemble
  ntree = 500L,                # Total trees in model
  ...                          # Other model-specific info
)
```

## The Conversion Process

### Step 1: Extract Tree Structure from Model

Each model package stores trees differently. Common formats:

**Tabular format** (ranger, randomForest, xgboost, lightgbm):
```r
# Tree as data.frame with parent-child relationships
tree_df <- data.frame(
  node_id = c(0, 1, 2, 3, 4),
  left_child = c(1, 3, -1, -1, -1),
  right_child = c(2, 4, -1, -1, -1),
  split_var = c(0, 1, -1, -1, -1),
  split_val = c(5.5, 3.2, NA, NA, NA),
  leaf_value = c(NA, NA, 1.5, 2.3, 1.8)
)
```

**Text format** (C5.0):
```r
# Tree as structured text
tree_text <- "
type=\"2\" att=\"Sepal.Length\" cut=\"5.5\"
type=\"0\" class=\"setosa\"
type=\"0\" class=\"versicolor\"
"
```

**Nested format** (BART):
```r
# Tree as nested list or matrix
tree_matrix <- matrix(
  c(var, value, left_child, right_child, ...),
  ncol = 5
)
```

### Step 2: Build partynode Tree

Convert model format to recursive partynode structure:

```r
# Recursive builder for tabular format
build_node <- function(tree_df, node_id) {
  row <- tree_df[tree_df$node_id == node_id, ]

  # Check if terminal
  if (row$left_child < 0) {
    return(partykit::partynode(id = 1L))  # ID assigned later
  }

  # Build split
  split <- partykit::partysplit(
    varid = row$split_var + 1L,  # Convert 0-based to 1-based
    breaks = row$split_val,
    index = 1:2,
    right = TRUE
  )

  # Recursively build children
  left <- build_node(tree_df, row$left_child)
  right <- build_node(tree_df, row$right_child)

  partykit::partynode(
    id = 1L,  # ID assigned later
    split = split,
    kids = list(left, right)
  )
}

root <- build_node(tree_df, root_node_id)
```

### Step 3: Assign Sequential Node IDs

Party objects require sequential IDs in pre-order:

```r
assign_node_ids <- function(node, next_id = 1L) {
  node$id <- next_id
  next_id <- next_id + 1L

  kids <- partykit::kids_node(node)
  if (!is.null(kids) && length(kids) > 0) {
    new_kids <- list()
    for (i in seq_along(kids)) {
      result <- assign_node_ids(kids[[i]], next_id)
      new_kids[[i]] <- result$node
      next_id <- result$next_id
    }
    node$kids <- new_kids
  }

  list(node = node, next_id = next_id)
}

root_with_ids <- assign_node_ids(root)$node
```

### Step 4: Extract or Reconstruct Training Data

Priority order for obtaining data:

1. **User-provided `data` parameter** (most reliable)
2. **Extract from model object** (if stored)
3. **Reconstruct from model metadata** (minimal placeholder)

```r
# Option 1: User provides (preferred)
if (!is.null(data)) {
  orig_data <- data[, var_names, drop = FALSE]
}

# Option 2: Extract from model
else if (!is.null(model$call$data)) {
  orig_data <- try(eval(model$call$data), silent = TRUE)
  if (!inherits(orig_data, "try-error")) {
    orig_data <- orig_data[, var_names, drop = FALSE]
  }
}

# Option 3: Reconstruct minimal
else {
  orig_data <- reconstruct_data(var_names, n_obs = 0)
}
```

### Step 5: Compute Fitted Node IDs

Determine which terminal node each observation belongs to:

```r
compute_fitted_node_ids <- function(node, data) {
  node_ids <- integer(nrow(data))

  for (i in seq_len(nrow(data))) {
    node_ids[i] <- traverse_to_terminal(node, data[i, , drop = FALSE])
  }

  node_ids
}

traverse_to_terminal <- function(node, obs) {
  kids <- partykit::kids_node(node)
  if (is.null(kids) || length(kids) == 0) {
    return(partykit::id_node(node))
  }

  split <- partykit::split_node(node)
  varid <- partykit::varid_split(split)
  value <- obs[[varid]]

  if (is.na(value)) {
    child_idx <- 1L  # Default to left
  } else {
    breaks <- partykit::breaks_split(split)
    right <- partykit::right_split(split)

    if (!is.null(breaks)) {
      # Numeric split
      if (right) {
        child_idx <- if (value < breaks) 1L else 2L
      } else {
        child_idx <- if (value <= breaks) 1L else 2L
      }
    } else {
      # Categorical split
      index <- partykit::index_split(split)
      level_idx <- match(as.character(value), levels(obs[[varid]]))
      child_idx <- if (!is.na(level_idx)) index[level_idx] else 1L
    }
  }

  traverse_to_terminal(kids[[child_idx]], obs)
}
```

### Step 6: Create the Party Object

Assemble all components:

```r
# Create fitted data.frame
fitted <- NULL
if (nrow(orig_data) > 0) {
  fitted_ids <- compute_fitted_node_ids(root_with_ids, orig_data)

  fitted <- data.frame(
    "(fitted)" = fitted_ids,
    check.names = FALSE
  )

  # Add response if available
  if (!is.null(response)) {
    fitted[["(response)"]] <- response
  }
}

# Create party object
party_obj <- partykit::party(
  node = root_with_ids,
  data = orig_data,
  fitted = fitted,
  terms = terms,
  info = list(method = "ModelType", tree = tree)
)

# Set class
if (!is.null(fitted) && "(response)" %in% names(fitted)) {
  class(party_obj) <- c("constparty", "party")
} else {
  class(party_obj) <- "party"
}

party_obj
```

## Implementation Template

### Basic Method Structure

```r
#' Convert ModelType to party object
#'
#' @param obj A ModelType object
#' @param tree Integer specifying which tree to convert (1-based)
#' @param data Optional data.frame with training data
#' @param ... Additional arguments (currently unused)
#'
#' @return A party object from partykit
#'
#' @export
as.party.ModelType <- function(obj, tree = 1L, data = NULL, ...) {
  # 1. Validate parameters
  validate_tree_param(tree, obj)
  validate_data_param(data, obj)

  # 2. Extract tree structure from model
  tree_structure <- extract_tree_structure(obj, tree)

  # 3. Build partynode tree
  root_node <- build_partynode(tree_structure, obj)

  # 4. Assign sequential IDs
  root_node <- assign_node_ids(root_node)$node

  # 5. Get or reconstruct training data
  orig_data <- get_training_data(obj, data)

  # 6. Create terms object
  terms <- get_terms(obj, orig_data)

  # 7. Compute fitted values
  fitted <- compute_fitted_values(root_node, orig_data, obj)

  # 8. Create and return party object
  create_party_object(
    node = root_node,
    data = orig_data,
    fitted = fitted,
    terms = terms,
    info = list(method = "ModelType", tree = tree)
  )
}
```

### Helper Functions

**Parameter validation**:
```r
validate_tree_param <- function(tree, obj, call = rlang::caller_env()) {
  if (!is.numeric(tree) || length(tree) != 1 || tree != as.integer(tree)) {
    cli::cli_abort(
      "{.arg tree} must be a single integer, not {.obj_type_friendly {tree}}.",
      call = call
    )
  }

  tree <- as.integer(tree)

  if (tree < 1) {
    cli::cli_abort("{.arg tree} must be >= 1, not {tree}.", call = call)
  }

  max_tree <- get_max_tree_number(obj)
  if (tree > max_tree) {
    cli::cli_abort(
      "{.arg tree} must be between 1 and {max_tree}, not {tree}.",
      call = call
    )
  }

  invisible(tree)
}
```

**Data reconstruction**:
```r
reconstruct_data <- function(var_names, var_types = NULL, n_obs = 0L) {
  if (is.null(var_types)) {
    var_types <- rep("numeric", length(var_names))
  }

  df <- as.data.frame(matrix(
    nrow = n_obs,
    ncol = length(var_names),
    dimnames = list(NULL, var_names)
  ))

  for (i in seq_along(var_names)) {
    if (var_types[i] == "factor") {
      df[[i]] <- factor(character(n_obs))
    } else if (var_types[i] == "integer") {
      df[[i]] <- integer(n_obs)
    } else {
      df[[i]] <- numeric(n_obs)
    }
  }

  df
}
```

**Split building**:
```r
build_partysplit <- function(varid, threshold, right = TRUE) {
  if (is.na(varid) || length(varid) == 0) {
    cli::cli_abort("Invalid varid for partysplit: {.val {varid}}")
  }

  if (is.na(threshold) || length(threshold) == 0 || !is.numeric(threshold)) {
    cli::cli_abort("Invalid threshold for partysplit: {.val {threshold}}")
  }

  partykit::partysplit(
    varid = as.integer(varid),
    breaks = as.numeric(threshold),
    index = 1:2,
    right = right
  )
}
```

## Model-Specific Considerations

### Tabular Tree Format (randomForest, ranger, xgboost, lightgbm)

**Challenge**: Different indexing conventions (0-based vs 1-based)

**Solution**: Always convert to 1-based for partykit:
```r
# XGBoost/LightGBM: 0-based node IDs, 0-based variable indices
varid_1based <- xgb_varid + 1L
node_id_1based <- xgb_node_id + 1L

# randomForest/ranger: 1-based, but may use 0 to indicate missing child
is_terminal <- (left_child == 0) && (right_child == 0)
```

**Challenge**: Determining if node is terminal

**Solution**: Check child indicators:
```r
# 0-based systems (xgboost, lightgbm)
is_terminal <- (left_child < 0) && (right_child < 0)

# 1-based systems (randomForest, ranger)
is_terminal <- (is.na(left_child) || left_child == 0) &&
               (is.na(right_child) || right_child == 0)

# Or use explicit leaf indicator column
is_terminal <- (is_leaf == TRUE) || (status == -1)
```

### Text-Based Format (C5.0)

**Challenge**: Parsing hierarchical text structure

**Solution**: Recursive line-by-line parser:
```r
parse_tree_recursive <- function(lines, line_idx, indent_level) {
  line <- lines[line_idx]
  attrs <- parse_attributes(line)  # Extract key="value" pairs

  if (attrs$type == "0") {
    # Terminal node
    return(list(
      node = partykit::partynode(id = 1L),
      next_line = line_idx + 1L
    ))
  }

  # Internal node - parse split
  split <- build_partysplit(
    varid = which(var_names == attrs$att),
    threshold = as.numeric(attrs$cut),
    right = FALSE
  )

  # Parse children
  next_line <- line_idx + 1L
  left_result <- parse_tree_recursive(lines, next_line, indent_level + 1)
  right_result <- parse_tree_recursive(
    lines,
    left_result$next_line,
    indent_level + 1
  )

  list(
    node = partykit::partynode(
      id = 1L,
      split = split,
      kids = list(left_result$node, right_result$node)
    ),
    next_line = right_result$next_line
  )
}
```

### Matrix Format (BART)

**Challenge**: Trees stored as matrices with special encoding

**Solution**: Column-based extraction:
```r
# BART matrix columns: var, value, left_child, right_child
for (node_idx in seq_len(nrow(tree_matrix))) {
  var_idx <- tree_matrix[node_idx, "var"]

  if (var_idx == -1) {
    # Terminal node
    nodes[[node_idx]] <- partykit::partynode(id = 1L)
  } else {
    # Internal node
    threshold <- tree_matrix[node_idx, "value"]
    left_idx <- tree_matrix[node_idx, "left"]
    right_idx <- tree_matrix[node_idx, "right"]

    # Build after all nodes created
    nodes[[node_idx]] <- partykit::partynode(
      id = 1L,
      split = build_partysplit(var_idx + 1L, threshold),
      kids = list(nodes[[left_idx]], nodes[[right_idx]])
    )
  }
}
```

### Boosted/Ensemble Models

**Challenge**: Multiple trees per model

**Solution**: Tree indexing scheme:
```r
# XGBoost multi-class: trees organized by round then class
# For 3 classes, 10 rounds: 30 total trees
# Tree 1: Round 1, Class 1
# Tree 2: Round 1, Class 2
# Tree 3: Round 1, Class 3
# Tree 4: Round 2, Class 1
# ...

extract_tree_for_index <- function(model, tree_num) {
  num_class <- model$params$num_class
  round_num <- ceiling(tree_num / num_class)
  class_num <- ((tree_num - 1) %% num_class) + 1

  # Extract tree for specific round and class
  ...
}
```

```r
# C5.0 boosted: sequential trials
# Tree 1: Trial 1
# Tree 2: Trial 2
# ...

# Text contains multiple trees concatenated
tree_start <- find_tree_start(tree_lines, tree_num)
tree_node <- parse_tree_from_line(tree_lines, tree_start)
```

### Factor Variables

**Challenge**: Models encode factors as numeric internally

**Solution**: Preserve factor information:
```r
# Store factor levels with data
if (is.factor(data[[var_idx]])) {
  # Model uses numeric encoding, but preserve in party data
  orig_data[[var_idx]] <- factor(
    orig_data[[var_idx]],
    levels = levels(training_data[[var_idx]])
  )
}

# In traverse_to_terminal, handle factor comparison
if (is.factor(obs[[varid]])) {
  # Convert to numeric for comparison with threshold
  numeric_value <- as.numeric(obs[[varid]])
  child_idx <- if (numeric_value < breaks) 1L else 2L
}
```

## Testing New Implementations

### Unit Test Structure

Create tests in `tests/testthat/test-ModelType.R`:

```r
test_that("as.party.ModelType returns valid party object", {
  skip_if_not_installed("modelpkg")

  # Create simple model
  model <- modelpkg::fit_function(y ~ ., data = iris, ntree = 5)

  # Convert to party
  party_obj <- as.party(model, tree = 1, data = iris)

  # Basic structure checks
  expect_s3_class(party_obj, "party")
  expect_s3_class(party_obj$node, "partynode")
  expect_true(is.data.frame(party_obj$data))
})
```

### Essential Test Cases

**1. Parameter validation**:
```r
test_that("as.party.ModelType validates tree parameter", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(y ~ ., data = iris, ntree = 5)

  expect_snapshot(as.party(model, tree = 0), error = TRUE)
  expect_snapshot(as.party(model, tree = 10), error = TRUE)
  expect_snapshot(as.party(model, tree = c(1, 2)), error = TRUE)
  expect_snapshot(as.party(model, tree = "1"), error = TRUE)
})
```

**2. Node structure**:
```r
test_that("as.party.ModelType creates valid node structure", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(y ~ ., data = iris, ntree = 5)
  party_obj <- as.party(model, tree = 1, data = iris)

  # Check node IDs are sequential starting from 1
  node_ids <- partykit::nodeids(party_obj)
  expect_equal(min(node_ids), 1L)
  expect_equal(node_ids, seq_along(node_ids))

  # Check root is ID 1
  expect_equal(partykit::id_node(party_obj$node), 1L)

  # Check all terminal nodes have no children
  terminal_ids <- partykit::nodeids(party_obj, terminal = TRUE)
  for (tid in terminal_ids) {
    node <- party_obj[tid]
    expect_null(partykit::kids_node(node))
  }
})
```

**3. Split structure**:
```r
test_that("as.party.ModelType creates valid splits", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(y ~ ., data = iris, ntree = 5)
  party_obj <- as.party(model, tree = 1, data = iris)

  # Check all internal nodes have splits
  all_ids <- partykit::nodeids(party_obj)
  terminal_ids <- partykit::nodeids(party_obj, terminal = TRUE)
  internal_ids <- setdiff(all_ids, terminal_ids)

  for (iid in internal_ids) {
    split <- partykit::split_node(party_obj[iid])
    expect_s3_class(split, "partysplit")

    # Check varid is valid
    varid <- partykit::varid_split(split)
    expect_true(varid >= 1)
    expect_true(varid <= ncol(party_obj$data))

    # Check breaks exist for numeric splits
    breaks <- partykit::breaks_split(split)
    if (!is.null(breaks)) {
      expect_true(is.numeric(breaks))
      expect_true(length(breaks) > 0)
    }
  }
})
```

**4. Data consistency**:
```r
test_that("as.party.ModelType data matches tree structure", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(y ~ x1 + x2, data = test_data, ntree = 5)
  party_obj <- as.party(model, tree = 1, data = test_data)

  # Check variable names
  expect_true("x1" %in% names(party_obj$data))
  expect_true("x2" %in% names(party_obj$data))

  # Check varids reference existing columns
  all_ids <- partykit::nodeids(party_obj)
  terminal_ids <- partykit::nodeids(party_obj, terminal = TRUE)
  internal_ids <- setdiff(all_ids, terminal_ids)

  for (iid in internal_ids) {
    split <- partykit::split_node(party_obj[iid])
    varid <- partykit::varid_split(split)
    expect_true(varid <= ncol(party_obj$data))
  }
})
```

**5. Fitted values**:
```r
test_that("as.party.ModelType computes correct fitted values", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(y ~ ., data = iris, ntree = 5)
  party_obj <- as.party(model, tree = 1, data = iris)

  # Check fitted structure
  expect_true(!is.null(party_obj$fitted))
  expect_true("(fitted)" %in% names(party_obj$fitted))
  expect_equal(nrow(party_obj$fitted), nrow(iris))

  # Check all fitted IDs are terminal nodes
  terminal_ids <- partykit::nodeids(party_obj, terminal = TRUE)
  fitted_ids <- party_obj$fitted[["(fitted)"]]
  expect_true(all(fitted_ids %in% terminal_ids))
})
```

**6. Different data types**:
```r
test_that("as.party.ModelType handles different data types", {
  skip_if_not_installed("modelpkg")

  # Numeric only
  data_numeric <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  model <- modelpkg::fit_function(y ~ ., data = data_numeric, ntree = 5)
  party_obj <- as.party(model, tree = 1, data = data_numeric)
  expect_s3_class(party_obj, "party")

  # With factors
  data_factor <- data.frame(
    y = factor(sample(c("A", "B"), 100, replace = TRUE)),
    x1 = rnorm(100),
    x2 = factor(sample(c("Low", "High"), 100, replace = TRUE))
  )
  model <- modelpkg::fit_function(y ~ ., data = data_factor, ntree = 5)
  party_obj <- as.party(model, tree = 1, data = data_factor)
  expect_s3_class(party_obj, "party")
  expect_true(is.factor(party_obj$data$x2))
})
```

**7. Multiple trees**:
```r
test_that("as.party.ModelType extracts different trees", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(y ~ ., data = iris, ntree = 10)

  party_1 <- as.party(model, tree = 1, data = iris)
  party_5 <- as.party(model, tree = 5, data = iris)
  party_10 <- as.party(model, tree = 10, data = iris)

  expect_s3_class(party_1, "party")
  expect_s3_class(party_5, "party")
  expect_s3_class(party_10, "party")

  # Trees should generally be different (not a hard requirement)
  expect_true(length(partykit::nodeids(party_1)) > 0)
  expect_true(length(partykit::nodeids(party_5)) > 0)
  expect_true(length(partykit::nodeids(party_10)) > 0)
})
```

**8. Without data parameter**:
```r
test_that("as.party.ModelType works without data parameter", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(Species ~ ., data = iris, ntree = 5)

  # Should work but may have minimal functionality
  party_obj <- as.party(model, tree = 1)

  expect_s3_class(party_obj, "party")
  expect_s3_class(party_obj$node, "partynode")
  expect_true(is.data.frame(party_obj$data))
})
```

**9. Edge cases**:
```r
test_that("as.party.ModelType handles edge cases", {
  skip_if_not_installed("modelpkg")

  # Very small data
  small_data <- data.frame(y = c(1, 2, 3), x1 = c(1, 2, 3))
  model <- modelpkg::fit_function(y ~ ., data = small_data, ntree = 1)
  party_obj <- as.party(model, tree = 1, data = small_data)
  expect_s3_class(party_obj, "party")

  # Single predictor
  single_pred <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- modelpkg::fit_function(y ~ ., data = single_pred, ntree = 1)
  party_obj <- as.party(model, tree = 1, data = single_pred)
  expect_s3_class(party_obj, "party")

  # Many predictors
  many_pred <- data.frame(
    y = rnorm(100),
    matrix(rnorm(100 * 20), ncol = 20)
  )
  names(many_pred)[-1] <- paste0("x", 1:20)
  model <- modelpkg::fit_function(y ~ ., data = many_pred, ntree = 1)
  party_obj <- as.party(model, tree = 1, data = many_pred)
  expect_s3_class(party_obj, "party")
})
```

### Integration Tests

Test interaction with partykit functions:

```r
test_that("as.party.ModelType integrates with partykit", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(y ~ ., data = iris, ntree = 5)
  party_obj <- as.party(model, tree = 1, data = iris)

  # Should work with partykit functions
  expect_silent(partykit::nodeids(party_obj))
  expect_silent(partykit::depth(party_obj))
  expect_silent(partykit::width(party_obj))

  # Should be plottable
  expect_silent(plot(party_obj))

  # Should be predictable
  expect_silent(predict(party_obj, newdata = iris[1:5, ]))
})
```

### Test with extract_rules

```r
test_that("as.party.ModelType works with extract_rules", {
  skip_if_not_installed("modelpkg")

  model <- modelpkg::fit_function(y ~ ., data = iris, ntree = 5)

  # extract_rules should work
  rules <- extract_rules(model, tree = 1)

  expect_s3_class(rules, "rule_set")
  expect_true(is.data.frame(rules))
  expect_true("rules" %in% names(rules))
  expect_true("id" %in% names(rules))
  expect_true(nrow(rules) > 0)

  # All rules should be valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})
```

## Common Pitfalls

### 1. Forgetting to Convert Indexing

```r
# WRONG: Using 0-based varid directly
split <- partykit::partysplit(varid = xgb_var_index, ...)

# RIGHT: Convert to 1-based
split <- partykit::partysplit(varid = xgb_var_index + 1L, ...)
```

### 2. Not Assigning Sequential IDs

```r
# WRONG: Using original model node IDs
partykit::partynode(id = original_node_id, ...)

# RIGHT: Assign after building tree
root <- build_tree(...)
root <- assign_node_ids(root)$node
```

### 3. Inconsistent Data Column Order

```r
# WRONG: Data columns don't match split varids
data <- data[, sample(names(data))]  # Shuffled order

# RIGHT: Ensure columns match varid order
data <- data[, model_var_names, drop = FALSE]
```

### 4. Incorrect Split Direction

```r
# WRONG: Not accounting for model's split convention
# Some models: left when <=, right when >
# Others: left when <, right when >=

# RIGHT: Check model documentation and use correct `right` parameter
split <- partykit::partysplit(varid, threshold, right = TRUE)  # or FALSE
```

### 5. Missing Type Conversions

```r
# WRONG: Threshold as character
split <- partykit::partysplit(varid, threshold = "5.5", ...)

# RIGHT: Convert to numeric
split <- partykit::partysplit(varid, threshold = as.numeric("5.5"), ...)
```

### 6. Not Handling Missing Data

```r
# WRONG: Assuming all observations can be traversed
child_idx <- if (value < threshold) 1L else 2L

# RIGHT: Handle NA values
if (is.na(value)) {
  child_idx <- 1L  # Default behavior
} else {
  child_idx <- if (value < threshold) 1L else 2L
}
```

## Documentation Requirements

### Roxygen Documentation

```r
#' Convert ModelType to party object
#'
#' Converts a single tree from a ModelType model into a party object from
#' the partykit package for visualization and analysis.
#'
#' @param obj A ModelType object returned by `modelpkg::fit_function()`
#' @param tree Integer specifying which tree to convert. Trees are numbered
#'   from 1 to the total number of trees in the model. For ensemble models,
#'   this extracts a single tree from the forest. For boosted models, this
#'   extracts a tree from a specific iteration/round.
#' @param data Optional data.frame containing the training data. If provided,
#'   enables full party functionality including proper variable names and
#'   fitted values. If NULL, attempts to extract from the model object or
#'   creates a minimal placeholder structure.
#' @param ... Additional arguments (currently unused, reserved for future use)
#'
#' @return A party object from the partykit package representing the tree
#'   structure. The object can be used with partykit functions for
#'   visualization (`plot`), prediction (`predict`), and analysis.
#'
#' @details
#' ## Model requirements
#'
#' The ModelType model must meet these requirements:
#' - [List any specific requirements, e.g., `keep.forest = TRUE`]
#' - [Note any limitations, e.g., "does not support rule-based models"]
#'
#' ## Tree indexing
#'
#' [Explain how trees are indexed for this model type]
#' - For simple ensembles: Trees numbered 1 to ntree
#' - For boosted multi-class: [Explain the tree numbering scheme]
#'
#' ## Data parameter
#'
#' [Explain data requirements]
#' - Required/optional/automatically extracted
#' - What columns should be included
#' - How response variable is handled
#'
#' @examples
#' \dontrun{
#' library(modelpkg)
#' library(partykit)
#'
#' # Train model
#' model <- modelpkg::fit_function(Species ~ ., data = iris, ntree = 100)
#'
#' # Convert to party
#' party_tree <- as.party(model, tree = 1, data = iris)
#'
#' # Visualize
#' plot(party_tree)
#'
#' # Extract rules
#' rules <- lorax::extract_rules(model, tree = 1)
#' }
#'
#' @seealso [modelpkg::fit_function()], [partykit::party()],
#'   [lorax::extract_rules()]
#'
#' @export
as.party.ModelType <- function(obj, tree = 1L, data = NULL, ...) {
  ...
}
```

### Update NAMESPACE

The method will be automatically exported if you use roxygen2 `@export` tag and run `devtools::document()`.

### Add to Package Description

Update `DESCRIPTION` to suggest the model package:

```
Suggests:
    modelpkg,
    ...
```

## Example: Complete Minimal Implementation

Here's a minimal working example for a hypothetical model:

```r
#' @export
as.party.SimpleTree <- function(obj, tree = 1L, data = NULL, ...) {
  # 1. Validate
  if (tree != 1L) {
    cli::cli_abort("SimpleTree only has one tree")
  }

  # 2. Extract tree (assuming obj$tree is a list with structure)
  var_names <- obj$var_names
  tree_struct <- obj$tree

  # 3. Build nodes recursively
  build_node <- function(node_data) {
    if (node_data$terminal) {
      return(partykit::partynode(id = 1L))
    }

    split <- partykit::partysplit(
      varid = which(var_names == node_data$split_var),
      breaks = node_data$threshold,
      index = 1:2,
      right = TRUE
    )

    partykit::partynode(
      id = 1L,
      split = split,
      kids = list(
        build_node(node_data$left),
        build_node(node_data$right)
      )
    )
  }

  root <- build_node(tree_struct)

  # 4. Assign IDs
  root <- assign_node_ids(root)$node

  # 5. Get data
  if (is.null(data)) {
    data <- reconstruct_data(var_names, n_obs = 0)
  } else {
    data <- data[, var_names, drop = FALSE]
  }

  # 6. Create party
  partykit::party(
    node = root,
    data = data,
    info = list(method = "SimpleTree")
  )
}
```

## Resources

- **partykit documentation**: `help(package = "partykit")`
- **partykit vignettes**: `vignette("partykit", package = "partykit")`
- **Existing implementations**: See `R/randomForest.R`, `R/ranger.R`, etc. in lorax
- **Helper functions**: `R/helpers_party.R` contains reusable utilities

## Summary Checklist

When implementing a new `as.party()` method:

- [ ] Understand how the model stores trees
- [ ] Extract tree structure from model object
- [ ] Build recursive partynode structure
- [ ] Convert node/variable indices to 1-based
- [ ] Assign sequential node IDs in pre-order
- [ ] Handle training data (provided or reconstructed)
- [ ] Compute fitted node IDs correctly
- [ ] Create proper partysplit objects with correct `right` parameter
- [ ] Handle factor variables appropriately
- [ ] Handle missing values in tree traversal
- [ ] Write comprehensive tests (structure, splits, data, fitted, edge cases)
- [ ] Add roxygen2 documentation with examples
- [ ] Test integration with partykit and extract_rules
- [ ] Add model package to Suggests in DESCRIPTION
