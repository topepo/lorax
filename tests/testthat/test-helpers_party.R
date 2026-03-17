test_that("build_partysplit validates inputs", {
  # Valid input
  split <- build_partysplit(1, 5.5, right = TRUE)
  expect_s3_class(split, "partysplit")

  # Invalid varid
  expect_snapshot(build_partysplit(NA, 5.5), error = TRUE)
  expect_snapshot(build_partysplit(integer(0), 5.5), error = TRUE)

  # Invalid threshold
  expect_snapshot(build_partysplit(1, NA), error = TRUE)
  expect_snapshot(build_partysplit(1, numeric(0)), error = TRUE)
  expect_snapshot(build_partysplit(1, "5.5"), error = TRUE)
})

test_that("build_partysplit creates correct split types", {
  # Right interval closed
  split_right <- build_partysplit(1, 5.5, right = TRUE)
  expect_equal(partykit::varid_split(split_right), 1L)
  expect_equal(partykit::breaks_split(split_right), 5.5)
  expect_true(partykit::right_split(split_right))

  # Left interval closed
  split_left <- build_partysplit(2, 10.0, right = FALSE)
  expect_equal(partykit::varid_split(split_left), 2L)
  expect_equal(partykit::breaks_split(split_left), 10.0)
  expect_false(partykit::right_split(split_left))
})

test_that("reconstruct_data creates correct structure", {
  # Numeric only (default)
  df1 <- reconstruct_data(c("x1", "x2", "x3"), n_obs = 0)
  expect_equal(nrow(df1), 0)
  expect_equal(ncol(df1), 3)
  expect_equal(names(df1), c("x1", "x2", "x3"))
  expect_true(all(sapply(df1, is.numeric)))

  # With non-zero rows
  df2 <- reconstruct_data(c("a", "b"), n_obs = 5)
  expect_equal(nrow(df2), 5)
  expect_equal(ncol(df2), 2)
})

test_that("reconstruct_data handles different types", {
  # Mixed types
  df <- reconstruct_data(
    c("x1", "x2", "x3"),
    var_types = c("numeric", "factor", "integer"),
    n_obs = 3
  )

  expect_true(is.numeric(df$x1))
  expect_true(is.factor(df$x2))
  expect_true(is.integer(df$x3))
})

test_that("create_party_object validates inputs", {
  # Valid input
  node <- partykit::partynode(id = 1L)
  data <- data.frame(x = 1:10)
  p <- create_party_object(node, data)
  expect_s3_class(p, "party")

  # Invalid node
  expect_snapshot(
    create_party_object(list(id = 1), data),
    error = TRUE
  )

  # Invalid data
  expect_snapshot(
    create_party_object(node, list(x = 1:10)),
    error = TRUE
  )
})

test_that("count_nodes works correctly", {
  # NULL node
  expect_equal(count_nodes(NULL), 0L)

  # Terminal node
  terminal <- partykit::partynode(id = 1L)
  expect_equal(count_nodes(terminal), 1L)

  # Node with children
  split <- build_partysplit(1, 5.5)
  left <- partykit::partynode(id = 2L)
  right <- partykit::partynode(id = 3L)
  parent <- partykit::partynode(
    id = 1L,
    split = split,
    kids = list(left, right)
  )
  expect_equal(count_nodes(parent), 3L)

  # Deeper tree
  split2 <- build_partysplit(1, 3.0)
  left2 <- partykit::partynode(id = 4L)
  right2 <- partykit::partynode(id = 5L)
  parent$kids[[1]] <- partykit::partynode(
    id = 2L,
    split = split2,
    kids = list(left2, right2)
  )
  expect_equal(count_nodes(parent), 5L)
})

test_that("remove_terminal_predictions removes info from leaves", {
  # Terminal node with info
  terminal <- partykit::partynode(id = 1L, info = list(pred = 5))
  result <- remove_terminal_predictions(terminal)
  expect_null(result$info)

  # Internal node with terminal children
  split <- build_partysplit(1, 5.5)
  left <- partykit::partynode(id = 2L, info = list(pred = 1))
  right <- partykit::partynode(id = 3L, info = list(pred = 2))
  parent <- partykit::partynode(
    id = 1L,
    split = split,
    kids = list(left, right),
    info = list(keep = TRUE)
  )

  result <- remove_terminal_predictions(parent)
  expect_equal(result$info, list(keep = TRUE))
  expect_null(result$kids[[1]]$info)
  expect_null(result$kids[[2]]$info)
})

test_that("traverse_to_terminal handles missing values", {
  skip_if_not_installed("randomForest")

  # Create simple model
  data <- data.frame(
    y = factor(c("A", "B", "A", "B", "A", "B")),
    x1 = c(1, 2, 3, 4, 5, 6)
  )
  rf <- randomForest::randomForest(y ~ x1, data = data, ntree = 1, maxnodes = 3)
  p <- as.party(rf, tree = 1, data = data)

  # Test with missing value
  test_data <- data.frame(x1 = NA_real_)
  node_id <- traverse_to_terminal(p$node, test_data)
  expect_true(is.integer(node_id))
  expect_true(node_id >= 1)
})

test_that("traverse_to_terminal handles terminal nodes", {
  # Simple terminal node
  terminal <- partykit::partynode(id = 5L)
  obs <- data.frame(x = 1)
  expect_equal(traverse_to_terminal(terminal, obs), 5L)
})

test_that("traverse_to_terminal handles node without split", {
  # Create a proper node then remove the split to simulate edge case
  split <- build_partysplit(1, 5.5)
  terminal1 <- partykit::partynode(id = 2L)
  terminal2 <- partykit::partynode(id = 3L)
  node <- partykit::partynode(
    id = 1L,
    split = split,
    kids = list(terminal1, terminal2)
  )
  # Manually set split to NULL to test edge case
  node$split <- NULL

  obs <- data.frame(x = 1)
  expect_equal(traverse_to_terminal(node, obs), 1L)
})

test_that("traverse_to_terminal handles right interval closed splits", {
  skip_if_not_installed("xgboost")

  # Create xgboost model (uses right = TRUE)
  data <- data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    x1 = c(1, 2, 3, 4, 5, 6)
  )
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, "x1", drop = FALSE]),
    label = data$y
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 2, objective = "reg:squarederror"),
    data = dtrain,
    nrounds = 1,
    verbose = 0
  )

  p <- as.party(bst, tree = 1, data = data)

  # Test observation
  test_obs <- data.frame(x1 = 3.5)
  node_id <- traverse_to_terminal(p$node, test_obs)
  expect_true(is.integer(node_id))
  expect_true(node_id >= 1)
})

test_that("traverse_to_terminal handles left interval closed splits", {
  skip_if_not_installed("lightgbm")

  # Create lightgbm model (uses right = FALSE)
  set.seed(888)
  data <- data.frame(
    y = rnorm(30),
    x1 = rnorm(30)
  )
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, "x1", drop = FALSE]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      max_depth = 2,
      min_data_in_leaf = 5
    ),
    data = dtrain,
    nrounds = 1,
    verbose = -1
  )

  p <- as.party(bst, tree = 1, data = data)

  # Test observation
  test_obs <- data.frame(x1 = 0.5)
  node_id <- traverse_to_terminal(p$node, test_obs)
  expect_true(is.integer(node_id))
  expect_true(node_id >= 1)
})

test_that("traverse_to_terminal handles out of bounds child index", {
  # Test edge case where we try to access a child that doesn't exist
  # Create a proper node then manually manipulate it to create edge case
  split <- build_partysplit(1, 5.5)
  terminal1 <- partykit::partynode(id = 2L)
  terminal2 <- partykit::partynode(id = 3L)
  node <- partykit::partynode(
    id = 1L,
    split = split,
    kids = list(terminal1, terminal2)
  )

  # Remove one child to simulate edge case (manually modify internals)
  node$kids <- list(terminal1)

  obs <- data.frame(x = 10) # Would go to right child (index 2)

  # Should return current node as fallback when child doesn't exist
  result <- traverse_to_terminal(node, obs)
  expect_true(result %in% c(1L, 2L)) # Either current or available child
})

test_that("traverse_to_terminal handles non-numeric non-factor variables", {
  # Create a split expecting numeric but provide character
  split <- build_partysplit(1, 5.5)
  left <- partykit::partynode(id = 2L)
  right <- partykit::partynode(id = 3L)
  node <- partykit::partynode(id = 1L, split = split, kids = list(left, right))

  # Provide character variable (unsupported type)
  obs <- data.frame(x = "text")
  # Should default to left child
  expect_equal(traverse_to_terminal(node, obs), 2L)
})

test_that("compute_fitted_node_ids works with empty data", {
  node <- partykit::partynode(id = 1L)
  data <- data.frame(x = numeric(0))
  result <- compute_fitted_node_ids(node, data)
  expect_equal(length(result), 0)
  expect_true(is.integer(result))
})

test_that("compute_fitted_node_ids assigns all observations to nodes", {
  skip_if_not_installed("randomForest")

  # Create simple model
  data <- data.frame(
    y = factor(rep(c("A", "B"), 10)),
    x1 = 1:20
  )
  rf <- randomForest::randomForest(y ~ x1, data = data, ntree = 1)
  p <- as.party(rf, tree = 1, data = data)

  fitted_ids <- compute_fitted_node_ids(p$node, data[, "x1", drop = FALSE])
  expect_equal(length(fitted_ids), nrow(data))
  expect_true(all(fitted_ids >= 1))
  expect_true(is.integer(fitted_ids))
})

test_that("assign_node_ids creates sequential IDs", {
  # Create tree structure
  split1 <- build_partysplit(1, 5.5)
  split2 <- build_partysplit(1, 3.0)

  leaf1 <- partykit::partynode(id = 999L) # Wrong ID
  leaf2 <- partykit::partynode(id = 888L) # Wrong ID
  left <- partykit::partynode(
    id = 777L,
    split = split2,
    kids = list(leaf1, leaf2)
  )
  right <- partykit::partynode(id = 666L)
  root <- partykit::partynode(
    id = 555L,
    split = split1,
    kids = list(left, right)
  )

  # Assign sequential IDs
  result <- assign_node_ids(root)
  new_root <- result$node

  # Check IDs are sequential starting from 1
  expect_equal(new_root$id, 1L)
  expect_equal(new_root$kids[[1]]$id, 2L)
  expect_equal(new_root$kids[[1]]$kids[[1]]$id, 3L)
  expect_equal(new_root$kids[[1]]$kids[[2]]$id, 4L)
  expect_equal(new_root$kids[[2]]$id, 5L)
  expect_equal(result$next_id, 6L)
})

test_that("assign_node_ids handles terminal node", {
  terminal <- partykit::partynode(id = 999L)
  result <- assign_node_ids(terminal)
  expect_equal(result$node$id, 1L)
  expect_equal(result$next_id, 2L)
})

test_that("assign_node_ids can start from custom ID", {
  split <- build_partysplit(1, 5.5)
  left <- partykit::partynode(id = 999L)
  right <- partykit::partynode(id = 888L)
  root <- partykit::partynode(
    id = 777L,
    split = split,
    kids = list(left, right)
  )

  # Start from 10
  result <- assign_node_ids(root, next_id = 10L)
  expect_equal(result$node$id, 10L)
  expect_equal(result$node$kids[[1]]$id, 11L)
  expect_equal(result$node$kids[[2]]$id, 12L)
  expect_equal(result$next_id, 13L)
})

# validate_and_select_data tests ----

test_that("validate_and_select_data selects predictor columns", {
  data <- data.frame(
    x1 = 1:10,
    x2 = 11:20,
    x3 = 21:30,
    y = 31:40
  )

  result <- validate_and_select_data(data, c("x1", "x2"))

  expect_equal(ncol(result), 4) # x1, x2, plus x3, y preserved
  expect_true(all(c("x1", "x2") %in% names(result)))
})

test_that("validate_and_select_data preserves extra columns by default", {
  data <- data.frame(
    x1 = 1:5,
    x2 = 6:10,
    y = 11:15,
    z = 16:20
  )

  result <- validate_and_select_data(data, c("x1", "x2"), preserve_extra = TRUE)

  expect_equal(ncol(result), 4)
  expect_true(all(c("x1", "x2", "y", "z") %in% names(result)))
})

test_that("validate_and_select_data can exclude extra columns", {
  data <- data.frame(
    x1 = 1:5,
    x2 = 6:10,
    y = 11:15,
    z = 16:20
  )

  result <- validate_and_select_data(
    data,
    c("x1", "x2"),
    preserve_extra = FALSE
  )

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x1", "x2"))
})

test_that("validate_and_select_data validates required variables", {
  data <- data.frame(x1 = 1:5, x2 = 6:10)

  expect_snapshot(
    validate_and_select_data(data, c("x1", "x3")),
    error = TRUE
  )

  expect_snapshot(
    validate_and_select_data(data, c("missing", "also_missing")),
    error = TRUE
  )
})

test_that("validate_and_select_data preserves column order", {
  data <- data.frame(
    b = 1:5,
    a = 6:10,
    c = 11:15
  )

  result <- validate_and_select_data(data, c("b", "a"), preserve_extra = TRUE)

  expect_equal(names(result), c("b", "a", "c"))
})

# create_fitted_dataframe tests ----

test_that("create_fitted_dataframe creates basic structure", {
  fitted_ids <- c(1L, 2L, 1L, 3L, 2L)

  result <- create_fitted_dataframe(fitted_ids)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 1)
  expect_equal(names(result), "(fitted)")
  expect_equal(result[["(fitted)"]], fitted_ids)
})

test_that("create_fitted_dataframe includes response when provided", {
  fitted_ids <- c(1L, 2L, 1L, 3L, 2L)
  response <- c(10, 20, 15, 30, 25)

  result <- create_fitted_dataframe(fitted_ids, response)

  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("(fitted)", "(response)"))
  expect_equal(result[["(fitted)"]], fitted_ids)
  expect_equal(result[["(response)"]], response)
})

test_that("create_fitted_dataframe preserves response type", {
  fitted_ids <- c(1L, 2L, 1L)

  # Factor response
  response_factor <- factor(c("A", "B", "A"))
  result1 <- create_fitted_dataframe(fitted_ids, response_factor)
  expect_true(is.factor(result1[["(response)"]]))
  expect_equal(levels(result1[["(response)"]]), c("A", "B"))

  # Numeric response
  response_numeric <- c(1.5, 2.5, 3.5)
  result2 <- create_fitted_dataframe(fitted_ids, response_numeric)
  expect_true(is.numeric(result2[["(response)"]]))

  # Character response
  response_char <- c("x", "y", "z")
  result3 <- create_fitted_dataframe(fitted_ids, response_char)
  expect_true(is.character(result3[["(response)"]]))
})

test_that("create_fitted_dataframe handles empty input", {
  fitted_ids <- integer(0)

  result <- create_fitted_dataframe(fitted_ids)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 1)
  expect_equal(names(result), "(fitted)")
})

test_that("create_fitted_dataframe errors on mismatched lengths", {
  fitted_ids <- c(1L, 2L, 3L)
  response <- c(10, 20) # Wrong length

  # Should error due to mismatched lengths
  expect_error(
    create_fitted_dataframe(fitted_ids, response),
    "differing number of rows"
  )
})

# validate_party_node_info tests ----

test_that("validate_party_node_info with valid party object", {
  node <- partykit::partynode(id = 1L)
  data <- data.frame(x = 1:5, y = 6:10)

  party_obj <- create_party_object(
    node = node,
    data = data,
    fitted = data.frame(
      "(fitted)" = rep(1L, 5),
      "(response)" = data$y,
      check.names = FALSE
    )
  )

  # Silent mode - should return TRUE
  result <- validate_party_node_info(party_obj, action = "silent")
  expect_true(result)

  # Error mode - should not error
  expect_no_error(validate_party_node_info(party_obj, action = "error"))
})

test_that("validate_party_node_info detects missing response", {
  node <- partykit::partynode(id = 1L)
  data <- data.frame(x = 1:5, y = 6:10)

  party_obj <- create_party_object(
    node = node,
    data = data,
    fitted = data.frame("(fitted)" = rep(1L, 5), check.names = FALSE)
  )

  # Silent mode - should return FALSE
  result <- validate_party_node_info(party_obj, action = "silent")
  expect_false(result)

  # Error mode - should error
  expect_snapshot(
    validate_party_node_info(party_obj, action = "error"),
    error = TRUE
  )

  # Warn mode - should warn
  expect_warning(
    validate_party_node_info(party_obj, action = "warn"),
    "asterisks"
  )
})

test_that("validate_party_node_info handles party without fitted", {
  node <- partykit::partynode(id = 1L)
  data <- data.frame(x = 1:5, y = 6:10)

  party_obj <- create_party_object(node = node, data = data)

  # Without fitted data, validation should pass
  result <- validate_party_node_info(party_obj, action = "silent")
  expect_true(result)
})

# build_partynode_from_tabular tests ----

test_that("build_partynode_from_tabular builds terminal node", {
  # Create tabular tree with single terminal node
  tree_df <- data.frame(
    node_id = 0L,
    left_child = -1L,
    right_child = -1L,
    split_var = -1L,
    split_val = NA_real_,
    prediction = 5.0
  )

  node <- build_partynode_from_tabular(
    tree_df = tree_df,
    node_id = 0L,
    node_id_col = "node_id",
    left_child_col = "left_child",
    right_child_col = "right_child",
    split_var_col = "split_var",
    split_val_col = "split_val",
    is_leaf_col = NULL,
    prediction_col = "prediction",
    var_names = c("x1", "x2"),
    zero_indexed = TRUE
  )

  expect_s3_class(node, "partynode")
  expect_equal(node$id, 1L)
  expect_null(partykit::kids_node(node))
})

test_that("build_partynode_from_tabular builds tree with split", {
  # Create simple tree: root splits, two leaves
  tree_df <- data.frame(
    node_id = c(0L, 1L, 2L),
    left_child = c(1L, -1L, -1L),
    right_child = c(2L, -1L, -1L),
    split_var = c(0L, -1L, -1L),
    split_val = c(5.5, NA, NA),
    prediction = c(NA, 1.0, 2.0)
  )

  node <- build_partynode_from_tabular(
    tree_df = tree_df,
    node_id = 0L,
    node_id_col = "node_id",
    left_child_col = "left_child",
    right_child_col = "right_child",
    split_var_col = "split_var",
    split_val_col = "split_val",
    is_leaf_col = NULL,
    prediction_col = "prediction",
    var_names = c("x1", "x2"),
    zero_indexed = TRUE
  )

  expect_s3_class(node, "partynode")
  expect_false(is.null(partykit::split_node(node)))
  kids <- partykit::kids_node(node)
  expect_length(kids, 2)
})

test_that("build_partynode_from_tabular uses is_leaf_col when provided", {
  tree_df <- data.frame(
    node_id = c(0L, 1L, 2L),
    left_child = c(1L, NA, NA),
    right_child = c(2L, NA, NA),
    split_var = c(0L, NA, NA),
    split_val = c(5.5, NA, NA),
    is_leaf = c(FALSE, TRUE, TRUE),
    prediction = c(NA, 1.0, 2.0)
  )

  node <- build_partynode_from_tabular(
    tree_df = tree_df,
    node_id = 0L,
    node_id_col = "node_id",
    left_child_col = "left_child",
    right_child_col = "right_child",
    split_var_col = "split_var",
    split_val_col = "split_val",
    is_leaf_col = "is_leaf",
    is_leaf_value = TRUE,
    prediction_col = "prediction",
    var_names = c("x1", "x2"),
    zero_indexed = TRUE
  )

  expect_s3_class(node, "partynode")
  kids <- partykit::kids_node(node)
  expect_length(kids, 2)
})

test_that("build_partynode_from_tabular handles 1-indexed node IDs", {
  # 1-indexed: use 0 for no child instead of -1
  tree_df <- data.frame(
    node_id = c(1L, 2L, 3L),
    left_child = c(2L, 0L, 0L),
    right_child = c(3L, 0L, 0L),
    split_var = c(1L, 0L, 0L),
    split_val = c(5.5, NA, NA),
    prediction = c(NA, 1.0, 2.0)
  )

  node <- build_partynode_from_tabular(
    tree_df = tree_df,
    node_id = 1L,
    node_id_col = "node_id",
    left_child_col = "left_child",
    right_child_col = "right_child",
    split_var_col = "split_var",
    split_val_col = "split_val",
    is_leaf_col = NULL,
    prediction_col = "prediction",
    var_names = c("x1", "x2"),
    zero_indexed = FALSE
  )

  expect_s3_class(node, "partynode")
  kids <- partykit::kids_node(node)
  expect_length(kids, 2)
})

test_that("build_partynode_from_tabular handles variable names", {
  tree_df <- data.frame(
    node_id = c(0L, 1L, 2L),
    left_child = c(1L, -1L, -1L),
    right_child = c(2L, -1L, -1L),
    split_var = c("height", NA, NA), # Character variable name
    split_val = c(170.5, NA, NA),
    prediction = c(NA, 1.0, 2.0)
  )

  node <- build_partynode_from_tabular(
    tree_df = tree_df,
    node_id = 0L,
    node_id_col = "node_id",
    left_child_col = "left_child",
    right_child_col = "right_child",
    split_var_col = "split_var",
    split_val_col = "split_val",
    is_leaf_col = NULL,
    prediction_col = "prediction",
    var_names = c("height", "weight", "age"),
    zero_indexed = TRUE
  )

  expect_s3_class(node, "partynode")
  split <- partykit::split_node(node)
  expect_equal(partykit::varid_split(split), 1L) # height is first
})

test_that("build_partynode_from_tabular errors on missing node", {
  tree_df <- data.frame(
    node_id = c(0L, 1L),
    left_child = c(1L, -1L),
    right_child = c(2L, -1L),
    split_var = c(0L, -1L),
    split_val = c(5.5, NA),
    prediction = c(NA, 1.0)
  )

  expect_snapshot(
    build_partynode_from_tabular(
      tree_df = tree_df,
      node_id = 0L,
      node_id_col = "node_id",
      left_child_col = "left_child",
      right_child_col = "right_child",
      split_var_col = "split_var",
      split_val_col = "split_val",
      is_leaf_col = NULL,
      prediction_col = "prediction",
      var_names = c("x1", "x2"),
      zero_indexed = TRUE
    ),
    error = TRUE
  )
})

test_that("build_partynode_from_tabular handles large variable index", {
  # Note: partykit doesn't validate varid against data columns
  # This creates a split with an out-of-bounds varid, which will error
  # only when used with actual data
  tree_df <- data.frame(
    node_id = c(0L, 1L, 2L),
    left_child = c(1L, -1L, -1L),
    right_child = c(2L, -1L, -1L),
    split_var = c(5L, -1L, -1L),
    split_val = c(5.5, NA, NA),
    prediction = c(NA, 1.0, 2.0)
  )

  node <- build_partynode_from_tabular(
    tree_df = tree_df,
    node_id = 0L,
    node_id_col = "node_id",
    left_child_col = "left_child",
    right_child_col = "right_child",
    split_var_col = "split_var",
    split_val_col = "split_val",
    is_leaf_col = NULL,
    prediction_col = "prediction",
    var_names = c("x1", "x2"),
    zero_indexed = TRUE
  )

  # Should create node with varid = 6 (5 + 1)
  expect_s3_class(node, "partynode")
  split <- partykit::split_node(node)
  expect_equal(partykit::varid_split(split), 6L)
})

test_that("build_partynode_from_tabular errors on missing variable name", {
  tree_df <- data.frame(
    node_id = c(0L, 1L, 2L),
    left_child = c(1L, -1L, -1L),
    right_child = c(2L, -1L, -1L),
    split_var = c("missing_var", NA, NA),
    split_val = c(5.5, NA, NA),
    prediction = c(NA, 1.0, 2.0)
  )

  expect_snapshot(
    build_partynode_from_tabular(
      tree_df = tree_df,
      node_id = 0L,
      node_id_col = "node_id",
      left_child_col = "left_child",
      right_child_col = "right_child",
      split_var_col = "split_var",
      split_val_col = "split_val",
      is_leaf_col = NULL,
      prediction_col = "prediction",
      var_names = c("x1", "x2"),
      zero_indexed = TRUE
    ),
    error = TRUE
  )
})

# traverse_vectorized tests ----

test_that("traverse_vectorized handles empty observation set", {
  node <- partykit::partynode(id = 1L)
  data <- data.frame(x = 1:10)
  obs_indices <- integer(0)
  node_ids <- integer(10)

  result <- traverse_vectorized(node, data, obs_indices, node_ids)

  expect_equal(result, node_ids)
})

test_that("traverse_vectorized assigns terminal node correctly", {
  terminal <- partykit::partynode(id = 5L)
  data <- data.frame(x = 1:10)
  obs_indices <- c(1L, 3L, 5L)
  node_ids <- integer(10)

  result <- traverse_vectorized(terminal, data, obs_indices, node_ids)

  expect_equal(result[1], 5L)
  expect_equal(result[3], 5L)
  expect_equal(result[5], 5L)
  expect_equal(result[c(2, 4, 6, 7, 8, 9, 10)], rep(0L, 7))
})

test_that("traverse_vectorized splits observations correctly", {
  split <- build_partysplit(1, 5.0, right = TRUE)
  left <- partykit::partynode(id = 2L)
  right <- partykit::partynode(id = 3L)
  node <- partykit::partynode(id = 1L, split = split, kids = list(left, right))

  data <- data.frame(x = c(1, 3, 5, 7, 9))
  obs_indices <- 1:5
  node_ids <- integer(5)

  result <- traverse_vectorized(node, data, obs_indices, node_ids)

  # Values < 5 should go left (node 2), >= 5 should go right (node 3)
  expect_equal(result[1:2], c(2L, 2L)) # 1, 3 < 5
  expect_equal(result[3:5], c(3L, 3L, 3L)) # 5, 7, 9 >= 5
})

test_that("traverse_vectorized handles missing values", {
  split <- build_partysplit(1, 5.0, right = TRUE)
  left <- partykit::partynode(id = 2L)
  right <- partykit::partynode(id = 3L)
  node <- partykit::partynode(id = 1L, split = split, kids = list(left, right))

  data <- data.frame(x = c(1, NA, 7, NA, 3))
  obs_indices <- 1:5
  node_ids <- integer(5)

  result <- traverse_vectorized(node, data, obs_indices, node_ids)

  # Missing values should go left (node 2)
  expect_equal(result[c(2, 4)], c(2L, 2L)) # NA values
  expect_equal(result[c(1, 5)], c(2L, 2L)) # 1, 3 < 5
  expect_equal(result[3], 3L) # 7 >= 5
})

test_that("traverse_vectorized handles factor variables", {
  skip_if_not_installed("randomForest")

  # Create model with factor variable
  data <- data.frame(
    y = factor(c("A", "B", "A", "B", "A", "B")),
    x = factor(c("low", "high", "low", "high", "med", "med"))
  )
  rf <- randomForest::randomForest(y ~ x, data = data, ntree = 1)
  p <- as.party(rf, tree = 1, data = data)

  # Test vectorized traversal
  fitted_ids <- compute_fitted_node_ids(p$node, data[, "x", drop = FALSE])
  expect_equal(length(fitted_ids), 6)
  expect_true(all(fitted_ids >= 1))
})
