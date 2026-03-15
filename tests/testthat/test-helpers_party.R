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
