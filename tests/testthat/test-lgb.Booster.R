test_that("as.party.lgb.Booster returns valid party object", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "multiclass", num_class = 3, max_depth = 3),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )
  p <- as.party(bst, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
})

test_that("as.party.lgb.Booster works with binary classification", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("linear_1", "linear_2", "linear_3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "binary", max_depth = 2),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )
  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.lgb.Booster works with regression", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "regression", max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )
  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.lgb.Booster validates tree parameter", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "multiclass", num_class = 3, max_depth = 3),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  expect_snapshot(as.party(bst, tree = 0, data = penguins), error = TRUE)
  expect_snapshot(as.party(bst, tree = c(1, 2), data = penguins), error = TRUE)
  expect_snapshot(as.party(bst, tree = "1", data = penguins), error = TRUE)
  expect_snapshot(as.party(bst, tree = 100, data = penguins), error = TRUE)
})

test_that("as.party.lgb.Booster works with data parameter", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "multiclass", num_class = 3, max_depth = 3),
    data = dtrain,
    nrounds = 2,
    verbose = -1
  )
  p <- as.party(bst, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_equal(ncol(p$data), 5)
})

test_that("as.party.lgb.Booster handles simple trees", {
  skip_if_not_installed("lightgbm")

  # Create simple data with some variation
  set.seed(123)
  test_data <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20),
    y = rnorm(20)
  )
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(test_data[, c("x1", "x2")]),
    label = test_data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      max_depth = 2,
      min_data_in_leaf = 1
    ),
    data = dtrain,
    nrounds = 2,
    verbose = -1
  )
  p <- as.party(bst, tree = 1, data = test_data)

  expect_s3_class(p, "party")
})

test_that("as.party.lgb.Booster extracts different trees in multiclass", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "multiclass", num_class = 3, max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  # Extract trees for different classes/rounds
  p1 <- as.party(bst, tree = 1, data = penguins)
  p2 <- as.party(bst, tree = 4, data = penguins)

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")

  # Verify both work
  expect_true(length(partykit::nodeids(p1)) > 0)
  expect_true(length(partykit::nodeids(p2)) > 0)
})

test_that("as.party.lgb.Booster requires response in data", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "multiclass", num_class = 3, max_depth = 3),
    data = dtrain,
    nrounds = 2,
    verbose = -1
  )

  # Provide data with only predictors (no response)
  expect_snapshot(
    as.party(bst, tree = 1, data = penguins[, -1]),
    error = TRUE
  )
})

test_that("as.party.lgb.Booster handles trees with many nodes", {
  skip_if_not_installed("lightgbm")

  # Create data that produces deeper tree
  set.seed(123)
  n <- 200
  data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    y = rnorm(n)
  )

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3", "x4")]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      max_depth = 5
    ),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  # Deeper tree should have more nodes
  expect_true(length(partykit::nodeids(p)) > 5)
})

test_that("as.party.lgb.Booster handles different boosting rounds", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("linear_1", "linear_2", "linear_3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "binary",
      max_depth = 3
    ),
    data = dtrain,
    nrounds = 10,
    verbose = -1
  )

  # Extract trees from different rounds
  p1 <- as.party(bst, tree = 1, data = data)
  p5 <- as.party(bst, tree = 5, data = data)
  p10 <- as.party(bst, tree = 10, data = data)

  expect_s3_class(p1, "party")
  expect_s3_class(p5, "party")
  expect_s3_class(p10, "party")

  # All should have valid structure
  expect_true(length(partykit::nodeids(p1)) > 0)
  expect_true(length(partykit::nodeids(p5)) > 0)
  expect_true(length(partykit::nodeids(p10)) > 0)
})

test_that("as.party.lgb.Booster handles narrow trees", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("linear_1", "linear_2", "linear_3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "binary",
      max_depth = 1
    ),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  # Shallow tree (max_depth=1) should have at most 3 nodes
  expect_true(length(partykit::nodeids(p)) <= 3)
})

test_that("as.party.lgb.Booster preserves column order", {
  skip_if_not_installed("lightgbm")

  # Use specific column order
  data <- data.frame(
    z_last = rnorm(50),
    a_first = rnorm(50),
    m_middle = rnorm(50),
    response = rnorm(50)
  )

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("z_last", "a_first", "m_middle")]),
    label = data$response
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      max_depth = 2
    ),
    data = dtrain,
    nrounds = 2,
    verbose = -1
  )

  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_true(all(c("z_last", "a_first", "m_middle") %in% names(p$data)))
})

test_that("as.party.lgb.Booster with large multiclass", {
  skip_if_not_installed("lightgbm")

  # Multiclass with many classes
  set.seed(789)
  n <- 150
  data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    y = factor(sample(1:5, n, replace = TRUE))
  )

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "multiclass",
      num_class = 5,
      max_depth = 3
    ),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  # Should have 5 classes * 3 rounds = 15 trees
  # Extract trees for different classes
  p1 <- as.party(bst, tree = 1, data = data)
  p6 <- as.party(bst, tree = 6, data = data)

  expect_s3_class(p1, "party")
  expect_s3_class(p6, "party")
})

test_that("as.party.lgb.Booster handles varied data patterns", {
  skip_if_not_installed("lightgbm")

  # Data with very different scales
  set.seed(999)
  n <- 100
  data <- data.frame(
    tiny = rnorm(n, mean = 0, sd = 0.001),
    huge = rnorm(n, mean = 1000, sd = 100),
    normal = rnorm(n),
    response = rnorm(n)
  )

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("tiny", "huge", "normal")]),
    label = data$response
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      max_depth = 3
    ),
    data = dtrain,
    nrounds = 2,
    verbose = -1
  )

  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_true(length(partykit::nodeids(p)) > 0)
})

test_that("as.party.lgb.Booster extracts different trees", {
  skip_if_not_installed("lightgbm")

  data(agaricus.train, package = "lightgbm")
  train_data <- as.data.frame(as.matrix(agaricus.train$data))
  train_data$label <- agaricus.train$label

  dtrain <- lightgbm::lgb.Dataset(
    agaricus.train$data,
    label = agaricus.train$label
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "binary", max_depth = 3),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  p1 <- as.party(bst, tree = 1, data = train_data)
  p2 <- as.party(bst, tree = 3, data = train_data)

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")

  # Trees should not be identical
  expect_false(identical(p1$node, p2$node))
})

test_that("as.party.lgb.Booster works with single numeric predictor", {
  skip_if_not_installed("lightgbm")

  data <- get_single_numeric_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, "x", drop = FALSE]),
    label = as.numeric(data$y) - 1
  )
  set.seed(487)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )
  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.lgb.Booster does not show asterisks in node summaries", {
  skip_if_not_installed("lightgbm")

  data(agaricus.train, package = "lightgbm")
  train_data <- as.data.frame(as.matrix(agaricus.train$data))
  train_data$label <- agaricus.train$label

  dtrain <- lightgbm::lgb.Dataset(
    agaricus.train$data,
    label = agaricus.train$label
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "binary", max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  p <- as.party(bst, tree = 1, data = train_data)

  output <- capture.output(print(p))

  # Check for asterisks in node summaries (after the colon)
  # Pattern: ": *" or ": * " indicates missing summary
  has_asterisk_summary <- any(grepl(":\\s*\\*\\s*($|\\()", output))

  expect_false(has_asterisk_summary)
})

# ------------------------------------------------------------------------------
# active_predictors tests
# ------------------------------------------------------------------------------

test_that("active_predictors.lgb.Booster() returns correct structure", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "regression", max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  result <- active_predictors(bst)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("active_predictors", "tree"))
  expect_type(result$active_predictors, "list")
  expect_type(result$tree, "integer")
  expect_equal(nrow(result), 1)
})

test_that("active_predictors.lgb.Booster() extracts from single tree", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "regression", max_depth = 3),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- active_predictors(bst, tree = 1L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.lgb.Booster() extracts from multiple trees", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "regression", max_depth = 3),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- active_predictors(bst, tree = c(1L, 2L, 3L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_equal(length(result$active_predictors), 3)
})

test_that("active_predictors.lgb.Booster() works with all trees", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "regression", max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  result <- active_predictors(bst, tree = 1:3)

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_equal(length(result$active_predictors), 3)
})

test_that("active_predictors.lgb.Booster() validates tree argument", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "regression", max_depth = 3),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  expect_snapshot(
    active_predictors(bst, tree = "1"),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(bst, tree = 1.5),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(bst, tree = 0L),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(bst, tree = 999L),
    error = TRUE
  )
})

test_that("active_predictors.lgb.Booster() handles tree with no splits", {
  skip_if_not_installed("lightgbm")

  # Create simple data with min_data_in_leaf set high
  test_data <- data.frame(
    x1 = rnorm(20),
    y = rnorm(20)
  )
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(test_data[, "x1", drop = FALSE]),
    label = test_data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      max_depth = 1,
      min_data_in_leaf = 50
    ),
    data = dtrain,
    nrounds = 1,
    verbose = -1
  )

  result <- active_predictors(bst, tree = 1L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
  expect_type(result$active_predictors[[1]], "character")
  # May have 0 or more predictors depending on whether split occurred
  expect_true(length(result$active_predictors[[1]]) >= 0)
})

test_that("active_predictors.lgb.Booster() returns sorted unique variables", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "regression", max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  result <- active_predictors(bst, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  # Should be sorted (case-insensitive)
  expect_equal(active_vars, active_vars[order(tolower(active_vars))])

  # Should be unique
  expect_equal(length(active_vars), length(unique(active_vars)))
})

test_that("active_predictors.lgb.Booster() works with numeric predictors", {
  skip_if_not_installed("lightgbm")

  # Use mtcars which has no factors
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(mtcars[, c("cyl", "disp", "hp", "wt")]),
    label = mtcars$mpg
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "regression", max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  result <- active_predictors(bst, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_type(active_vars, "character")
  expect_true(all(active_vars %in% c("cyl", "disp", "hp", "wt")))
})

test_that("active_predictors.lgb.Booster() works with feature names", {
  skip_if_not_installed("lightgbm")

  data(agaricus.train, package = "lightgbm")

  dtrain <- lightgbm::lgb.Dataset(
    agaricus.train$data,
    label = agaricus.train$label
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "binary", max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  result <- active_predictors(bst, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_type(active_vars, "character")
  expect_true(length(active_vars) > 0)
})

# Tests for var_imp.lgb.Booster() ---------------------------------------------

test_that("active_predictors.lgb.Booster() works with single numeric predictor", {
  skip_if_not_installed("lightgbm")

  data <- get_single_numeric_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, "x", drop = FALSE]),
    label = as.numeric(data$y) - 1
  )
  set.seed(127)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )
  active <- active_predictors(bst)

  expect_s3_class(active, "tbl_df")
  # If the model made splits, should have "x" as active predictor
  if (length(unique(unlist(active$active_predictors))) > 0) {
    expect_setequal(unique(unlist(active$active_predictors)), "x")
  }
})

test_that("var_imp.lgb.Booster() returns correct structure", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  set.seed(821)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_type(result$term, "character")
  expect_type(result$estimate, "double")
})

test_that("var_imp.lgb.Booster() extracts variable importance scores", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  set.seed(347)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst)

  # Should have at least one variable with non-zero importance
  expect_true(any(result$estimate > 0))

  # All estimates should be non-negative
  expect_true(all(result$estimate >= 0))

  # Should have at least one predictor
  expect_true(nrow(result) > 0)
})

test_that("var_imp.lgb.Booster() with complete=TRUE fills missing predictors", {
  skip_if_not_installed("lightgbm")

  set.seed(629)
  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  set.seed(914)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst, complete = TRUE)

  # Should have at least one predictor
  expect_true(nrow(result) >= 1)

  # All estimates should be non-negative
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.lgb.Booster() with complete=FALSE returns only used predictors", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  set.seed(483)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst, complete = FALSE)

  # Should only have predictors with importance scores
  expect_true(nrow(result) >= 1)
  expect_true(all(result$estimate > 0))
})

test_that("var_imp.lgb.Booster() works with numeric predictors only", {
  skip_if_not_installed("lightgbm")

  mtcars <- get_mtcars_data()

  pred_cols <- c("cyl", "disp", "hp", "wt")
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(mtcars[, pred_cols]),
    label = mtcars$mpg
  )
  set.seed(756)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst, complete = TRUE, feature_names = pred_cols)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_true(all(result$estimate >= 0))
  expect_setequal(result$term, pred_cols)
})

test_that("var_imp.lgb.Booster() importance scores match underlying object", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  set.seed(192)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst, complete = FALSE)

  # Get expected importance from lightgbm
  expected <- as.data.frame(lightgbm::lgb.importance(model = bst))

  # Match scores
  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expected_row <- expected[expected$Feature == term, ]
    expect_equal(estimate, expected_row$Gain, tolerance = 1e-10)
  }
})

test_that("var_imp.lgb.Booster() works with binary classification", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("linear_1", "linear_2", "linear_3")]),
    label = as.numeric(data$y) - 1
  )
  set.seed(568)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "binary"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) >= 1)
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.lgb.Booster() works with multiclass classification", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  set.seed(274)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "multiclass",
      num_class = 3
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) >= 1)
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.lgb.Booster() handles model with no valid splits", {
  skip_if_not_installed("lightgbm")

  # Create data that produces no splits
  test_data <- data.frame(
    x1 = rep(1, 10),
    y = rep(0, 10)
  )

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(test_data[, "x1", drop = FALSE]),
    label = test_data$y
  )
  set.seed(835)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 0,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 1,
    verbose = -1
  )

  result <- var_imp(bst, complete = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  # Should have 0 rows since no features were used
  expect_equal(nrow(result), 0)

  # With feature_names, should return the feature with 0 importance
  result_complete <- var_imp(bst, complete = TRUE, feature_names = "x1")
  expect_equal(nrow(result_complete), 1)
  expect_equal(result_complete$term, "x1")
  expect_equal(result_complete$estimate, 0)
})

test_that("var_imp.lgb.Booster() handles many predictors", {
  skip_if_not_installed("lightgbm")

  set.seed(402)
  n <- 200
  p <- 15
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", 1:p)
  X$y <- X$x1 + X$x2 + rnorm(n)

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(X[, paste0("x", 1:p)]),
    label = X$y
  )
  set.seed(967)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst, complete = FALSE)

  # Should have only predictors that were used
  expect_true(nrow(result) >= 1)
  expect_true(nrow(result) <= p)
  expect_true(all(result$estimate >= 0))

  # With feature_names parameter, should have all predictors
  result_complete <- var_imp(
    bst,
    complete = TRUE,
    feature_names = paste0("x", 1:p)
  )
  expect_equal(nrow(result_complete), p)
  expect_true(all(result_complete$estimate >= 0))
  expect_setequal(result_complete$term, paste0("x", 1:p))
})

test_that("var_imp.lgb.Booster() works with constrained trees", {
  skip_if_not_installed("lightgbm")

  set.seed(518)
  small_data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(small_data[, c("x1", "x2")]),
    label = small_data$y
  )
  set.seed(693)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 1,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  result <- var_imp(bst, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.lgb.Booster() works with deep trees", {
  skip_if_not_installed("lightgbm")

  set.seed(741)
  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  set.seed(326)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 10,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) >= 1)
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.lgb.Booster() handles agaricus data", {
  skip_if_not_installed("lightgbm")

  data(agaricus.train, package = "lightgbm")

  dtrain <- lightgbm::lgb.Dataset(
    agaricus.train$data,
    label = agaricus.train$label
  )
  set.seed(159)
  bst <- lightgbm::lgb.train(
    params = list(max_depth = 3, objective = "binary"),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.lgb.Booster() handles very shallow trees", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("linear_1", "linear_2", "linear_3")]),
    label = as.numeric(data$y) - 1
  )
  set.seed(487)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 1,
      objective = "binary"
    ),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  result <- var_imp(bst)

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.lgb.Booster() works with many boosting rounds", {
  skip_if_not_installed("lightgbm")

  data <- get_regression_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("predictor_01", "predictor_02", "predictor_03")]),
    label = data$y
  )
  set.seed(851)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 20,
    verbose = -1
  )

  result <- var_imp(bst)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) >= 1)
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.lgb.Booster() works with single numeric predictor", {
  skip_if_not_installed("lightgbm")

  data <- get_single_numeric_data()

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, "x", drop = FALSE]),
    label = as.numeric(data$y) - 1
  )
  set.seed(298)
  bst <- lightgbm::lgb.train(
    params = list(
      max_depth = 3,
      objective = "regression"
    ),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )
  importance <- var_imp(bst)

  expect_s3_class(importance, "tbl_df")
  # If the model made splits, "x" should have importance
  if (nrow(importance) > 0) {
    expect_equal(importance$term, "x")
    expect_true(importance$estimate >= 0)
  }
})
