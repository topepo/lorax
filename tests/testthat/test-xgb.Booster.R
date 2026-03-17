test_that("as.party.xgb.Booster returns valid party object", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 3,
      objective = "multi:softmax",
      num_class = 3
    ),
    data = dtrain,
    nrounds = 5,
    verbose = 0
  )
  p <- as.party(bst, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
})

test_that("as.party.xgb.Booster works with binary classification", {
  skip_if_not_installed("xgboost")

  data <- get_binary_data(n = 100)

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 2,
      objective = "binary:logistic"
    ),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )
  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.xgb.Booster works with regression", {
  skip_if_not_installed("xgboost")

  data <- get_regression_data(n = 100)

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = data$y
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 3,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )
  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.xgb.Booster validates tree parameter", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 3,
      objective = "multi:softmax",
      num_class = 3
    ),
    data = dtrain,
    nrounds = 5,
    verbose = 0
  )

  expect_snapshot(as.party(bst, tree = 0, data = penguins), error = TRUE)
  expect_snapshot(as.party(bst, tree = c(1, 2), data = penguins), error = TRUE)
  expect_snapshot(as.party(bst, tree = "1", data = penguins), error = TRUE)
  expect_snapshot(as.party(bst, tree = 100, data = penguins), error = TRUE)
})

test_that("as.party.xgb.Booster works with data parameter", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 3,
      objective = "multi:softmax",
      num_class = 3
    ),
    data = dtrain,
    nrounds = 2,
    verbose = 0
  )
  p <- as.party(bst, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_equal(ncol(p$data), 5)
})

test_that("as.party.xgb.Booster handles single-node trees", {
  skip_if_not_installed("xgboost")

  # Create trivial data that might produce single-node tree
  test_data <- data.frame(
    x1 = rep(1, 10),
    y = rep(0, 10)
  )
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(test_data[, "x1", drop = FALSE]),
    label = test_data$y
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 0,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 1,
    verbose = 0
  )
  p <- as.party(bst, tree = 1, data = test_data)

  expect_s3_class(p, "party")
})

test_that("as.party.xgb.Booster extracts different trees in multiclass", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 3,
      objective = "multi:softmax",
      num_class = 3
    ),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )

  # Extract trees for different classes/rounds
  p1 <- as.party(bst, tree = 1, data = penguins)
  p2 <- as.party(bst, tree = 4, data = penguins) # Different class or round

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")

  # Verify both work
  expect_true(length(partykit::nodeids(p1)) > 0)
  expect_true(length(partykit::nodeids(p2)) > 0)
})

test_that("as.party.xgb.Booster requires response in data", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    label = as.numeric(penguins$species) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 3,
      objective = "multi:softmax",
      num_class = 3
    ),
    data = dtrain,
    nrounds = 2,
    verbose = 0
  )

  # Provide data with only predictors (no response)
  expect_snapshot(
    as.party(bst, tree = 1, data = penguins[, -1]),
    error = TRUE
  )
})

test_that("as.party.xgb.Booster handles trees with many nodes", {
  skip_if_not_installed("xgboost")

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

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3", "x4")]),
    label = data$y
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 5,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )

  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  # Deeper tree should have more nodes
  expect_true(length(partykit::nodeids(p)) > 5)
})

test_that("as.party.xgb.Booster handles different boosting rounds", {
  skip_if_not_installed("xgboost")

  data <- get_binary_data(n = 100)

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 3,
      objective = "binary:logistic"
    ),
    data = dtrain,
    nrounds = 10,
    verbose = 0
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

test_that("as.party.xgb.Booster handles narrow trees", {
  skip_if_not_installed("xgboost")

  data <- get_binary_data(n = 50)

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 1,
      objective = "binary:logistic"
    ),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )

  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  # Shallow tree (max_depth=1) should have at most 3 nodes
  expect_true(length(partykit::nodeids(p)) <= 3)
})

test_that("as.party.xgb.Booster preserves column order", {
  skip_if_not_installed("xgboost")

  # Use specific column order
  data <- data.frame(
    z_last = rnorm(50),
    a_first = rnorm(50),
    m_middle = rnorm(50),
    response = rnorm(50)
  )

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("z_last", "a_first", "m_middle")]),
    label = data$response
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 2,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 2,
    verbose = 0
  )

  p <- as.party(bst, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_true(all(c("z_last", "a_first", "m_middle") %in% names(p$data)))
})

test_that("as.party.xgb.Booster with large multiclass", {
  skip_if_not_installed("xgboost")

  # Multiclass with many classes
  set.seed(789)
  n <- 150
  data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    y = factor(sample(1:5, n, replace = TRUE))
  )

  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(
      max_depth = 3,
      objective = "multi:softmax",
      num_class = 5
    ),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )

  # Should have 5 classes * 3 rounds = 15 trees
  # Extract trees for different classes
  p1 <- as.party(bst, tree = 1, data = data)
  p6 <- as.party(bst, tree = 6, data = data)

  expect_s3_class(p1, "party")
  expect_s3_class(p6, "party")

  # Trees should not be identical
  expect_false(identical(p1$node, p6$node))
})

test_that("as.party.xgb.Booster does not show asterisks in node summaries", {
  skip_if_not_installed("xgboost")

  data(agaricus.train, package = "xgboost")
  train_data <- as.data.frame(as.matrix(agaricus.train$data))
  train_data$label <- agaricus.train$label

  dtrain <- xgboost::xgb.DMatrix(
    agaricus.train$data,
    label = agaricus.train$label
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 3, objective = "binary:logistic"),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )

  p <- as.party(bst, tree = 1, data = train_data)

  output <- capture.output(print(p))

  # Check for asterisks in node summaries (after the colon)
  # Pattern: ": *" or ": * " indicates missing summary
  has_asterisk_summary <- any(grepl(":\\s*\\*\\s*($|\\()", output))

  expect_false(has_asterisk_summary)
})
