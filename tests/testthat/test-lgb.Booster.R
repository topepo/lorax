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

  data <- get_binary_data(n = 100)

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
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

  data <- get_regression_data(n = 100)

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
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

  data <- get_binary_data(n = 100)

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
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

  data <- get_binary_data(n = 50)

  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
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
