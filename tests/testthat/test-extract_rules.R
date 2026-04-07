test_that("extract_rules.xgb.Booster returns valid rule set", {
  skip_if_not_installed("xgboost")

  data <- get_binary_data()
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 3, objective = "binary:logistic"),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )

  rules <- extract_rules(bst, tree = 1)

  expect_s3_class(rules, "rule_set_xgb.Booster")
  expect_s3_class(rules, "rule_set")
  expect_true(is.data.frame(rules))
  expect_true("tree" %in% names(rules))
  expect_true("rules" %in% names(rules))
  expect_true("id" %in% names(rules))
  expect_true(nrow(rules) > 0)
  expect_true(all(sapply(rules$rules, is.language)))
})

test_that("extract_rules.xgb.Booster validates tree parameter", {
  skip_if_not_installed("xgboost")

  data <- get_binary_data()
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 2, objective = "binary:logistic"),
    data = dtrain,
    nrounds = 2,
    verbose = 0
  )

  expect_snapshot(extract_rules(bst, tree = 0), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = c(1, 2)), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = "1"), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = 100), error = TRUE)
})

test_that("extract_rules.xgb.Booster handles single-node trees", {
  skip_if_not_installed("xgboost")

  # Create data that produces single-node tree
  data <- data.frame(x1 = rep(1, 10), y = rep(0, 10))
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, "x1", drop = FALSE]),
    label = data$y
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 0, objective = "reg:squarederror"),
    data = dtrain,
    nrounds = 1,
    verbose = 0
  )

  rules <- extract_rules(bst, tree = 1)

  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  # Single node tree has TRUE rule
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

test_that("extract_rules.xgb.Booster extracts different trees", {
  skip_if_not_installed("xgboost")

  data <- get_binary_data()
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 3, objective = "binary:logistic"),
    data = dtrain,
    nrounds = 5,
    verbose = 0
  )

  rules1 <- extract_rules(bst, tree = 1)
  rules2 <- extract_rules(bst, tree = 3)

  expect_s3_class(rules1, "rule_set")
  expect_s3_class(rules2, "rule_set")
  expect_equal(rules1$tree[1], 1L)
  expect_equal(rules2$tree[1], 3L)
})

test_that("extract_rules.lgb.Booster returns valid rule set", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "binary", max_depth = 3),
    data = dtrain,
    nrounds = 3,
    verbose = -1
  )

  rules <- extract_rules(bst, tree = 1)

  expect_s3_class(rules, "rule_set_lgb.Booster")
  expect_s3_class(rules, "rule_set")
  expect_true(is.data.frame(rules))
  expect_true("tree" %in% names(rules))
  expect_true("rules" %in% names(rules))
  expect_true("id" %in% names(rules))
  expect_true(nrow(rules) > 0)
  expect_true(all(sapply(rules$rules, is.language)))
})

test_that("extract_rules.lgb.Booster validates tree parameter", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "binary", max_depth = 2),
    data = dtrain,
    nrounds = 2,
    verbose = -1
  )

  expect_snapshot(extract_rules(bst, tree = 0), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = c(1, 2)), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = "1"), error = TRUE)
  expect_snapshot(extract_rules(bst, tree = 100), error = TRUE)
})

test_that("extract_rules.lgb.Booster handles shallow trees", {
  skip_if_not_installed("lightgbm")

  # Create simple data that will produce a shallow tree
  set.seed(555)
  n <- 50
  data <- data.frame(
    x1 = c(rnorm(n / 2, -1), rnorm(n / 2, 1)),
    y = c(rep(0, n / 2), rep(1, n / 2))
  )
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, "x1", drop = FALSE]),
    label = data$y
  )
  bst <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      max_depth = 1,
      num_leaves = 2
    ),
    data = dtrain,
    nrounds = 1,
    verbose = -1
  )

  rules <- extract_rules(bst, tree = 1)

  expect_true(nrow(rules) >= 1)
  expect_true(all(rules$id >= 1L))
  expect_true(all(sapply(rules$rules, is.language)))
})

test_that("extract_rules.lgb.Booster extracts different trees", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "binary", max_depth = 3),
    data = dtrain,
    nrounds = 5,
    verbose = -1
  )

  rules1 <- extract_rules(bst, tree = 1)
  rules2 <- extract_rules(bst, tree = 3)

  expect_s3_class(rules1, "rule_set")
  expect_s3_class(rules2, "rule_set")
  expect_equal(rules1$tree[1], 1L)
  expect_equal(rules2$tree[1], 3L)
})

test_that("extract_rules works with multiclass models", {
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
      max_depth = 2,
      objective = "multi:softmax",
      num_class = 3
    ),
    data = dtrain,
    nrounds = 2,
    verbose = 0
  )

  # Extract rules from different trees (one per class per round)
  rules1 <- extract_rules(bst, tree = 1)
  rules2 <- extract_rules(bst, tree = 2)
  rules3 <- extract_rules(bst, tree = 4) # Different round

  expect_s3_class(rules1, "rule_set")
  expect_s3_class(rules2, "rule_set")
  expect_s3_class(rules3, "rule_set")

  expect_equal(rules1$tree[1], 1L)
  expect_equal(rules2$tree[1], 2L)
  expect_equal(rules3$tree[1], 4L)
})

test_that("rule_text formats rules as text", {
  skip_if_not_installed("xgboost")

  data <- get_binary_data()
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 2, objective = "binary:logistic"),
    data = dtrain,
    nrounds = 1,
    verbose = 0
  )

  rules <- extract_rules(bst, tree = 1)

  # Test that rule_text works on extracted rules
  for (i in seq_len(nrow(rules))) {
    text <- rule_text(rules$rules[[i]])
    expect_true(is.character(text))
    expect_true(nchar(text) > 0)
  }
})

test_that("extract_rules handles regression trees", {
  skip_if_not_installed("xgboost")

  data <- get_regression_data()
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = data$y
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 3, objective = "reg:squarederror"),
    data = dtrain,
    nrounds = 3,
    verbose = 0
  )

  rules <- extract_rules(bst, tree = 1)

  expect_s3_class(rules, "rule_set")
  expect_true(nrow(rules) > 0)
  expect_equal(rules$tree[1], 1L)
})

test_that("extract_rules IDs are 1-based", {
  skip_if_not_installed("xgboost")

  data <- get_binary_data()
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- xgboost::xgb.train(
    params = list(max_depth = 3, objective = "binary:logistic"),
    data = dtrain,
    nrounds = 1,
    verbose = 0
  )

  rules <- extract_rules(bst, tree = 1)

  # IDs should start from 1
  expect_true(all(rules$id >= 1))
  # IDs should be unique
  expect_equal(length(unique(rules$id)), nrow(rules))
})

test_that("extract_rules returns sorted by ID", {
  skip_if_not_installed("lightgbm")

  data <- get_binary_data()
  dtrain <- lightgbm::lgb.Dataset(
    as.matrix(data[, c("x1", "x2", "x3")]),
    label = as.numeric(data$y) - 1
  )
  bst <- lightgbm::lgb.train(
    params = list(objective = "binary", max_depth = 3),
    data = dtrain,
    nrounds = 1,
    verbose = -1
  )

  rules <- extract_rules(bst, tree = 1)

  # Should be sorted by id
  expect_true(all(rules$id == sort(rules$id)))
})
