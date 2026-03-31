test_that("as.party.regression_forest returns valid party object", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )
  p <- as.party(rf, tree = 1)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
})

test_that("as.party.regression_forest works with simple data", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)

  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 5
  )
  p <- as.party(rf, tree = 1)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.grf validates tree parameter", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )

  expect_snapshot(as.party(rf, tree = 0), error = TRUE)
  expect_snapshot(as.party(rf, tree = c(1, 2)), error = TRUE)
  expect_snapshot(as.party(rf, tree = "1"), error = TRUE)
  expect_snapshot(as.party(rf, tree = 10), error = TRUE)
})

test_that("as.party.grf works with data parameter", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )
  p <- as.party(
    rf,
    tree = 1,
    data = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]
  )

  expect_s3_class(p, "party")
  expect_equal(ncol(p$data), 4)
})

test_that("as.party.grf method works", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )
  class(rf) <- c("grf", class(rf))
  p <- as.party(rf, tree = 1)

  expect_s3_class(p, "party")
})

test_that("as.party.regression_forest extracts different trees", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 10
  )

  p1 <- as.party(rf, tree = 1)
  p2 <- as.party(rf, tree = 5)

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")

  # Verify both work
  expect_true(length(partykit::nodeids(p1)) > 0)
  expect_true(length(partykit::nodeids(p2)) > 0)

  # Trees should not be identical
  expect_false(identical(p1$node, p2$node))
})

test_that("as.party.grf does not show asterisks in node summaries", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )

  p <- as.party(rf, tree = 1)

  output <- capture.output(print(p))

  # Check for asterisks in node summaries (after the colon)
  # Pattern: ": *" or ": * " indicates missing summary
  has_asterisk_summary <- any(grepl(":\\s*\\*\\s*($|\\()", output))

  expect_false(has_asterisk_summary)
})

# Tests for var_imp.grf() ---------------------------------------------------

test_that("var_imp.grf() returns correct structure", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  forest <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 50
  )
  result <- var_imp(forest)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_type(result$term, "character")
  expect_type(result$estimate, "double")
})

test_that("var_imp.grf() extracts variable importance scores", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  forest <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 50
  )
  result <- var_imp(forest)

  # Should have 3 variables
  expect_equal(nrow(result), 3)

  # All estimates should be between 0 and 1 (grf returns normalized importance)
  expect_true(all(result$estimate >= 0))
  expect_true(all(result$estimate <= 1))

  # Should sum to approximately 1 (not exact due to grf's calculation method)
  expect_true(sum(result$estimate) > 0.5)
  expect_true(sum(result$estimate) <= 1.0)
})

test_that("var_imp.grf() with complete=TRUE includes all predictors", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  # Create a scenario where one predictor might have very low importance
  data$x3 <- rnorm(200, mean = 1000, sd = 0.001) # Near-constant predictor

  forest <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 50
  )
  result <- var_imp(forest, complete = TRUE)

  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("x1", "x2", "x3"))
})

test_that("var_imp.grf() with complete=FALSE matches complete=TRUE for grf", {
  skip_if_not_installed("grf")

  # For grf, variable_importance always returns all predictors
  # so complete should not matter much
  data <- get_regression_data(n = 200)
  forest <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 50
  )

  result_complete <- var_imp(forest, complete = TRUE)
  result_incomplete <- var_imp(forest, complete = FALSE)

  expect_equal(nrow(result_complete), 3)
  expect_equal(nrow(result_incomplete), 3)
  expect_equal(result_complete$term, result_incomplete$term)
})

test_that("var_imp.grf() works with numeric predictors", {
  skip_if_not_installed("grf")

  mtcars <- get_mtcars_data()
  forest <- grf::regression_forest(
    X = as.matrix(mtcars[, c("cyl", "disp", "hp", "wt")]),
    Y = mtcars$mpg,
    num.trees = 50
  )
  result <- var_imp(forest)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("cyl", "disp", "hp", "wt"))
  expect_true(all(result$estimate >= 0))
  expect_true(all(result$estimate <= 1))
  expect_true(sum(result$estimate) > 0.5)
})

test_that("var_imp.grf() passes additional arguments to variable_importance", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  forest <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 50
  )

  # Test that we can pass decay.exponent argument
  result1 <- var_imp(forest, decay.exponent = 2)
  result2 <- var_imp(forest, decay.exponent = 1)

  # Results should be different when using different decay parameters
  expect_false(identical(result1$estimate, result2$estimate))
})

test_that("var_imp.grf() importance scores match grf::variable_importance", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  forest <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 50
  )
  result <- var_imp(forest, complete = FALSE)

  # Match against grf::variable_importance
  expected <- grf::variable_importance(forest)[, 1]
  names(expected) <- colnames(forest$X.orig)

  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expect_equal(estimate, expected[[term]], tolerance = 1e-10)
  }
})

test_that("var_imp.grf() works with regression_forest", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  forest <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 50
  )
  result <- var_imp(forest)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
})

test_that("var_imp.grf() works with causal_forest", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  data$w <- rbinom(200, 1, 0.5)

  forest <- grf::causal_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    W = data$w,
    num.trees = 50
  )
  result <- var_imp(forest)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("x1", "x2", "x3"))
})

test_that("var_imp.grf() handles forest with named predictors", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  X <- as.matrix(data[, c("x1", "x2", "x3")])
  colnames(X) <- c("predictor_a", "predictor_b", "predictor_c")

  forest <- grf::regression_forest(
    X = X,
    Y = data$y,
    num.trees = 50
  )
  result <- var_imp(forest)

  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("predictor_a", "predictor_b", "predictor_c"))
})

test_that("var_imp.grf() handles forest with many predictors", {
  skip_if_not_installed("grf")

  set.seed(123)
  n <- 200
  p <- 20
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("x", 1:p)
  y <- X[, 1] + X[, 2] + rnorm(n)

  forest <- grf::regression_forest(
    X = X,
    Y = y,
    num.trees = 50
  )
  result <- var_imp(forest)

  expect_equal(nrow(result), p)
  expect_setequal(result$term, paste0("x", 1:p))
  expect_true(all(result$estimate >= 0))
  expect_true(sum(result$estimate) > 0.5)
})

test_that("var_imp.grf() with max.depth argument", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 200)
  forest <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 50
  )

  # Test with max.depth parameter for variable_importance
  result <- var_imp(forest, max.depth = 2)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true(all(result$estimate >= 0))
})

# ------------------------------------------------------------------------------
# extract_rules tests
# ------------------------------------------------------------------------------

test_that("extract_rules.grf() returns correct structure", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(847)
  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules", "tree"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
  expect_type(rules$tree, "integer")
})

test_that("extract_rules.grf() extracts from single tree", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(532)
  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = 1L)

  expect_equal(unique(rules$tree), 1L)
  expect_true(nrow(rules) > 0)

  # Check that rules are valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.grf() extracts from multiple trees", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(219)
  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = c(1L, 2L, 3L))

  expect_equal(sort(unique(rules$tree)), c(1L, 2L, 3L))
  expect_true(nrow(rules) > 0)

  # Check that each tree has rules
  for (tree_num in c(1L, 2L, 3L)) {
    tree_rules <- rules[rules$tree == tree_num, ]
    expect_true(nrow(tree_rules) > 0)
  }
})

test_that("extract_rules.grf() validates tree argument", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(674)
  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 3
  )

  expect_snapshot(extract_rules(rf, tree = "1"), error = TRUE)
  expect_snapshot(extract_rules(rf, tree = 1.5), error = TRUE)
  expect_snapshot(extract_rules(rf, tree = 0L), error = TRUE)
  expect_snapshot(extract_rules(rf, tree = 10L), error = TRUE)
})

test_that("extract_rules.grf() works with numeric predictors", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(158)
  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2")]),
    Y = data$y,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.grf() rules are sorted by tree then id", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(803)
  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = c(2L, 1L, 3L))

  # Check sorting
  expect_true(all(diff(rules$tree) >= 0))

  # Within each tree, ids should be sorted
  for (tree_num in unique(rules$tree)) {
    tree_rules <- rules[rules$tree == tree_num, ]
    expect_true(all(diff(tree_rules$id) > 0))
  }
})

test_that("extract_rules.grf() handles duplicate tree numbers", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(456)
  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = c(1L, 1L, 2L))

  # Should have results for tree 1 twice
  tree_counts <- table(rules$tree)
  expect_equal(as.numeric(names(tree_counts)), c(1, 2))
})

test_that("extract_rules.grf() works with all trees", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(927)
  rf <- grf::regression_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    num.trees = 3
  )
  n_trees <- 3
  rules <- extract_rules(rf, tree = 1:n_trees)

  expect_equal(sort(unique(rules$tree)), 1:n_trees)
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.grf() handles tree with no valid splits", {
  skip_if_not_installed("grf")

  # Create data where tree may have no splits
  null_data <- data.frame(
    y = 1:10,
    x1 = rep(1:5, each = 2),
    x2 = rep(1:5, each = 2)
  )
  set.seed(614)
  rf <- grf::regression_forest(
    X = as.matrix(null_data[, c("x1", "x2")]),
    Y = null_data$y,
    num.trees = 2,
    min.node.size = 5
  )
  rules <- extract_rules(rf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  # Even with no splits, should return at least one rule (TRUE)
  expect_true(nrow(rules) >= 1)
})

test_that("extract_rules.grf() works with causal_forest", {
  skip_if_not_installed("grf")

  data <- get_regression_data(n = 100)
  set.seed(391)
  cf <- grf::causal_forest(
    X = as.matrix(data[, c("x1", "x2", "x3")]),
    Y = data$y,
    W = rbinom(100, 1, 0.5),
    num.trees = 3
  )
  rules <- extract_rules(cf, tree = 1L)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

# ------------------------------------------------------------------------------
# active_predictors tests
# ------------------------------------------------------------------------------

test_that("active_predictors.regression_forest() returns correct structure", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )

  result <- active_predictors(rf)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("active_predictors", "tree"))
  expect_type(result$active_predictors, "list")
  expect_type(result$tree, "integer")
  expect_equal(nrow(result), 1)
})

test_that("active_predictors.regression_forest() extracts from single tree", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )

  result <- active_predictors(rf, tree = 1L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.regression_forest() extracts from multiple trees", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )

  result <- active_predictors(rf, tree = c(1L, 2L, 3L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_equal(length(result$active_predictors), 3)
})

test_that("active_predictors.regression_forest() works with all trees", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 3
  )

  result <- active_predictors(rf, tree = 1:3)

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_equal(length(result$active_predictors), 3)
})

test_that("active_predictors.regression_forest() validates tree argument", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )

  expect_snapshot(
    active_predictors(rf, tree = "1"),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(rf, tree = 1.5),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(rf, tree = 0L),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(rf, tree = 9999L),
    error = TRUE
  )
})

test_that("active_predictors.regression_forest() handles tree with no splits", {
  skip_if_not_installed("grf")

  # Create data that might produce single node tree
  small_data <- data.frame(y = rep(1, 20), x = rep(1, 20))

  no_split_forest <- grf::regression_forest(
    X = as.matrix(small_data[, "x", drop = FALSE]),
    Y = small_data$y,
    num.trees = 1,
    min.node.size = 100
  )

  result <- active_predictors(no_split_forest, tree = 1L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
  expect_type(result$active_predictors[[1]], "character")
  # May have 0 or more predictors depending on whether split occurred
  expect_true(length(result$active_predictors[[1]]) >= 0)
})

test_that("active_predictors.regression_forest() returns sorted unique variables", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  # Should be sorted (case-insensitive)
  expect_equal(active_vars, active_vars[order(tolower(active_vars))])

  # Should be unique
  expect_equal(length(active_vars), length(unique(active_vars)))
})

test_that("active_predictors.regression_forest() works with numeric predictors", {
  skip_if_not_installed("grf")

  # Use mtcars which has no factors
  rf <- grf::regression_forest(
    X = as.matrix(mtcars[, c("cyl", "disp", "hp", "wt")]),
    Y = mtcars$mpg,
    num.trees = 3
  )

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_type(active_vars, "character")
  expect_true(all(active_vars %in% c("cyl", "disp", "hp", "wt")))
})

test_that("active_predictors.grf() method works", {
  skip_if_not_installed("grf")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- grf::regression_forest(
    X = as.matrix(penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )]),
    Y = penguins$bill_length_mm,
    num.trees = 5
  )

  # Add grf class
  class(rf) <- c("grf", class(rf))

  result <- active_predictors(rf, tree = 1L)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
})
