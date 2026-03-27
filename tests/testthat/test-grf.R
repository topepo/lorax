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
