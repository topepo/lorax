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
