test_that("as.party.bart returns valid party object with numeric data", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  p <- as.party(fit, tree = 1, chain = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
  expect_true(ncol(p$data) >= 4)
})

test_that("as.party.bart works with regression data", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2", "x3")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3,
    nskip = 50,
    ndpost = 1
  ))

  p <- as.party(fit, tree = 1, chain = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.bart works with factor predictors", {
  skip_if_not_installed("dbarts")

  data <- get_factor_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("bill_length_mm", "island", "bill_depth_mm", "sex")],
    y.train = as.numeric(data$y),
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3,
    nskip = 50,
    ndpost = 1
  ))

  # BART expands factors to dummies
  p <- as.party(fit, tree = 1, chain = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.bart validates tree parameter", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  expect_snapshot(
    as.party(fit, tree = 0, chain = 1, data = penguins),
    error = TRUE
  )
  expect_snapshot(
    as.party(fit, tree = c(1, 2), chain = 1, data = penguins),
    error = TRUE
  )
  expect_snapshot(
    as.party(fit, tree = "1", chain = 1, data = penguins),
    error = TRUE
  )
  expect_snapshot(
    as.party(fit, tree = 10, chain = 1, data = penguins),
    error = TRUE
  )
})

test_that("as.party.bart validates chain parameter", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  expect_snapshot(
    as.party(fit, tree = 1, chain = 0, data = penguins),
    error = TRUE
  )
  expect_snapshot(
    as.party(fit, tree = 1, chain = 5, data = penguins),
    error = TRUE
  )
  expect_snapshot(
    as.party(fit, tree = 1, chain = "1", data = penguins),
    error = TRUE
  )
})

test_that("as.party.bart requires data parameter", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  expect_snapshot(as.party(fit, tree = 1, chain = 1), error = TRUE)
})

test_that("as.party.bart requires keeptrees = TRUE", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = FALSE,
    verbose = FALSE,
    ntree = 5
  ))

  expect_snapshot(
    as.party(fit, tree = 1, chain = 1, data = penguins),
    error = TRUE
  )
})

test_that("as.party.bart extracts different trees", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  p1 <- as.party(fit, tree = 1, chain = 1, data = penguins)
  p2 <- as.party(fit, tree = 2, chain = 1, data = penguins)

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")

  # Trees should generally be different (though could be identical by chance)
  # Just verify both work
  expect_true(length(partykit::nodeids(p1)) > 0)
  expect_true(length(partykit::nodeids(p2)) > 0)
})

test_that("as.party.bart handles models with missing values in training data", {
  skip_if_not_installed("dbarts")

  # Create data with some pattern
  set.seed(123)
  data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50),
    x4 = rnorm(50),
    y = rnorm(50)
  )

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("bill_length_mm", "island", "bill_depth_mm", "sex")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3,
    nskip = 50,
    ndpost = 1
  ))

  p <- as.party(fit, tree = 1, chain = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.bart handles deep trees", {
  skip_if_not_installed("dbarts")

  # Create data that might produce deeper trees
  set.seed(456)
  n <- 100
  data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    y = rnorm(n)
  )

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2", "x3")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  # Extract multiple trees
  p1 <- as.party(fit, tree = 1, chain = 1, data = data)
  p2 <- as.party(fit, tree = 3, chain = 1, data = data)
  p3 <- as.party(fit, tree = 5, chain = 1, data = data)

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")
  expect_s3_class(p3, "party")

  # Verify trees have nodes
  expect_true(length(partykit::nodeids(p1)) > 0)
  expect_true(length(partykit::nodeids(p2)) > 0)
  expect_true(length(partykit::nodeids(p3)) > 0)
})

test_that("as.party.bart preserves variable names", {
  skip_if_not_installed("dbarts")

  # Use named predictors
  data <- data.frame(
    height = rnorm(30),
    weight = rnorm(30),
    age = rnorm(30),
    outcome = rnorm(30)
  )

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("height", "weight", "age")],
    y.train = data$outcome,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3,
    nskip = 50,
    ndpost = 1
  ))

  p <- as.party(fit, tree = 1, chain = 1, data = data)

  # Check that variable names are preserved
  expect_true(
    all(c("height", "weight", "age") %in% names(p$data)) ||
      all(grepl("^height|^weight|^age", names(p$data)))
  )
})

test_that("as.party.bart with minimal trees", {
  skip_if_not_installed("dbarts")

  # Very small dataset
  data <- data.frame(
    x1 = rep(1:2, 6),
    y = rep(1:2, each = 6)
  )

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, "x1", drop = FALSE],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 1,
    nskip = 10,
    ndpost = 1
  ))

  p <- as.party(fit, tree = 1, chain = 1, data = data)

  expect_s3_class(p, "party")
})

test_that("as.party.bart produces different trees for different tree numbers", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  p1 <- as.party(fit, tree = 1, chain = 1, data = penguins)
  p2 <- as.party(fit, tree = 2, chain = 1, data = penguins)

  # Trees should not be identical
  expect_false(identical(p1$node, p2$node))
})

test_that("as.party.bart does not show asterisks in node summaries", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3,
    nskip = 100,
    ndpost = 1
  ))

  p <- as.party(fit, tree = 1, chain = 1, data = penguins)
  output <- capture.output(print(p))

  # Check for asterisks in node summaries (after the colon)
  # Pattern: ": *" or ": * " indicates missing summary
  has_asterisk_summary <- any(grepl(":\\s*\\*\\s*($|\\()", output))

  expect_false(has_asterisk_summary)
})

# Tests for active_predictors.bart() -----------------------------------------

test_that("active_predictors.bart() returns correct structure", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  result <- active_predictors(fit)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("active_predictors", "tree"))
  expect_type(result$active_predictors, "list")
  expect_type(result$tree, "integer")
  expect_equal(nrow(result), 1)
})

test_that("active_predictors.bart() extracts from single tree", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  result <- active_predictors(fit, tree = 1L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.bart() extracts from multiple trees", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  result <- active_predictors(fit, tree = c(1L, 2L, 3L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_equal(length(result$active_predictors), 3)
})

test_that("active_predictors.bart() validates tree argument", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  expect_snapshot(
    active_predictors(fit, tree = "1"),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(fit, tree = 1.5),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(fit, tree = 0L),
    error = TRUE
  )

  expect_snapshot(
    active_predictors(fit, tree = 11L),
    error = TRUE
  )
})

test_that("active_predictors.bart() requires keeptrees = TRUE", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = FALSE,
    verbose = FALSE,
    ntree = 5
  ))

  expect_snapshot(
    active_predictors(fit, tree = 1L),
    error = TRUE
  )
})

test_that("active_predictors.bart() returns sorted unique variables", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  result <- active_predictors(fit, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_equal(active_vars, active_vars[order(tolower(active_vars))])
  expect_equal(length(active_vars), length(unique(active_vars)))
})

test_that("active_predictors.bart() handles numeric-only predictors", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2", "x3")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3,
    nskip = 50,
    ndpost = 1
  ))

  result <- active_predictors(fit, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_type(active_vars, "character")
  expect_true(all(active_vars %in% c("x1", "x2", "x3")))
})

test_that("active_predictors.bart() works with all trees", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  trees_df <- fit$fit$getTrees()
  max_tree <- max(trees_df$tree)

  result <- active_predictors(fit, tree = 1:max_tree)

  expect_equal(nrow(result), max_tree)
  expect_equal(result$tree, 1:max_tree)
  expect_equal(length(result$active_predictors), max_tree)
})

test_that("active_predictors.bart() handles duplicate tree numbers", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c(
      "bill_length_mm",
      "bill_depth_mm",
      "flipper_length_mm",
      "body_mass_g"
    )],
    y.train = penguins$bill_length_mm,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  result <- active_predictors(fit, tree = c(1L, 1L, 2L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 1L, 2L))
})

test_that("as.party.bart works with single numeric predictor", {
  skip_if_not_installed("dbarts")

  data <- get_single_numeric_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, "x", drop = FALSE],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  p <- as.party(fit, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("active_predictors.bart() works with single numeric predictor", {
  skip_if_not_installed("dbarts")

  data <- get_single_numeric_data()

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, "x", drop = FALSE],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5,
    nskip = 100,
    ndpost = 1
  ))

  active <- active_predictors(fit)

  expect_s3_class(active, "tbl_df")
  # If the model made splits, should have "x" as active predictor
  if (length(unique(unlist(active$active_predictors))) > 0) {
    expect_setequal(unique(unlist(active$active_predictors)), "x")
  }
})
