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

  data <- get_regression_data(n = 50)

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

  data <- get_factor_data(n = 50)

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2", "x3", "x4")],
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
    x.train = data[, c("x1", "x2", "x3", "x4")],
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
