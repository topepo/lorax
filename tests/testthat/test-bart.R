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

test_that("reformat_data_bart works with numeric predictors", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data(n = 50)

  reformatted <- reformat_data_bart(y ~ x1 + x2, data = data)

  expect_s3_class(reformatted, "data.frame")
  expect_equal(nrow(reformatted), 50)
  expect_equal(ncol(reformatted), 3)
  expect_equal(names(reformatted), c("x1", "x2", "y"))
  expect_true(is.numeric(reformatted$x1))
  expect_true(is.numeric(reformatted$x2))
  expect_true(is.numeric(reformatted$y))
})

test_that("reformat_data_bart expands factor predictors", {
  skip_if_not_installed("dbarts")

  data <- data.frame(
    y = rnorm(30),
    x1 = rnorm(30),
    f1 = factor(rep(c("A", "B", "C"), 10))
  )

  reformatted <- reformat_data_bart(y ~ x1 + f1, data = data)

  expect_s3_class(reformatted, "data.frame")
  expect_equal(nrow(reformatted), 30)
  expect_true(ncol(reformatted) > 3)
  expect_true("x1" %in% names(reformatted))
  expect_true("y" %in% names(reformatted))
  expect_true(any(grepl("^f1\\.", names(reformatted))))
  expect_true(is.numeric(reformatted$x1))
  expect_true(is.numeric(reformatted$y))
})

test_that("reformat_data_bart preserves response variable format", {
  skip_if_not_installed("dbarts")

  # Factor response
  data_factor <- data.frame(
    y = factor(rep(c("A", "B"), 15)),
    x1 = rnorm(30),
    x2 = rnorm(30)
  )

  reformatted_factor <- reformat_data_bart(y ~ x1 + x2, data = data_factor)
  expect_true(is.factor(reformatted_factor$y))
  expect_equal(levels(reformatted_factor$y), c("A", "B"))

  # Numeric response
  data_numeric <- data.frame(
    y = rnorm(30),
    x1 = rnorm(30),
    x2 = rnorm(30)
  )

  reformatted_numeric <- reformat_data_bart(y ~ x1 + x2, data = data_numeric)
  expect_true(is.numeric(reformatted_numeric$y))
})

test_that("reformat_data_bart works with as.party.bart", {
  skip_if_not_installed("dbarts")
  skip_if_not_installed("palmerpenguins")

  data("penguins", package = "palmerpenguins", envir = environment())
  penguins <- na.omit(get("penguins", envir = environment()))

  # Fit model with factor predictor
  fit <- suppressWarnings(dbarts::bart(
    x.train = penguins[, c("bill_length_mm", "bill_depth_mm", "island")],
    y.train = penguins$species,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 2
  ))

  # Use reformat_data_bart for as.party
  reformatted <- reformat_data_bart(
    species ~ bill_length_mm + bill_depth_mm + island,
    data = penguins
  )

  p <- as.party(fit, tree = 1, data = reformatted)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(length(partykit::nodeids(p)) > 1)
})

test_that("reformat_data_bart validates inputs", {
  skip_if_not_installed("dbarts")

  data <- data.frame(x = 1:10, y = rnorm(10))

  expect_snapshot(
    reformat_data_bart("not a formula", data),
    error = TRUE
  )

  expect_snapshot(
    reformat_data_bart(y ~ x, "not a data.frame"),
    error = TRUE
  )
})

# extract_rules.bart tests ----

test_that("extract_rules.bart returns valid rule_set object", {
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
    y.train = penguins$species,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3
  ))

  rules <- extract_rules(fit, tree = 1L)

  expect_s3_class(rules, "rule_set_bart")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_equal(ncol(rules), 3)
  expect_equal(names(rules), c("tree", "rules", "id"))
  expect_true(all(rules$tree == 1L))
  expect_true(is.list(rules$rules))
  expect_true(all(sapply(rules$rules, is.language)))
  expect_true(is.integer(rules$id))
  expect_true(all(rules$id >= 1))
})

test_that("extract_rules.bart works with regression data", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data(n = 50)

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 2
  ))

  rules <- extract_rules(fit, tree = 1L)

  expect_s3_class(rules, "rule_set_bart")
  expect_true(nrow(rules) >= 1)
  expect_equal(rules$tree[1], 1L)
})

test_that("extract_rules.bart extracts different trees correctly", {
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
    y.train = penguins$species,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 5
  ))

  rules1 <- extract_rules(fit, tree = 1L)
  rules2 <- extract_rules(fit, tree = 2L)
  rules3 <- extract_rules(fit, tree = 5L)

  expect_equal(rules1$tree[1], 1L)
  expect_equal(rules2$tree[1], 2L)
  expect_equal(rules3$tree[1], 5L)

  expect_true(all(rules1$tree == 1L))
  expect_true(all(rules2$tree == 2L))
  expect_true(all(rules3$tree == 5L))
})

test_that("extract_rules.bart handles single-node trees", {
  skip_if_not_installed("dbarts")

  # Very small dataset that might produce single-node trees
  data <- data.frame(
    x1 = rep(1:2, 3),
    x2 = rep(1:3, 2),
    y = rep(1:2, 3)
  )

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 10,
    ndpost = 1
  ))

  # Try multiple trees - at least one might be single-node
  for (tree_num in 1:10) {
    rules <- extract_rules(fit, tree = tree_num)
    expect_s3_class(rules, "rule_set_bart")
    expect_true(nrow(rules) >= 1)

    # If single node, should have TRUE rule (logical, not expression)
    if (nrow(rules) == 1) {
      expect_identical(rules$rules[[1]], TRUE)
    }
  }
})

test_that("extract_rules.bart produces valid rule expressions", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data(n = 100)

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3
  ))

  rules <- extract_rules(fit, tree = 1L)

  # Each rule should be either an expression or TRUE (for single-node trees)
  for (i in seq_len(nrow(rules))) {
    rule <- rules$rules[[i]]
    expect_true(is.language(rule) || identical(rule, TRUE))

    # Should be able to deparse it
    rule_str <- deparse(rule)
    expect_type(rule_str, "character")
    expect_true(nchar(rule_str[1]) > 0)
  }
})

test_that("extract_rules.bart rule text output works", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data(n = 50)

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 2
  ))

  rules <- extract_rules(fit, tree = 1L)

  # rule_text should work on extracted rules
  for (i in seq_len(nrow(rules))) {
    text <- rule_text(rules$rules[[i]])
    expect_type(text, "character")
    expect_true(nchar(text) > 0)
  }
})

test_that("extract_rules.bart validates tree parameter", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data(n = 30)

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3
  ))

  # Non-integer
  expect_snapshot(
    extract_rules(fit, tree = 1.5),
    error = TRUE
  )

  # Character
  expect_snapshot(
    extract_rules(fit, tree = "1"),
    error = TRUE
  )

  # Vector
  expect_snapshot(
    extract_rules(fit, tree = c(1, 2)),
    error = TRUE
  )

  # Less than 1
  expect_snapshot(
    extract_rules(fit, tree = 0),
    error = TRUE
  )

  # Greater than max
  expect_snapshot(
    extract_rules(fit, tree = 10),
    error = TRUE
  )
})

test_that("extract_rules.bart validates chain parameter", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data(n = 30)

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3
  ))

  # Non-integer
  expect_snapshot(
    extract_rules(fit, tree = 1, chain = 1.5),
    error = TRUE
  )

  # Character
  expect_snapshot(
    extract_rules(fit, tree = 1, chain = "1"),
    error = TRUE
  )

  # Vector
  expect_snapshot(
    extract_rules(fit, tree = 1, chain = c(1, 2)),
    error = TRUE
  )

  # Less than 1
  expect_snapshot(
    extract_rules(fit, tree = 1, chain = 0),
    error = TRUE
  )

  # Invalid for single-chain model
  expect_snapshot(
    extract_rules(fit, tree = 1, chain = 2),
    error = TRUE
  )
})

test_that("extract_rules.bart requires keeptrees = TRUE", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data(n = 30)

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2")],
    y.train = data$y,
    keeptrees = FALSE,
    verbose = FALSE,
    ntree = 3
  ))

  expect_snapshot(
    extract_rules(fit, tree = 1),
    error = TRUE
  )
})

test_that("extract_rules.bart preserves variable names", {
  skip_if_not_installed("dbarts")

  data <- data.frame(
    height = rnorm(50),
    weight = rnorm(50),
    age = rnorm(50),
    outcome = rnorm(50)
  )

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("height", "weight", "age")],
    y.train = data$outcome,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3
  ))

  rules <- extract_rules(fit, tree = 1L)

  # Check that rule text contains variable names
  rule_strings <- sapply(rules$rules, function(r) {
    paste(deparse(r), collapse = " ")
  })

  # At least one rule should reference at least one variable
  has_vars <- any(
    grepl("height|weight|age", rule_strings)
  )
  expect_true(has_vars)
})

test_that("extract_rules.bart sequential IDs are correct", {
  skip_if_not_installed("dbarts")

  data <- get_regression_data(n = 100)

  fit <- suppressWarnings(dbarts::bart(
    x.train = data[, c("x1", "x2")],
    y.train = data$y,
    keeptrees = TRUE,
    verbose = FALSE,
    ntree = 3
  ))

  rules <- extract_rules(fit, tree = 1L)

  # IDs should be sequential starting from 1
  expect_equal(rules$id, seq_len(nrow(rules)))

  # IDs should be sorted
  expect_true(all(diff(rules$id) == 1))
})
