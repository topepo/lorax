test_that("as.party.ranger returns valid party object", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)
  p <- as.party(rf, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
  expect_equal(ncol(p$data), 5)
})

test_that("as.party.ranger works with binary classification", {
  skip_if_not_installed("ranger")

  data <- get_binary_data(n = 100)

  rf <- ranger::ranger(y ~ ., data = data, num.trees = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.ranger works with regression", {
  skip_if_not_installed("ranger")

  data <- get_regression_data(n = 100)

  rf <- ranger::ranger(y ~ ., data = data, num.trees = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.ranger works with factor predictors", {
  skip_if_not_installed("ranger")

  data <- get_factor_data(n = 100)

  rf <- ranger::ranger(y ~ ., data = data, num.trees = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.ranger works with wa_trees data (mixed types)", {
  skip_if_not_installed("ranger")

  wa_trees <- get_wa_trees_data()

  rf <- ranger::ranger(class ~ ., data = wa_trees, num.trees = 5, max.depth = 5)
  p <- as.party(rf, tree = 1, data = wa_trees)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.ranger validates tree parameter", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

  expect_snapshot(as.party(rf, tree = 0, data = penguins), error = TRUE)
  expect_snapshot(as.party(rf, tree = 10, data = penguins), error = TRUE)
  expect_snapshot(as.party(rf, tree = c(1, 2), data = penguins), error = TRUE)
  expect_snapshot(as.party(rf, tree = "1", data = penguins), error = TRUE)
})

test_that("as.party.ranger requires data parameter", {
  skip_if_not_installed("ranger")

  rf <- ranger::ranger(Species ~ ., data = iris, num.trees = 5)

  expect_snapshot(as.party(rf, tree = 1), error = TRUE)
})

test_that("as.party.ranger requires write.forest = TRUE", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- ranger::ranger(
    species ~ .,
    data = penguins,
    num.trees = 5,
    write.forest = FALSE
  )

  expect_snapshot(as.party(rf, tree = 1, data = penguins), error = TRUE)
})

test_that("as.party.ranger extracts different trees", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 10)

  p1 <- as.party(rf, tree = 1, data = penguins)
  p2 <- as.party(rf, tree = 5, data = penguins)

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")

  # Verify both work
  expect_true(length(partykit::nodeids(p1)) > 0)
  expect_true(length(partykit::nodeids(p2)) > 0)

  # Trees should not be identical
  expect_false(identical(p1$node, p2$node))
})

test_that("as.party.ranger does not show asterisks in node summaries", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)
  p <- as.party(rf, tree = 1, data = penguins)

  output <- capture.output(print(p))

  # Check for asterisks in node summaries (after the colon)
  # Pattern: ": *" or ": * " indicates missing summary
  has_asterisk_summary <- any(grepl(":\\s*\\*\\s*($|\\()", output))

  expect_false(has_asterisk_summary)
})

# Tests for active_predictors.ranger() ---------------------------------------

test_that("active_predictors.ranger() returns correct structure", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

  result <- active_predictors(rf)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("active_predictors", "tree"))
  expect_type(result$active_predictors, "list")
  expect_type(result$tree, "integer")
  expect_equal(nrow(result), 1)
})

test_that("active_predictors.ranger() extracts from single tree", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

  result <- active_predictors(rf, tree = 1L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.ranger() extracts from multiple trees", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

  result <- active_predictors(rf, tree = c(1L, 2L, 3L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_equal(length(result$active_predictors), 3)
})

test_that("active_predictors.ranger() validates tree argument", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

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
    active_predictors(rf, tree = 11L),
    error = TRUE
  )
})

test_that("active_predictors.ranger() requires write.forest = TRUE", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- ranger::ranger(
    species ~ .,
    data = penguins,
    num.trees = 5,
    write.forest = FALSE
  )

  expect_snapshot(
    active_predictors(rf, tree = 1L),
    error = TRUE
  )
})

test_that("active_predictors.ranger() handles mixed numeric and factor predictors", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_type(active_vars, "character")
  expect_false(any(grepl("_Adelie|_Chinstrap|_Gentoo", active_vars)))
  expect_false(any(grepl("_Biscoe|_Dream|_Torgersen", active_vars)))
  expect_false(any(grepl("_female|_male", active_vars)))
})

test_that("active_predictors.ranger() returns sorted unique variables", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_equal(active_vars, active_vars[order(tolower(active_vars))])
  expect_equal(length(active_vars), length(unique(active_vars)))
})

test_that("active_predictors.ranger() handles numeric-only predictors", {
  skip_if_not_installed("ranger")

  data <- get_regression_data(n = 100)
  rf <- ranger::ranger(y ~ ., data = data, num.trees = 5)

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_type(active_vars, "character")
  expect_true(all(active_vars %in% c("x1", "x2", "x3")))
})

test_that("active_predictors.ranger() works with all trees", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

  result <- active_predictors(rf, tree = 1:rf$num.trees)

  expect_equal(nrow(result), rf$num.trees)
  expect_equal(result$tree, 1:rf$num.trees)
  expect_equal(length(result$active_predictors), rf$num.trees)
})

test_that("active_predictors.ranger() handles duplicate tree numbers", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

  result <- active_predictors(rf, tree = c(1L, 1L, 2L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 1L, 2L))
})
