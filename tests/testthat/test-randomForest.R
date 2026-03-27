test_that("as.party.randomForest returns valid party object", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)
  p <- as.party(rf, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
})

test_that("as.party.randomForest works with binary classification", {
  skip_if_not_installed("randomForest")

  data <- get_binary_data(n = 100)

  rf <- randomForest::randomForest(y ~ ., data = data, ntree = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.randomForest works with regression", {
  skip_if_not_installed("randomForest")

  data <- get_regression_data(n = 100)

  rf <- randomForest::randomForest(y ~ ., data = data, ntree = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.randomForest works with factor predictors", {
  skip_if_not_installed("randomForest")

  data <- get_factor_data(n = 100)

  rf <- randomForest::randomForest(y ~ ., data = data, ntree = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.randomForest works with wa_trees data (mixed types)", {
  skip_if_not_installed("randomForest")

  wa_trees <- get_wa_trees_data()

  rf <- randomForest::randomForest(
    class ~ .,
    data = wa_trees,
    ntree = 5,
    maxnodes = 10
  )
  p <- as.party(rf, tree = 1, data = wa_trees)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.randomForest validates tree parameter", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  expect_snapshot(as.party(rf, tree = 0), error = TRUE)
  expect_snapshot(as.party(rf, tree = 10), error = TRUE)
  expect_snapshot(as.party(rf, tree = c(1, 2)), error = TRUE)
  expect_snapshot(as.party(rf, tree = "1"), error = TRUE)
})

test_that("as.party.randomForest works without data parameter", {
  skip_if_not_installed("randomForest")

  # Use mtcars which is in global environment
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 5)
  p <- as.party(rf, tree = 1)

  expect_s3_class(p, "party")
  expect_true(ncol(p$data) >= 4)
})

test_that("as.party.randomForest requires keep.forest = TRUE", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    ntree = 5,
    keep.forest = FALSE
  )

  expect_snapshot(as.party(rf, tree = 1), error = TRUE)
})

test_that("as.party.randomForest extracts different trees", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 10)

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

test_that("as.party.randomForest does not show asterisks in node summaries", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)
  p <- as.party(rf, tree = 1, data = penguins)

  output <- capture.output(print(p))

  # Check for asterisks in node summaries (after the colon)
  # Pattern: ": *" or ": * " indicates missing summary
  has_asterisk_summary <- any(grepl(":\\s*\\*\\s*($|\\()", output))

  expect_false(has_asterisk_summary)
})

# Tests for active_predictors.randomForest() ---------------------------------

test_that("active_predictors.randomForest() returns correct structure", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("active_predictors", "tree"))
  expect_type(result$active_predictors, "list")
  expect_type(result$tree, "integer")
  expect_equal(nrow(result), 1)
})

test_that("active_predictors.randomForest() extracts from single tree", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf, tree = 1L)

  expect_equal(nrow(result), 1)
  expect_equal(result$tree, 1L)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.randomForest() extracts from multiple trees", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf, tree = c(1L, 2L, 3L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 2L, 3L))
  expect_equal(length(result$active_predictors), 3)
})

test_that("active_predictors.randomForest() validates tree argument", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

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

test_that("active_predictors.randomForest() requires keep.forest = TRUE", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    ntree = 5,
    keep.forest = FALSE
  )

  expect_snapshot(
    active_predictors(rf, tree = 1L),
    error = TRUE
  )
})

test_that("active_predictors.randomForest() handles mixed numeric and factor predictors", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_type(active_vars, "character")
  expect_false(any(grepl("_Adelie|_Chinstrap|_Gentoo", active_vars)))
  expect_false(any(grepl("_Biscoe|_Dream|_Torgersen", active_vars)))
  expect_false(any(grepl("_female|_male", active_vars)))
})

test_that("active_predictors.randomForest() returns sorted unique variables", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_equal(active_vars, active_vars[order(tolower(active_vars))])
  expect_equal(length(active_vars), length(unique(active_vars)))
})

test_that("active_predictors.randomForest() handles numeric-only predictors", {
  skip_if_not_installed("randomForest")

  data <- get_regression_data(n = 100)
  rf <- randomForest::randomForest(y ~ ., data = data, ntree = 5)

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_type(active_vars, "character")
  expect_true(all(active_vars %in% c("x1", "x2", "x3")))
})

test_that("active_predictors.randomForest() works with all trees", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf, tree = 1:rf$ntree)

  expect_equal(nrow(result), rf$ntree)
  expect_equal(result$tree, 1:rf$ntree)
  expect_equal(length(result$active_predictors), rf$ntree)
})

test_that("active_predictors.randomForest() handles duplicate tree numbers", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf, tree = c(1L, 1L, 2L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 1L, 2L))
})
