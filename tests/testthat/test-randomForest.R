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
