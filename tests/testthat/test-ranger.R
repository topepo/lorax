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
