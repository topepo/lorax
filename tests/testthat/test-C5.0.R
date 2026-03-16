test_that("as.party.C5.0 returns valid party object for single tree", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  c5_model <- C50::C5.0(species ~ ., data = penguins)
  p <- as.party(c5_model, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
})

test_that("as.party.C5.0 works with binary classification", {
  skip_if_not_installed("C50")

  data <- get_binary_data(n = 100)

  c5_model <- C50::C5.0(y ~ ., data = data)
  p <- as.party(c5_model, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.C5.0 works with iris data", {
  skip_if_not_installed("C50")

  iris_data <- get_iris_data()

  c5_model <- C50::C5.0(Species ~ ., data = iris_data)
  p <- as.party(c5_model, tree = 1, data = iris_data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.C5.0 validates tree parameter", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_model <- C50::C5.0(species ~ ., data = penguins)

  expect_snapshot(as.party(c5_model, tree = 0, data = penguins), error = TRUE)
  # When tree exceeds num_trials, it should warn and use max tree
  expect_snapshot(as.party(c5_model, tree = 5, data = penguins))
  expect_snapshot(
    as.party(c5_model, tree = c(1, 2), data = penguins),
    error = TRUE
  )
  expect_snapshot(as.party(c5_model, tree = "1", data = penguins), error = TRUE)
})

test_that("as.party.C5.0 requires data parameter", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_model <- C50::C5.0(species ~ ., data = penguins)

  expect_snapshot(as.party(c5_model, tree = 1), error = TRUE)
})

test_that("as.party.C5.0 works with boosted models", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_boost <- C50::C5.0(species ~ ., data = penguins, trials = 10)

  # Extract first and second trees
  p1 <- as.party(c5_boost, tree = 1, data = penguins)
  p2 <- as.party(c5_boost, tree = 2, data = penguins)

  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")

  # Trees should be different
  expect_false(isTRUE(all.equal(p1, p2)))

  # Check tree number validation
  expect_snapshot(as.party(c5_boost, tree = 0, data = penguins), error = TRUE)
  # When tree exceeds num_trials, it should warn and use max tree
  expect_snapshot(as.party(c5_boost, tree = 11, data = penguins))
})

test_that("as.party.C5.0 extracts different boosted trees", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_boost <- C50::C5.0(species ~ ., data = penguins, trials = 5)

  # Extract multiple trees
  p1 <- as.party(c5_boost, tree = 1, data = penguins)
  p2 <- as.party(c5_boost, tree = 2, data = penguins)
  p3 <- as.party(c5_boost, tree = 5, data = penguins)

  # All should be valid
  expect_s3_class(p1, "party")
  expect_s3_class(p2, "party")
  expect_s3_class(p3, "party")

  # Get root splits to verify they're different
  split1 <- partykit::split_node(p1$node)
  split2 <- partykit::split_node(p2$node)

  # At least one property should differ (varid or threshold)
  varid1 <- partykit::varid_split(split1)
  varid2 <- partykit::varid_split(split2)
  breaks1 <- partykit::breaks_split(split1)
  breaks2 <- partykit::breaks_split(split2)

  # Trees should not be identical
  expect_false(
    identical(varid1, varid2) &&
      identical(breaks1, breaks2) &&
      identical(p1$node, p2$node)
  )
})

test_that("as.party.C5.0 rejects rule-based models", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  c5_rules <- C50::C5.0(species ~ ., data = penguins, rules = TRUE)

  expect_snapshot(as.party(c5_rules, tree = 1, data = penguins), error = TRUE)
})

test_that("as.party.C5.0 does not show asterisks in node summaries", {
  skip_if_not_installed("C50")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  c5_model <- C50::C5.0(species ~ ., data = penguins)
  p <- as.party(c5_model, tree = 1, data = penguins)

  output <- capture.output(print(p))

  # Check for asterisks in node summaries (after the colon)
  # Pattern: ": *" or ": * " indicates missing summary
  has_asterisk_summary <- any(grepl(":\\s*\\*\\s*($|\\()", output))

  expect_false(has_asterisk_summary)
})
