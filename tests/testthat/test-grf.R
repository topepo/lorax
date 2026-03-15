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
})
