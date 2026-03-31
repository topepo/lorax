test_that("as.party.randomForest returns valid party object", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  set.seed(834)
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)
  p <- as.party(rf, tree = 1, data = penguins)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
  expect_true(is.data.frame(p$data))
})

test_that("as.party.randomForest works with binary classification", {
  skip_if_not_installed("randomForest")

  data <- get_binary_data(n = 100)

  set.seed(267)
  rf <- randomForest::randomForest(y ~ ., data = data, ntree = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.randomForest works with regression", {
  skip_if_not_installed("randomForest")

  data <- get_regression_data(n = 100)

  set.seed(551)
  rf <- randomForest::randomForest(y ~ ., data = data, ntree = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.randomForest works with factor predictors", {
  skip_if_not_installed("randomForest")

  data <- get_factor_data(n = 100)

  set.seed(912)
  rf <- randomForest::randomForest(y ~ ., data = data, ntree = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.randomForest works with wa_trees data (mixed types)", {
  skip_if_not_installed("randomForest")

  wa_trees <- get_wa_trees_data()

  set.seed(705)
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

  set.seed(463)
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  expect_snapshot(as.party(rf, tree = 0), error = TRUE)
  expect_snapshot(as.party(rf, tree = 10), error = TRUE)
  expect_snapshot(as.party(rf, tree = c(1, 2)), error = TRUE)
  expect_snapshot(as.party(rf, tree = "1"), error = TRUE)
})

test_that("as.party.randomForest works without data parameter", {
  skip_if_not_installed("randomForest")

  # Use mtcars which is in global environment
  set.seed(189)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 5)
  p <- as.party(rf, tree = 1)

  expect_s3_class(p, "party")
  expect_true(ncol(p$data) >= 4)
})

test_that("as.party.randomForest requires keep.forest = TRUE", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()

  set.seed(741)
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

  set.seed(582)
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

  set.seed(328)
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
  set.seed(495)
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
  set.seed(614)
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
  set.seed(729)
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
  set.seed(873)
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

  set.seed(196)
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
  set.seed(452)
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
  set.seed(587)
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf, tree = 1L)
  active_vars <- result$active_predictors[[1]]

  expect_equal(active_vars, active_vars[order(tolower(active_vars))])
  expect_equal(length(active_vars), length(unique(active_vars)))
})

test_that("active_predictors.randomForest() handles numeric-only predictors", {
  skip_if_not_installed("randomForest")

  data <- get_regression_data(n = 100)
  set.seed(314)
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
  set.seed(768)
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
  set.seed(941)
  rf <- randomForest::randomForest(species ~ ., data = penguins, ntree = 5)

  result <- active_predictors(rf, tree = c(1L, 1L, 2L))

  expect_equal(nrow(result), 3)
  expect_equal(result$tree, c(1L, 1L, 2L))
})

# Tests for var_imp.randomForest() -------------------------------------------

test_that("var_imp.randomForest() returns correct structure", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(724)
  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_type(result$term, "character")
  expect_type(result$estimate, "double")
})

test_that("var_imp.randomForest() extracts variable importance scores", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(391)
  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  # Should have all predictors from the model
  expect_true(nrow(result) > 0)

  # All estimates should be numeric
  expect_true(all(is.numeric(result$estimate)))
})

test_that("var_imp.randomForest() uses MeanDecreaseGini by default for classification", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(856)
  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  # Should match MeanDecreaseGini column
  imp_matrix <- randomForest::importance(rf)
  expected <- imp_matrix[, "MeanDecreaseGini"]

  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expect_equal(estimate, expected[[term]], tolerance = 1e-10)
  }
})

test_that("var_imp.randomForest() can extract MeanDecreaseAccuracy", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(217)
  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf, type = "accuracy")

  # Should match MeanDecreaseAccuracy column
  imp_matrix <- randomForest::importance(rf)
  expected <- imp_matrix[, "MeanDecreaseAccuracy"]

  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expect_equal(estimate, expected[[term]], tolerance = 1e-10)
  }
})

test_that("var_imp.randomForest() can extract class-specific importance", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(503)
  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf, type = "Adelie")

  # Should match Adelie column
  imp_matrix <- randomForest::importance(rf)
  expected <- imp_matrix[, "Adelie"]

  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expect_equal(estimate, expected[[term]], tolerance = 1e-10)
  }
})

test_that("var_imp.randomForest() uses IncNodePurity by default for regression", {
  skip_if_not_installed("randomForest")

  mtcars <- get_mtcars_data()
  set.seed(648)
  rf <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  # Should match IncNodePurity column
  imp_matrix <- randomForest::importance(rf)
  expected <- imp_matrix[, "IncNodePurity"]

  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expect_equal(estimate, expected[[term]], tolerance = 1e-10)
  }
})

test_that("var_imp.randomForest() can extract %IncMSE for regression", {
  skip_if_not_installed("randomForest")

  mtcars <- get_mtcars_data()
  set.seed(935)
  rf <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf, type = "permutation")

  # Should match %IncMSE column
  imp_matrix <- randomForest::importance(rf)
  expected <- imp_matrix[, "%IncMSE"]

  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expect_equal(estimate, expected[[term]], tolerance = 1e-10)
  }
})

test_that("var_imp.randomForest() works even with importance=FALSE", {
  skip_if_not_installed("randomForest")

  # randomForest always calculates IncNodePurity even with importance=FALSE
  mtcars <- get_mtcars_data()
  set.seed(172)
  rf <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    ntree = 10,
    importance = FALSE
  )

  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("cyl", "disp", "hp"))
  # Should still get IncNodePurity values
  expect_true(all(result$estimate > 0))
})

test_that("var_imp.randomForest() errors for invalid type", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(809)
  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    importance = TRUE,
    ntree = 10
  )

  expect_snapshot(var_imp(rf, type = "invalid"), error = TRUE)
})

test_that("var_imp.randomForest() with complete=TRUE fills missing predictors", {
  skip_if_not_installed("randomForest")

  set.seed(426)
  data <- get_regression_data(n = 200)
  # Add a near-constant predictor
  data$x4 <- rnorm(200, mean = 1000, sd = 0.001)

  set.seed(138)
  rf <- randomForest::randomForest(
    y ~ x1 + x2 + x3 + x4,
    data = data,
    importance = TRUE,
    ntree = 10
  )

  result <- var_imp(rf, complete = TRUE)

  # Should have all 4 predictors
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("x1", "x2", "x3", "x4"))

  # All estimates should be numeric
  expect_true(all(is.numeric(result$estimate)))
})

test_that("var_imp.randomForest() with complete=FALSE returns only used predictors", {
  skip_if_not_installed("randomForest")

  set.seed(593)
  data <- get_regression_data(n = 200)

  rf <- randomForest::randomForest(
    y ~ x1 + x2 + x3,
    data = data,
    importance = TRUE,
    ntree = 10
  )

  result <- var_imp(rf, complete = FALSE)

  # Should only have predictors with importance scores
  expected_vars <- rownames(rf$importance)
  expect_equal(nrow(result), length(expected_vars))
  expect_setequal(result$term, expected_vars)
})

test_that("var_imp.randomForest() works with numeric predictors only", {
  skip_if_not_installed("randomForest")

  mtcars <- get_mtcars_data()
  set.seed(761)
  rf <- randomForest::randomForest(
    mpg ~ cyl + disp + hp + wt,
    data = mtcars,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("cyl", "disp", "hp", "wt"))
})

test_that("var_imp.randomForest() works with factor predictors", {
  skip_if_not_installed("randomForest")

  wa_trees <- get_wa_trees_data()
  set.seed(384)
  rf <- randomForest::randomForest(
    county ~ class + elevation + roughness,
    data = wa_trees,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("class", "elevation", "roughness"))
})

test_that("var_imp.randomForest() works with mixed numeric and factor predictors", {
  skip_if_not_installed("randomForest")

  wa_trees <- get_wa_trees_data()
  set.seed(917)
  rf <- randomForest::randomForest(
    elevation ~ class + county + roughness + dew_temp,
    data = wa_trees,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("class", "county", "roughness", "dew_temp"))
})

test_that("var_imp.randomForest() handles many predictors", {
  skip_if_not_installed("randomForest")

  set.seed(258)
  n <- 200
  p <- 15
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", 1:p)
  X$y <- X$x1 + X$x2 + rnorm(n)

  set.seed(476)
  rf <- randomForest::randomForest(
    y ~ .,
    data = X,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  expect_equal(nrow(result), p)
  expect_setequal(result$term, paste0("x", 1:p))
})

test_that("var_imp.randomForest() works with classification forest", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(605)
  rf <- randomForest::randomForest(
    species ~ .,
    data = penguins,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("var_imp.randomForest() works with regression forest", {
  skip_if_not_installed("randomForest")

  mtcars <- get_mtcars_data()
  set.seed(441)
  rf <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("cyl", "disp", "hp"))
})

test_that("var_imp.randomForest() handles forest with constrained splits", {
  skip_if_not_installed("randomForest")

  # Force a very constrained forest
  set.seed(773)
  small_data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  set.seed(651)
  rf <- randomForest::randomForest(
    y ~ x1 + x2,
    data = small_data,
    importance = TRUE,
    ntree = 5,
    nodesize = 30
  )

  result <- var_imp(rf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x2"))
})

test_that("var_imp.randomForest() handles forest with very deep trees", {
  skip_if_not_installed("randomForest")

  set.seed(149)
  data <- get_regression_data(n = 200)

  rf <- randomForest::randomForest(
    y ~ x1 + x2 + x3,
    data = data,
    importance = TRUE,
    ntree = 10,
    maxnodes = 50
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
})

test_that("var_imp.randomForest() type parameter is case-insensitive", {
  skip_if_not_installed("randomForest")

  mtcars <- get_mtcars_data()
  set.seed(867)
  rf <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    importance = TRUE,
    ntree = 10
  )

  result1 <- var_imp(rf, type = "MSE")
  result2 <- var_imp(rf, type = "mse")
  result3 <- var_imp(rf, type = "Mse")

  expect_equal(result1$estimate, result2$estimate)
  expect_equal(result1$estimate, result3$estimate)
})

test_that("var_imp.randomForest() handles binary classification", {
  skip_if_not_installed("randomForest")

  set.seed(512)
  data <- get_binary_data(n = 200)

  rf <- randomForest::randomForest(
    y ~ x1 + x2 + x3,
    data = data,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("x1", "x2", "x3"))
})

test_that("var_imp.randomForest() handles multiclass classification", {
  skip_if_not_installed("randomForest")

  set.seed(296)
  data <- get_factor_data(n = 200)

  rf <- randomForest::randomForest(
    y ~ x1 + x3,
    data = data,
    importance = TRUE,
    ntree = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x3"))
})
