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

  data <- get_binary_data()

  rf <- ranger::ranger(y ~ ., data = data, num.trees = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.ranger works with regression", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()

  rf <- ranger::ranger(y ~ ., data = data, num.trees = 5)
  p <- as.party(rf, tree = 1, data = data)

  expect_s3_class(p, "party")
  expect_s3_class(p$node, "partynode")
})

test_that("as.party.ranger works with factor predictors", {
  skip_if_not_installed("ranger")

  data <- get_factor_data()

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
  skip_if_not_installed("palmerpenguins")

  data("penguins", package = "palmerpenguins", envir = environment())
  penguins <- get("penguins", envir = environment())
  penguins <- na.omit(penguins)

  rf <- ranger::ranger(species ~ ., data = penguins, num.trees = 5)

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

# Tests for extract_rules.ranger() -------------------------------------------

test_that("extract_rules.ranger() returns correct structure", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(527)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = 1L, data = data)

  expect_s3_class(rules, "rule_set_party")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules", "tree"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
  expect_type(rules$tree, "integer")
})

test_that("extract_rules.ranger() extracts from single tree", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(638)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = 1L, data = data)

  expect_equal(unique(rules$tree), 1L)
  expect_true(nrow(rules) > 0)

  # Check that rules are valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.ranger() extracts from multiple trees", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(749)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = c(1L, 2L, 3L), data = data)

  expect_equal(sort(unique(rules$tree)), c(1L, 2L, 3L))
  expect_true(nrow(rules) > 0)

  # Check that each tree has rules
  for (tree_num in c(1L, 2L, 3L)) {
    tree_rules <- rules[rules$tree == tree_num, ]
    expect_true(nrow(tree_rules) > 0)
  }
})

test_that("extract_rules.ranger() validates tree argument", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(851)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3
  )

  expect_snapshot(extract_rules(rf, tree = "1", data = data), error = TRUE)
  expect_snapshot(extract_rules(rf, tree = 1.5, data = data), error = TRUE)
  expect_snapshot(extract_rules(rf, tree = 0L, data = data), error = TRUE)
  expect_snapshot(extract_rules(rf, tree = 10L, data = data), error = TRUE)
})

test_that("extract_rules.ranger() requires data parameter", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(962)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3
  )

  expect_snapshot(extract_rules(rf, tree = 1L), error = TRUE)
  expect_snapshot(extract_rules(rf, tree = 1L, data = NULL), error = TRUE)
})

test_that("extract_rules.ranger() requires write.forest = TRUE", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(173)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3,
    write.forest = FALSE
  )

  expect_snapshot(extract_rules(rf, tree = 1L, data = data), error = TRUE)
})

test_that("extract_rules.ranger() works with numeric predictors", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(284)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02,
    data = data,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = 1L, data = data)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.ranger() works with factor predictors", {
  skip_if_not_installed("ranger")

  data <- get_factor_data()
  set.seed(395)
  rf <- ranger::ranger(y ~ island + sex, data = data, num.trees = 3)
  rules <- extract_rules(rf, tree = 1L, data = data)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.ranger() works with mixed predictors", {
  skip_if_not_installed("ranger")

  wa_trees <- get_wa_trees_data()[1:200, ]
  set.seed(416)
  rf <- ranger::ranger(
    class ~ elevation + county,
    data = wa_trees,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = 1L, data = wa_trees)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.ranger() works with single numeric predictor", {
  skip_if_not_installed("ranger")

  data <- get_single_numeric_data()
  set.seed(527)
  model <- ranger::ranger(y ~ x, data = data, num.trees = 5)
  rules <- extract_rules(model, tree = 1L, data = data)

  expect_s3_class(rules, "rule_set")
  expect_true(nrow(rules) > 0)

  # Check that rules are valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.ranger() works with single factor predictor", {
  skip_if_not_installed("ranger")

  data <- get_single_factor_data()
  set.seed(638)
  model <- ranger::ranger(y ~ x, data = data, num.trees = 5)
  rules <- extract_rules(model, tree = 1L, data = data)

  expect_s3_class(rules, "rule_set")
  expect_true(nrow(rules) > 0)

  # Check that rules are valid expressions
  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.ranger() rules are sorted by tree then id", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(527)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = c(2L, 1L, 3L), data = data)

  # Check sorting
  expect_true(all(diff(rules$tree) >= 0))

  # Within each tree, ids should be sorted
  for (tree_num in unique(rules$tree)) {
    tree_rules <- rules[rules$tree == tree_num, ]
    expect_true(all(diff(tree_rules$id) > 0))
  }
})

test_that("extract_rules.ranger() handles duplicate tree numbers", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(638)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3
  )
  rules <- extract_rules(rf, tree = c(1L, 1L, 2L), data = data)

  # Should have results for tree 1 twice
  tree_counts <- table(rules$tree)
  expect_equal(as.numeric(names(tree_counts)), c(1, 2))
})

test_that("extract_rules.ranger() works with all trees", {
  skip_if_not_installed("ranger")

  data <- get_regression_data()
  set.seed(749)
  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    num.trees = 3
  )
  n_trees <- 3
  rules <- extract_rules(rf, tree = 1:n_trees, data = data)

  expect_equal(sort(unique(rules$tree)), 1:n_trees)
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.ranger() handles tree with no valid splits", {
  skip_if_not_installed("ranger")

  # Create data where tree may have no splits
  null_data <- data.frame(
    y = 1:10,
    x1 = rep(1:5, each = 2),
    x2 = rep(1:5, each = 2)
  )
  set.seed(851)
  rf <- ranger::ranger(
    y ~ .,
    data = null_data,
    num.trees = 2,
    min.node.size = 5
  )
  rules <- extract_rules(rf, tree = 1L, data = null_data)

  expect_s3_class(rules, "rule_set_party")
  # Even with no splits, should return at least one rule (TRUE)
  expect_true(nrow(rules) >= 1)
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

  data <- get_regression_data()
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

test_that("active_predictors.ranger() works with single numeric predictor", {
  skip_if_not_installed("ranger")

  data <- get_single_numeric_data()
  set.seed(749)
  model <- ranger::ranger(y ~ x, data = data, num.trees = 5)
  active <- active_predictors(model)

  expect_s3_class(active, "tbl_df")
  expect_setequal(unique(unlist(active$active_predictors)), "x")
})

test_that("active_predictors.ranger() works with single factor predictor", {
  skip_if_not_installed("ranger")

  data <- get_single_factor_data()
  set.seed(851)
  model <- ranger::ranger(y ~ x, data = data, num.trees = 5)
  active <- active_predictors(model)

  expect_s3_class(active, "tbl_df")
  expect_setequal(unique(unlist(active$active_predictors)), "x")
})

# Tests for var_imp.ranger() -------------------------------------------------

test_that("var_imp.ranger() returns correct structure", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(847)
  rf <- ranger::ranger(
    species ~ .,
    data = penguins,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_type(result$term, "character")
  expect_type(result$estimate, "double")
})

test_that("var_imp.ranger() extracts variable importance scores", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(293)
  rf <- ranger::ranger(
    species ~ .,
    data = penguins,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf)

  # Should have at least one variable with non-zero importance
  expect_true(any(result$estimate > 0))

  # All estimates should be non-negative
  expect_true(all(result$estimate >= 0))

  # Should have all predictors from the model
  expect_true(nrow(result) > 0)
})

test_that("var_imp.ranger() with complete=TRUE fills missing predictors", {
  skip_if_not_installed("ranger")

  # Create a scenario where some predictors might have zero importance
  set.seed(614)
  data <- get_regression_data()
  # Add a near-constant predictor
  data$x4 <- rnorm(200, mean = 1000, sd = 0.001)

  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03 + predictor_04,
    data = data,
    importance = "impurity",
    num.trees = 10
  )

  result <- var_imp(rf, complete = TRUE)

  # Should have all 4 predictors
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("x1", "x2", "x3", "x4"))

  # All estimates should be non-negative
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.ranger() with complete=FALSE returns only used predictors", {
  skip_if_not_installed("ranger")

  set.seed(759)
  data <- get_regression_data()

  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    importance = "impurity",
    num.trees = 10
  )

  result <- var_imp(rf, complete = FALSE)

  # Should only have predictors with importance scores
  expected_vars <- names(rf$variable.importance)
  expect_equal(nrow(result), length(expected_vars))
  expect_setequal(result$term, expected_vars)
})

test_that("var_imp.ranger() works with numeric predictors only", {
  skip_if_not_installed("ranger")

  mtcars <- get_mtcars_data()
  set.seed(186)
  rf <- ranger::ranger(
    mpg ~ cyl + disp + hp + wt,
    data = mtcars,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("cyl", "disp", "hp", "wt"))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.ranger() works with factor predictors", {
  skip_if_not_installed("ranger")

  wa_trees <- get_wa_trees_data()
  set.seed(421)
  rf <- ranger::ranger(
    county ~ class + elevation + roughness,
    data = wa_trees,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("class", "elevation", "roughness"))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.ranger() works with mixed numeric and factor predictors", {
  skip_if_not_installed("ranger")

  wa_trees <- get_wa_trees_data()
  set.seed(537)
  rf <- ranger::ranger(
    elevation ~ class + county + roughness + dew_temp,
    data = wa_trees,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("class", "county", "roughness", "dew_temp"))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.ranger() importance scores match underlying object", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(905)
  rf <- ranger::ranger(
    species ~ .,
    data = penguins,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf, complete = FALSE)

  # Match against rf$variable.importance
  expected <- rf$variable.importance

  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expect_equal(estimate, expected[[term]], tolerance = 1e-10)
  }
})

test_that("var_imp.ranger() works with impurity importance", {
  skip_if_not_installed("ranger")

  mtcars <- get_mtcars_data()
  set.seed(672)
  rf <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("cyl", "disp", "hp"))
})

test_that("var_imp.ranger() works with permutation importance", {
  skip_if_not_installed("ranger")

  mtcars <- get_mtcars_data()
  set.seed(318)
  rf <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    importance = "permutation",
    num.trees = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("cyl", "disp", "hp"))
})

test_that("var_imp.ranger() errors when importance not calculated", {
  skip_if_not_installed("ranger")

  mtcars <- get_mtcars_data()
  set.seed(894)
  rf <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 10
  )

  expect_snapshot(var_imp(rf), error = TRUE)
})

test_that("var_imp.ranger() handles many predictors", {
  skip_if_not_installed("ranger")

  set.seed(753)
  n <- 200
  p <- 15
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", 1:p)
  X$y <- X$x1 + X$x2 + rnorm(n)

  rf <- ranger::ranger(
    y ~ .,
    data = X,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf)

  expect_equal(nrow(result), p)
  expect_setequal(result$term, paste0("x", 1:p))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.ranger() works with classification forest", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("palmerpenguins")

  penguins <- get_penguins_data()
  set.seed(481)
  rf <- ranger::ranger(
    species ~ .,
    data = penguins,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.ranger() works with regression forest", {
  skip_if_not_installed("ranger")

  mtcars <- get_mtcars_data()
  set.seed(629)
  rf <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    importance = "impurity",
    num.trees = 10
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("cyl", "disp", "hp"))
})

test_that("var_imp.ranger() handles forest with constrained splits", {
  skip_if_not_installed("ranger")

  # Force a very constrained forest
  set.seed(142)
  small_data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02,
    data = small_data,
    importance = "impurity",
    num.trees = 5,
    min.node.size = 30
  )

  result <- var_imp(rf, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("x1", "x2"))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.ranger() handles forest with very deep trees", {
  skip_if_not_installed("ranger")

  set.seed(568)
  data <- get_regression_data()

  rf <- ranger::ranger(
    y ~ predictor_01 + predictor_02 + predictor_03,
    data = data,
    importance = "impurity",
    num.trees = 10,
    max.depth = 20
  )
  result <- var_imp(rf)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.ranger() works with single numeric predictor", {
  skip_if_not_installed("ranger")

  data <- get_single_numeric_data()
  set.seed(962)
  model <- ranger::ranger(
    y ~ x,
    data = data,
    num.trees = 5,
    importance = "impurity"
  )
  importance <- var_imp(model)

  expect_s3_class(importance, "tbl_df")
  expect_equal(importance$term, "x")
  expect_true(importance$estimate >= 0)
})

test_that("var_imp.ranger() works with single factor predictor", {
  skip_if_not_installed("ranger")

  data <- get_single_factor_data()
  set.seed(173)
  model <- ranger::ranger(
    y ~ x,
    data = data,
    num.trees = 5,
    importance = "impurity"
  )
  importance <- var_imp(model)

  expect_s3_class(importance, "tbl_df")
  expect_equal(importance$term, "x")
  expect_true(importance$estimate >= 0)
})
