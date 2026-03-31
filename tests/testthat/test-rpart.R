test_that("extract_rules.rpart() returns correct structure", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
})

test_that("extract_rules.rpart() extracts all terminal nodes", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)
  terminal_nodes <- as.integer(rownames(tree$frame[
    tree$frame$var == "<leaf>",
  ]))
  expect_equal(sort(rules$id), sort(terminal_nodes))
})

test_that("extract_rules.rpart() produces valid R expressions", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.rpart() rules evaluate correctly", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    rule_expr <- rules$rules[[i]]
    result <- eval(rule_expr, iris)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(iris))
    expect_true(sum(result) > 0)
  }
})

test_that("extract_rules.rpart() works with numeric-only splits", {
  skip_if_not_installed("rpart")

  num_tree <- rpart::rpart(
    Petal.Length ~ Sepal.Length + Sepal.Width,
    data = iris
  )
  rules <- extract_rules(num_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_true(nrow(rules) > 0)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.rpart() works with categorical splits", {
  skip_if_not_installed("rpart")

  cat_tree <- rpart::rpart(Species ~ ., data = iris)
  rules <- extract_rules(cat_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.rpart() works with single-node tree", {
  skip_if_not_installed("rpart")

  # Create a tree with no splits
  single_tree <- rpart::rpart(Sepal.Length ~ Sepal.Width, data = iris, cp = 1)
  rules <- extract_rules(single_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

test_that("extract_rules.rpart() integrates with rule_text()", {
  skip_if_not_installed("rpart")

  tree <- get_iris_rpart_tree()
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    text <- rule_text(rules$rules[[i]])
    expect_type(text, "character")
    expect_true(nchar(text) > 0)

    text_bullets <- rule_text(rules$rules[[i]], bullets = TRUE)
    expect_type(text_bullets, "character")
    expect_true(nchar(text_bullets) > 0)
  }
})

test_that("extract_rules.rpart() works with wa_trees data", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart::rpart(class ~ ., data = wa_trees)
  rules <- extract_rules(wa_tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_true(nrow(rules) > 0)

  # Check that rules are sorted by id
  expect_true(all(diff(rules$id) > 0))

  # Check that all rules evaluate and match expected counts
  for (i in seq_len(nrow(rules))) {
    result <- eval(rules$rules[[i]], wa_trees)
    expected_n <- wa_tree$frame[as.character(rules$id[i]), "n"]

    expect_type(result, "logical")
    expect_equal(length(result), nrow(wa_trees))
    expect_equal(sum(result), expected_n)
  }
})

test_that("extract_rules.rpart() handles no-split data", {
  skip_if_not_installed("rpart")

  # Data where no good split can be made
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  tree <- rpart::rpart(y ~ ., data = null_data)

  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_rpart")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

# Tests for active_predictors.rpart() -----------------------------------------

test_that("active_predictors.rpart() returns correct structure", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart::rpart(class ~ ., data = wa_trees)
  result <- active_predictors(wa_tree)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "active_predictors")
  expect_type(result$active_predictors, "list")
  expect_length(result$active_predictors, 1)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.rpart() extracts correct variables", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart::rpart(class ~ ., data = wa_trees)
  result <- active_predictors(wa_tree)

  # Expected: unique variables from frame excluding "<leaf>"
  expected <- unique(wa_tree$frame$var[wa_tree$frame$var != "<leaf>"])

  expect_equal(sort(result$active_predictors[[1]]), sort(expected))
})

test_that("active_predictors.rpart() excludes competing and surrogate splits", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- rpart::rpart(
    class ~ elevation + eastness + northness + roughness + county,
    data = wa_trees
  )
  result <- active_predictors(wa_tree)
  active_vars <- result$active_predictors[[1]]

  # Only variables used in actual splits should be included
  # Not those in competing or surrogate splits
  all_frame_vars <- unique(wa_tree$frame$var[wa_tree$frame$var != "<leaf>"])
  expect_equal(sort(active_vars), sort(all_frame_vars))
})

test_that("active_predictors.rpart() handles single-node tree", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  single_tree <- rpart::rpart(class ~ elevation, data = wa_trees, cp = 1)
  result <- active_predictors(single_tree)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_type(result$active_predictors[[1]], "character")
  expect_length(result$active_predictors[[1]], 0)
})

test_that("active_predictors.rpart() handles tree with repeated variables", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  deep_tree <- rpart::rpart(class ~ ., data = wa_trees, cp = 0.01)
  result <- active_predictors(deep_tree)

  # Should return unique variables only (no duplicates)
  expect_equal(
    length(result$active_predictors[[1]]),
    length(unique(result$active_predictors[[1]]))
  )
})

test_that("active_predictors.rpart() works with numeric predictors", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  num_tree <- rpart::rpart(
    elevation ~ year + roughness + dew_temp,
    data = wa_trees
  )
  result <- active_predictors(num_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  # All predictors should be from the model formula
  expect_true(all(
    result$active_predictors[[1]] %in%
      c("year", "roughness", "dew_temp")
  ))
})

test_that("active_predictors.rpart() works with factor predictors", {
  skip_if_not_installed("rpart")

  load(system.file(package = "lorax", "wa_trees.RData"))
  factor_tree <- rpart::rpart(
    county ~ class + elevation + roughness,
    data = wa_trees
  )
  result <- active_predictors(factor_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  expect_true(length(result$active_predictors[[1]]) >= 0)
})

# Tests for var_imp.rpart() -------------------------------------------------

test_that("var_imp.rpart() returns correct structure", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  tree <- rpart::rpart(
    class ~ elevation + roughness + dew_temp,
    data = wa_trees
  )
  result <- var_imp(tree)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))
  expect_type(result$term, "character")
  expect_type(result$estimate, "double")
})

test_that("var_imp.rpart() extracts variable importance scores", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  tree <- rpart::rpart(
    class ~ elevation + roughness + dew_temp,
    data = wa_trees
  )
  result <- var_imp(tree)

  # Should have at least one variable with non-zero importance
  expect_true(any(result$estimate > 0))

  # All estimates should be non-negative
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.rpart() with complete=TRUE fills missing predictors", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  # Build a tree that won't use all predictors
  tree <- rpart::rpart(
    class ~ elevation + roughness + dew_temp + northness + eastness,
    data = wa_trees
  )
  result <- var_imp(tree, complete = TRUE)

  # Should have all 5 predictors
  expect_equal(nrow(result), 5)
  expect_setequal(
    result$term,
    c("elevation", "roughness", "dew_temp", "northness", "eastness")
  )

  # Unused predictors should have estimate = 0
  unused_preds <- setdiff(
    c("elevation", "roughness", "dew_temp", "northness", "eastness"),
    names(tree$variable.importance)
  )
  if (length(unused_preds) > 0) {
    unused_estimates <- result$estimate[result$term %in% unused_preds]
    expect_true(all(unused_estimates == 0))
  }
})

test_that("var_imp.rpart() with complete=FALSE returns only used predictors", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  tree <- rpart::rpart(
    class ~ elevation + roughness + dew_temp + northness + eastness,
    data = wa_trees
  )
  result <- var_imp(tree, complete = FALSE)

  # Should only have predictors that were actually used
  expected_vars <- names(tree$variable.importance)
  expect_equal(nrow(result), length(expected_vars))
  expect_setequal(result$term, expected_vars)

  # All should have non-zero importance
  expect_true(all(result$estimate > 0))
})

test_that("var_imp.rpart() works with numeric predictors only", {
  skip_if_not_installed("rpart")

  mtcars <- get_mtcars_data()
  tree <- rpart::rpart(mpg ~ cyl + disp + hp + wt, data = mtcars)
  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("cyl", "disp", "hp", "wt"))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.rpart() works with factor predictors", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  tree <- rpart::rpart(county ~ class + elevation + roughness, data = wa_trees)
  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_setequal(result$term, c("class", "elevation", "roughness"))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.rpart() works with mixed numeric and factor predictors", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  tree <- rpart::rpart(
    elevation ~ class + county + roughness + dew_temp,
    data = wa_trees
  )
  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_setequal(result$term, c("class", "county", "roughness", "dew_temp"))
  expect_true(all(result$estimate >= 0))
})

test_that("var_imp.rpart() handles tree with no valid splits", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  # Force a tree with no splits using high cp
  tree <- rpart::rpart(class ~ elevation + roughness, data = wa_trees, cp = 1)
  result <- var_imp(tree, complete = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))

  # Should have all predictors with zero importance
  expect_equal(nrow(result), 2)
  expect_setequal(result$term, c("elevation", "roughness"))
  expect_true(all(result$estimate == 0))
})

test_that("var_imp.rpart() handles tree with no splits and complete=FALSE", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  tree <- rpart::rpart(class ~ elevation + roughness, data = wa_trees, cp = 1)
  result <- var_imp(tree, complete = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate"))

  # Should have zero rows since no variables have importance
  expect_equal(nrow(result), 0)
})

test_that("var_imp.rpart() importance scores match underlying object", {
  skip_if_not_installed("rpart")

  wa_trees <- get_wa_trees_data()
  tree <- rpart::rpart(
    class ~ elevation + roughness + dew_temp,
    data = wa_trees
  )
  result <- var_imp(tree, complete = FALSE)

  # Match against tree$variable.importance
  expected <- tree$variable.importance
  for (i in seq_len(nrow(result))) {
    term <- result$term[i]
    estimate <- result$estimate[i]
    expect_equal(estimate, expected[[term]], tolerance = 1e-10)
  }
})
