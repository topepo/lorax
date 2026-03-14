library(aorsf)

# Test data setup
penguins <- palmerpenguins::penguins[complete.cases(palmerpenguins::penguins), ]
forest <- orsf(species ~ ., data = penguins, n_tree = 10)

test_that("extract_rules.ObliqueForest() returns correct structure", {
  rules <- extract_rules(forest)

  expect_s3_class(rules, "rule_set_ObliqueForest")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("tree", "rules", "id"))
  expect_type(rules$tree, "integer")
  expect_type(rules$rules, "list")
  expect_type(rules$id, "integer")
})

test_that("extract_rules.ObliqueForest() validates tree argument", {
  expect_snapshot(extract_rules(forest, tree = "1"), error = TRUE)
  expect_snapshot(extract_rules(forest, tree = c(1, 2)), error = TRUE)
  expect_snapshot(extract_rules(forest, tree = 1.5), error = TRUE)
  expect_snapshot(extract_rules(forest, tree = 0), error = TRUE)
  expect_snapshot(extract_rules(forest, tree = 11), error = TRUE)
})

test_that("extract_rules.ObliqueForest() extracts all terminal nodes", {
  rules <- extract_rules(forest, tree = 1)

  # Get terminal nodes from forest structure (0-indexed internally)
  # Convert to 1-indexed to match user-facing IDs
  terminal_nodes <- which(forest$forest$child_left[[1]] == 0)
  expect_equal(sort(rules$id), sort(terminal_nodes))
})

test_that("extract_rules.ObliqueForest() produces valid R expressions", {
  rules <- extract_rules(forest, tree = 1)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.ObliqueForest() rules evaluate correctly with numeric data", {
  # Use regression forest with numeric predictors only
  # (avoids factor one-hot encoding issues)
  penguins_numeric <- penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]
  reg_forest <- orsf(body_mass_g ~ ., data = penguins_numeric, n_tree = 5)
  rules <- extract_rules(reg_forest, tree = 1)

  # Check that all rules are valid expressions that evaluate to logical vectors
  for (i in seq_len(nrow(rules))) {
    rule_expr <- rules$rules[[i]]
    result <- eval(rule_expr, penguins_numeric)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(penguins_numeric))
  }

  # At least some rules should match some rows
  # (not all terminal nodes need to match training data)
  total_matches <- sum(vapply(rules$rules, function(r) {
    sum(eval(r, penguins_numeric), na.rm = TRUE)
  }, numeric(1)))
  expect_true(total_matches > 0)
})

test_that("extract_rules.ObliqueForest() works with different trees", {
  rules1 <- extract_rules(forest, tree = 1)
  rules5 <- extract_rules(forest, tree = 5)

  expect_s3_class(rules1, "rule_set_ObliqueForest")
  expect_s3_class(rules5, "rule_set_ObliqueForest")
  expect_equal(unique(rules1$tree), 1L)
  expect_equal(unique(rules5$tree), 5L)
})

test_that("extract_rules.ObliqueForest() integrates with rule_text()", {
  rules <- extract_rules(forest, tree = 1)

  for (i in seq_len(nrow(rules))) {
    text <- rule_text(rules$rules[[i]])
    expect_type(text, "character")
    expect_true(nchar(text) > 0)

    text_bullets <- rule_text(rules$rules[[i]], bullets = TRUE)
    expect_type(text_bullets, "character")
    expect_true(nchar(text_bullets) > 0)
  }
})

test_that("extract_rules.ObliqueForest() rules contain oblique splits", {
  rules <- extract_rules(forest, tree = 1)

  # Check that at least some rules contain arithmetic operators
  # indicating linear combinations (oblique splits)
  has_arithmetic <- vapply(
    rules$rules,
    function(rule) {
      rule_text <- deparse1(rule)
      grepl("\\*|\\+|\\-", rule_text)
    },
    logical(1)
  )

  expect_true(any(has_arithmetic))
})

test_that("extract_rules.ObliqueForest() works with regression forest", {
  penguins_numeric <- penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]
  reg_forest <- orsf(body_mass_g ~ ., data = penguins_numeric, n_tree = 5)
  rules <- extract_rules(reg_forest, tree = 1)

  expect_s3_class(rules, "rule_set_ObliqueForest")
  expect_true(nrow(rules) > 0)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.ObliqueForest() sorts results by id", {
  rules <- extract_rules(forest, tree = 1)

  expect_true(all(diff(rules$id) > 0))
})
