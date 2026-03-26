test_that("extract_rules.party() returns correct structure", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_party")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
})

test_that("extract_rules.party() extracts all terminal nodes", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  rules <- extract_rules(tree)
  terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
  expect_equal(sort(rules$id), sort(terminal_nodes))
})

test_that("extract_rules.party() produces valid R expressions", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.party() rules evaluate correctly", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
  penguins <- palmerpenguins::penguins
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    rule_expr <- rules$rules[[i]]
    result <- eval(rule_expr, penguins)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(penguins))
    expect_true(sum(result, na.rm = TRUE) > 0)
  }
})

test_that("extract_rules.party() works with numeric-only splits", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  penguins <- palmerpenguins::penguins
  num_tree <- partykit::ctree(
    body_mass_g ~ bill_length_mm + bill_depth_mm,
    data = penguins
  )
  rules <- extract_rules(num_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.party() works with categorical splits", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  penguins <- palmerpenguins::penguins
  cat_tree <- partykit::ctree(species ~ ., data = penguins)
  rules <- extract_rules(cat_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.party() works with single-node tree", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  # Create a tree with no splits
  penguins <- palmerpenguins::penguins
  single_tree <- partykit::ctree(
    body_mass_g ~ bill_length_mm,
    data = penguins,
    mincriterion = 1
  )
  rules <- extract_rules(single_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

test_that("extract_rules.party() integrates with rule_text()", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("palmerpenguins")

  tree <- get_penguins_tree()
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

test_that("extract_rules.party() works with wa_trees data", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- partykit::ctree(class ~ ., data = wa_trees)
  rules <- extract_rules(wa_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)

  # Check that rules are sorted by id
  expect_true(all(diff(rules$id) > 0))

  # Check that all rules evaluate correctly
  for (i in seq_len(nrow(rules))) {
    result <- eval(rules$rules[[i]], wa_trees)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(wa_trees))
    expect_true(sum(result) > 0)
  }
})

test_that("extract_rules.party() handles no-split data", {
  skip_if_not_installed("partykit")

  # Data where no good split can be made
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  tree <- partykit::ctree(y ~ ., data = null_data)

  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_party")
  expect_equal(nrow(rules), 1)
  expect_equal(rules$id, 1L)
  expect_equal(rules$rules[[1]], rlang::expr(TRUE))
})

# Tests for active_predictors.party() -----------------------------------------

test_that("active_predictors.party() returns correct structure", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- partykit::ctree(class ~ ., data = wa_trees)
  result <- active_predictors(wa_tree)

  expect_s3_class(result, "tbl_df")
  expect_named(result, "active_predictors")
  expect_type(result$active_predictors, "list")
  expect_length(result$active_predictors, 1)
  expect_type(result$active_predictors[[1]], "character")
})

test_that("active_predictors.party() extracts correct variables", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- partykit::ctree(
    class ~ elevation + eastness + county,
    data = wa_trees
  )
  result <- active_predictors(wa_tree)

  # Get expected variables from non-terminal nodes
  non_term <- partykit::nodeids(wa_tree, terminal = FALSE)
  expected_vars <- character()
  for (node_id in non_term) {
    node <- partykit::nodeapply(wa_tree, ids = node_id, by_node = TRUE)[[1]]
    sp <- partykit::split_node(node)
    if (!is.null(sp)) {
      var_idx <- partykit::varid_split(sp)
      expected_vars <- c(expected_vars, names(wa_tree$data)[var_idx])
    }
  }
  expected <- unique(expected_vars)

  expect_equal(sort(result$active_predictors[[1]]), sort(expected))
})

test_that("active_predictors.party() works with numeric predictors", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  num_tree <- partykit::ctree(
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

test_that("active_predictors.party() works with factor predictors", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  factor_tree <- partykit::ctree(
    county ~ class + elevation + roughness,
    data = wa_trees
  )
  result <- active_predictors(factor_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  expect_true(length(result$active_predictors[[1]]) >= 0)
  # All predictors should be from the model formula
  expect_true(all(
    result$active_predictors[[1]] %in%
      c("class", "elevation", "roughness")
  ))
})

test_that("active_predictors.party() works with mixed numeric and factor predictors", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  mixed_tree <- partykit::ctree(
    class ~ elevation + county + roughness,
    data = wa_trees
  )
  result <- active_predictors(mixed_tree)

  expect_s3_class(result, "tbl_df")
  expect_type(result$active_predictors[[1]], "character")
  # Should have at least one predictor
  expect_true(length(result$active_predictors[[1]]) >= 1)
  # All predictors should be from the model formula
  expect_true(all(
    result$active_predictors[[1]] %in%
      c("elevation", "county", "roughness")
  ))
})

test_that("active_predictors.party() handles single-node tree", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  single_tree <- partykit::ctree(
    class ~ elevation,
    data = wa_trees,
    mincriterion = 1
  )
  result <- active_predictors(single_tree)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_type(result$active_predictors[[1]], "character")
  expect_length(result$active_predictors[[1]], 0)
})

test_that("active_predictors.party() handles tree with repeated variables", {
  skip_if_not_installed("partykit")

  load(system.file(package = "lorax", "wa_trees.RData"))
  tree <- partykit::ctree(class ~ ., data = wa_trees)
  result <- active_predictors(tree)

  # Should return unique variables only (no duplicates)
  expect_equal(
    length(result$active_predictors[[1]]),
    length(unique(result$active_predictors[[1]]))
  )
})

test_that("active_predictors.party() handles no-split data", {
  skip_if_not_installed("partykit")

  # Data where no good split can be made
  null_data <- data.frame(y = 1:10, x = rep(1:5, each = 2))
  tree <- partykit::ctree(y ~ ., data = null_data)
  result <- active_predictors(tree)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_type(result$active_predictors[[1]], "character")
  expect_length(result$active_predictors[[1]], 0)
})
