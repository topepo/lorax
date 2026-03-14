library(partykit)

# Test data setup
penguins <- palmerpenguins::penguins
tree <- ctree(species ~ ., data = penguins)

test_that("extract_rules.party() returns correct structure", {
  rules <- extract_rules(tree)

  expect_s3_class(rules, "rule_set_party")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("id", "rules"))
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")
})

test_that("extract_rules.party() extracts all terminal nodes", {
  rules <- extract_rules(tree)
  terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
  expect_equal(sort(rules$id), sort(terminal_nodes))
})

test_that("extract_rules.party() produces valid R expressions", {
  rules <- extract_rules(tree)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.party() rules evaluate correctly", {
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
  num_tree <- ctree(
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
  cat_tree <- ctree(species ~ ., data = penguins)
  rules <- extract_rules(cat_tree)

  expect_s3_class(rules, "rule_set_party")
  expect_true(nrow(rules) > 0)
})

test_that("extract_rules.party() works with single-node tree", {
  # Create a tree with no splits
  single_tree <- ctree(
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
  load(system.file(package = "lorax", "wa_trees.RData"))
  wa_tree <- ctree(class ~ ., data = wa_trees)
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
