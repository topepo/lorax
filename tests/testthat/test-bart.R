library(dbarts)

# Test data setup
set.seed(123)
n <- 200
data <- data.frame(
  x1 = runif(n),
  x2 = runif(n),
  x3 = runif(n)
)
data$y <- 2 * data$x1 + 3 * data$x2 - data$x3 + rnorm(n, sd = 0.1)

bart_fit <- bart(
  y ~ .,
  data,
  keeptrees = TRUE,
  verbose = FALSE,
  ntree = 10,
  ndpost = 1,
  nskip = 100
)

test_that("extract_rules.bart() returns correct structure", {
  rules <- extract_rules(bart_fit, tree = 1)

  expect_s3_class(rules, "rule_set_bart")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")
  expect_named(rules, c("tree", "rules", "id"))
  expect_type(rules$tree, "integer")
  expect_type(rules$rules, "list")
  expect_type(rules$id, "integer")
})

test_that("extract_rules.bart() validates tree parameter", {
  expect_snapshot(extract_rules(bart_fit, tree = 1.5), error = TRUE)
  expect_snapshot(extract_rules(bart_fit, tree = c(1, 2)), error = TRUE)
  expect_snapshot(extract_rules(bart_fit, tree = 100), error = TRUE)
  expect_snapshot(extract_rules(bart_fit, tree = 0), error = TRUE)
})

test_that("extract_rules.bart() validates chain parameter", {
  expect_snapshot(extract_rules(bart_fit, chain = 1.5), error = TRUE)
  expect_snapshot(extract_rules(bart_fit, chain = c(1, 2)), error = TRUE)
  expect_snapshot(extract_rules(bart_fit, chain = 100), error = TRUE)
  expect_snapshot(extract_rules(bart_fit, chain = 0), error = TRUE)
})

test_that("extract_rules.bart() extracts all terminal nodes", {
  rules <- extract_rules(bart_fit, tree = 1)

  # Get terminals directly from bart fit object
  trees_df <- bart_fit$fit$getTrees()
  if ("chain" %in% names(trees_df)) {
    tree_df <- trees_df[trees_df$tree == 1 & trees_df$chain == 1, ]
  } else {
    tree_df <- trees_df[trees_df$tree == 1, ]
  }
  if ("sample" %in% names(tree_df) && nrow(tree_df) > 0) {
    tree_df <- tree_df[tree_df$sample == max(tree_df$sample), ]
  }
  expected_terminals <- sum(tree_df$var == -1)

  expect_equal(nrow(rules), expected_terminals)
})

test_that("extract_rules.bart() produces valid R expressions", {
  rules <- extract_rules(bart_fit, tree = 1)

  for (i in seq_len(nrow(rules))) {
    expect_true(is.language(rules$rules[[i]]))
  }
})

test_that("extract_rules.bart() rules evaluate correctly", {
  rules <- extract_rules(bart_fit, tree = 1)

  for (i in seq_len(nrow(rules))) {
    rule_expr <- rules$rules[[i]]
    result <- eval(rule_expr, data)

    expect_type(result, "logical")
    expect_equal(length(result), nrow(data))
  }

  # At least one rule should match some observations
  total_matches <- sum(sapply(rules$rules, function(r) sum(eval(r, data))))
  expect_true(total_matches > 0)
})

test_that("extract_rules.bart() rules are sorted by id", {
  rules <- extract_rules(bart_fit, tree = 1)
  expect_true(all(diff(rules$id) > 0))
})

test_that("extract_rules.bart() works with different trees", {
  rules1 <- extract_rules(bart_fit, tree = 1)
  rules2 <- extract_rules(bart_fit, tree = 2)

  expect_s3_class(rules1, "rule_set_bart")
  expect_s3_class(rules2, "rule_set_bart")

  # Trees should generally be different
  expect_false(identical(rules1$rules, rules2$rules))
})

test_that("extract_rules.bart() integrates with rule_text()", {
  rules <- extract_rules(bart_fit, tree = 1)

  for (i in seq_len(nrow(rules))) {
    text <- rule_text(rules$rules[[i]])
    expect_type(text, "character")
    expect_true(nchar(text) > 0)

    text_bullets <- rule_text(rules$rules[[i]], bullets = TRUE)
    expect_type(text_bullets, "character")
    expect_true(nchar(text_bullets) > 0)
  }
})

test_that("extract_rules.bart() tree column matches input", {
  rules1 <- extract_rules(bart_fit, tree = 1)
  rules2 <- extract_rules(bart_fit, tree = 2)

  expect_true(all(rules1$tree == 1))
  expect_true(all(rules2$tree == 2))
})

test_that("extract_rules.bart() requires keeptrees = TRUE", {
  bart_fit_no_trees <- bart(
    y ~ .,
    data,
    keeptrees = FALSE,
    verbose = FALSE,
    ntree = 5,
    ndpost = 1,
    nskip = 50
  )

  expect_snapshot(extract_rules(bart_fit_no_trees, tree = 1), error = TRUE)
})
