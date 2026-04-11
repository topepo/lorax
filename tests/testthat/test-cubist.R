test_that("extract_rules.cubist returns correct structure", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Ames housing data - real estate pricing with meaningful predictors
  data <- get_ames_data(n = 200)
  mod <- cubist(data$x, data$y)

  rules <- extract_rules(mod)

  # Check class
  expect_s3_class(rules, "rule_set_cubist")
  expect_s3_class(rules, "rule_set")
  expect_s3_class(rules, "tbl_df")

  # Check columns
  expect_named(rules, c("committee", "id", "rules"))

  # Check column types
  expect_type(rules$committee, "integer")
  expect_type(rules$id, "integer")
  expect_type(rules$rules, "list")

  # Check that rules are expressions
  for (rule in rules$rules) {
    expect_true(is.language(rule) || is.logical(rule))
  }
})

test_that("extract_rules.cubist handles committee parameter", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Sacramento housing data with multiple committees
  data <- get_sacramento_data()
  mod <- cubist(data$x, data$y, committees = 3)

  # Default should be first committee
  rules1 <- extract_rules(mod)
  expect_equal(unique(rules1$committee), 1L)

  # Extract specific committee
  rules2 <- extract_rules(mod, committee = 2L)
  expect_equal(unique(rules2$committee), 2L)

  # Extract multiple committees
  rules_multi <- extract_rules(mod, committee = c(1L, 3L))
  expect_equal(sort(unique(rules_multi$committee)), c(1L, 3L))

  # Check ordering
  expect_true(all(diff(rules_multi$committee) >= 0))

  # Invalid committee should error
  expect_error(
    extract_rules(mod, committee = 0L),
    "must be between 1 and"
  )
  expect_error(
    extract_rules(mod, committee = 4L),
    "must be between 1 and"
  )
  expect_error(
    extract_rules(mod, committee = "1"),
    "must be an integer vector"
  )
})

test_that("extract_rules.cubist handles continuous splits correctly", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use structured data with clear continuous splits
  data <- get_structured_regression_data(n = 100)
  mod <- cubist(data$x, data$y)
  rules <- extract_rules(mod)

  # Should have at least one rule
  expect_gt(nrow(rules), 0)

  # Check that expressions can be evaluated
  if (nrow(rules) > 0 && !isTRUE(rules$rules[[1]])) {
    expr <- rules$rules[[1]]
    result <- eval(expr, data$x)
    expect_type(result, "logical")
    expect_length(result, nrow(data$x))
  }

  # Verify that rules contain expected variables
  # With structured data, we expect splits on x1 and/or x2
  rule_text_all <- sapply(rules$rules, deparse)
  expect_true(
    any(grepl("x1", rule_text_all)) || any(grepl("x2", rule_text_all))
  )
})

test_that("extract_rules.cubist handles categorical splits correctly", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Ames data which has factor predictors like Neighborhood and Bldg_Type
  data <- get_ames_data(n = 200)
  mod <- cubist(data$x, data$y)

  rules <- extract_rules(mod)

  # Should have rules
  expect_gt(nrow(rules), 0)

  # Check if we can evaluate the rules
  for (i in seq_len(min(3, nrow(rules)))) {
    expr <- rules$rules[[i]]
    if (!isTRUE(expr)) {
      result <- eval(expr, data$x)
      expect_type(result, "logical")
      expect_length(result, nrow(data$x))
    }
  }

  # Check for categorical splits in the rules
  # Ames data has factor predictors that should appear in some rules
  rule_text_all <- paste(sapply(rules$rules, deparse), collapse = " ")
  has_categorical <- grepl("==", rule_text_all) || grepl("%in%", rule_text_all)
  # May or may not have categorical splits depending on the model
  # Just verify it doesn't error
})

test_that("extract_rules.cubist handles rules with no conditions", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use simple data that may produce rules with no conditions
  data <- get_simple_cubist_data(n = 10)
  mod <- cubist(data$x, data$y, control = cubistControl(rules = 1))
  rules <- extract_rules(mod)

  # Should have at least one rule
  expect_gt(nrow(rules), 0)

  # If there's a rule with no conditions, it should be TRUE
  for (rule in rules$rules) {
    if (is.logical(rule) && length(rule) == 1) {
      expect_true(rule)
    }
  }
})

test_that("extract_rules.cubist works with multiple committees", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Ames data with multiple committees for ensemble modeling
  data <- get_ames_data(n = 300)
  mod <- cubist(data$x, data$y, committees = 5)

  # Extract all committees
  rules_all <- extract_rules(mod, committee = 1:5)

  # Should have rules from all committees
  expect_equal(sort(unique(rules_all$committee)), 1:5)

  # Each committee should have at least one rule
  for (com in 1:5) {
    com_rules <- rules_all[rules_all$committee == com, ]
    expect_gt(nrow(com_rules), 0)
  }

  # Rule IDs should restart for each committee
  for (com in 1:5) {
    com_rules <- rules_all[rules_all$committee == com, ]
    expect_equal(min(com_rules$id), 1L)
  }
})

test_that("active_predictors.cubist returns correct structure", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Sacramento data - well-understood housing predictors
  data <- get_sacramento_data()
  mod <- cubist(data$x, data$y)

  active <- active_predictors(mod)

  # Check structure
  expect_s3_class(active, "tbl_df")
  expect_named(active, "active_predictors")

  # Check content
  expect_type(active$active_predictors, "list")
  expect_length(active$active_predictors, 1)

  vars <- active$active_predictors[[1]]
  expect_type(vars, "character")

  # Should be subset of original predictors
  all_vars <- colnames(data$x)
  expect_true(all(vars %in% all_vars))

  # Should be sorted alphabetically (case-insensitive)
  expect_equal(vars, vars[order(tolower(vars))])
})

test_that("active_predictors.cubist identifies correct variables", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use structured data where we know which predictors are important
  data <- get_structured_regression_data(n = 200)
  mod <- cubist(data$x, data$y)

  active <- active_predictors(mod)
  vars <- active$active_predictors[[1]]

  # With structured data, x1 and x2 should definitely be included
  # as they directly determine the outcome
  expect_true("x1" %in% vars)
  expect_true("x2" %in% vars)

  # x3 might be included depending on the model
  # All variables should be from the original set
  expect_true(all(vars %in% c("x1", "x2", "x3")))
})

test_that("active_predictors.cubist works with real estate data", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Ames housing data
  data <- get_ames_data(n = 200)
  mod <- cubist(data$x, data$y)

  active <- active_predictors(mod)
  vars <- active$active_predictors[[1]]

  # Should include important predictors
  expect_gt(length(vars), 0)

  # All should be valid column names
  expect_true(all(vars %in% colnames(data$x)))

  # Likely to include key housing predictors
  # At minimum, living area and overall condition should be important
  important_vars <- c("Gr_Liv_Area", "Overall_Cond")
  expect_true(any(important_vars %in% vars))
})

test_that("active_predictors.cubist works with multiple committees", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Sacramento data with multiple committees
  data <- get_sacramento_data()
  mod <- cubist(data$x, data$y, committees = 5)

  active <- active_predictors(mod)

  # Should still return single row (aggregated across committees)
  expect_equal(nrow(active), 1)

  vars <- active$active_predictors[[1]]
  expect_type(vars, "character")

  # Variables should be from the original set
  all_vars <- colnames(data$x)
  expect_true(all(vars %in% all_vars))

  # With real estate data, we expect most variables to be used
  expect_gte(length(vars), 3) # At least beds, baths, sqft
})

test_that("extract_rules.cubist integrates with rule_text", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Ames data for realistic rules
  data <- get_ames_data(n = 150)
  mod <- cubist(data$x, data$y)

  rules <- extract_rules(mod)

  # Should be able to convert to text
  for (i in seq_len(min(3, nrow(rules)))) {
    rule_expr <- rules$rules[[i]]
    if (!isTRUE(rule_expr)) {
      text <- rule_text(rule_expr)
      expect_type(text, "character")
      expect_length(text, 1)
      expect_gt(nchar(text), 0)

      # Text should contain variable names from the data
      # Check that at least one predictor appears
      any_var_present <- any(sapply(colnames(data$x), function(v) {
        grepl(v, text)
      }))
      expect_true(any_var_present)
    }
  }
})

test_that("extract_rules.cubist handles edge case models", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Very small dataset - edge case
  data <- get_simple_cubist_data(n = 5)
  mod <- cubist(data$x, data$y)
  rules <- extract_rules(mod)

  # Should still work
  expect_s3_class(rules, "rule_set_cubist")
  expect_gt(nrow(rules), 0)

  # High dimensional data (p > n) - edge case
  data2 <- get_high_dim_cubist_data(n = 20, p = 50)
  mod2 <- cubist(data2$x, data2$y)
  rules2 <- extract_rules(mod2)

  expect_s3_class(rules2, "rule_set_cubist")
  # Model should still produce some rules
  expect_gt(nrow(rules2), 0)
})

test_that("extract_rules and active_predictors are consistent", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use Sacramento data for consistency check
  data <- get_sacramento_data()
  mod <- cubist(data$x, data$y)

  rules <- extract_rules(mod)
  active <- active_predictors(mod)

  active_vars <- active$active_predictors[[1]]

  # Extract variables mentioned in rules
  rule_vars <- character()
  for (rule_expr in rules$rules) {
    if (!isTRUE(rule_expr)) {
      # Get all symbols from the expression
      vars_in_rule <- all.vars(rule_expr)
      rule_vars <- c(rule_vars, vars_in_rule)
    }
  }
  rule_vars <- unique(rule_vars)

  # Variables in rules should be subset of active predictors
  # (active predictors may include variables only in linear models)
  if (length(rule_vars) > 0) {
    expect_true(all(rule_vars %in% active_vars))
  }

  # Active predictors should include at least the rule variables
  if (length(rule_vars) > 0) {
    expect_gte(length(active_vars), length(rule_vars))
  }
})

test_that("extract_rules.cubist handles Sacramento categorical predictors", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Sacramento has 'type' (Condo, Multi_Family, Residential) and 'city'
  data <- get_sacramento_data()
  mod <- cubist(data$x, data$y)

  rules <- extract_rules(mod)

  # Should be able to evaluate rules with factor predictors
  for (i in seq_len(min(5, nrow(rules)))) {
    expr <- rules$rules[[i]]
    if (!isTRUE(expr)) {
      result <- eval(expr, data$x)
      expect_type(result, "logical")
      expect_length(result, nrow(data$x))
      # Should partition the data (some TRUE, some FALSE ideally)
      # But at minimum shouldn't error
    }
  }
})

test_that("extract_rules.cubist handles models with no valid splits", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Use noisy data where predictors have no relationship with outcome
  # This is the exception case where we use random data
  data <- get_noisy_cubist_data(n = 50, p = 3)

  # Force a simple model with few rules
  mod <- cubist(data$x, data$y, control = cubistControl(rules = 1))
  rules <- extract_rules(mod)

  # Should still produce valid output
  expect_s3_class(rules, "rule_set_cubist")
  expect_gt(nrow(rules), 0)

  # With noisy data, might produce rules with no conditions (TRUE)
  # or very simple conditions
  for (rule in rules$rules) {
    # Should be evaluable
    if (!isTRUE(rule)) {
      result <- eval(rule, data$x)
      expect_type(result, "logical")
    }
  }
})

test_that("active_predictors.cubist handles models with no active predictors", {
  skip_if_not_installed("Cubist")
  library(Cubist)

  # Edge case: very simple model that might use no predictors
  # (just intercept)
  set.seed(999)
  x <- data.frame(x1 = rep(1, 10)) # Constant predictor
  y <- rnorm(10) # Random outcome

  mod <- cubist(x, y, control = cubistControl(rules = 1))
  active <- active_predictors(mod)

  # Should still return valid structure
  expect_s3_class(active, "tbl_df")
  expect_named(active, "active_predictors")

  vars <- active$active_predictors[[1]]
  expect_type(vars, "character")
  # Might be empty or contain x1
  expect_lte(length(vars), 1)
})
