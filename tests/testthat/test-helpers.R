test_that("combine_rule_elements() combines two expressions with AND", {
  expr1 <- rlang::expr(x > 5)
  expr2 <- rlang::expr(y < 10)
  result <- combine_rule_elements(list(expr1, expr2))
  expect_equal(deparse(result), "x > 5 & y < 10")
})

test_that("combine_rule_elements() combines three expressions with AND", {
  expr1 <- rlang::expr(x > 5)
  expr2 <- rlang::expr(y < 10)
  expr3 <- rlang::expr(z == 3)
  result <- combine_rule_elements(list(expr1, expr2, expr3))
  expect_equal(deparse(result), "x > 5 & y < 10 & z == 3")
})

test_that("combine_rule_elements() combines four expressions with AND", {
  expr1 <- rlang::expr(a > 1)
  expr2 <- rlang::expr(b < 2)
  expr3 <- rlang::expr(c == 3)
  expr4 <- rlang::expr(d >= 4)
  result <- combine_rule_elements(list(expr1, expr2, expr3, expr4))
  expect_equal(deparse(result), "a > 1 & b < 2 & c == 3 & d >= 4")
})

test_that("combine_rule_elements() combines expressions with OR operator", {
  expr1 <- rlang::expr(x > 5)
  expr2 <- rlang::expr(y < 10)
  result <- combine_rule_elements(list(expr1, expr2), operator = "|")
  expect_equal(deparse(result), "x > 5 | y < 10")
})

test_that("combine_rule_elements() combines three expressions with OR", {
  expr1 <- rlang::expr(x > 5)
  expr2 <- rlang::expr(y < 10)
  expr3 <- rlang::expr(z == 3)
  result <- combine_rule_elements(list(expr1, expr2, expr3), operator = "|")
  expect_equal(deparse(result), "x > 5 | y < 10 | z == 3")
})

test_that("combine_rule_elements() returns TRUE for empty list", {
  result <- combine_rule_elements(list())
  expect_equal(deparse(result), "TRUE")
})

test_that("combine_rule_elements() returns single expression unchanged", {
  expr <- rlang::expr(x > 5)
  result <- combine_rule_elements(list(expr))
  expect_identical(result, expr)
})

test_that("combine_rule_elements() integrates with rect_split_to_expr()", {
  split1 <- list(column = "age", value = 30, operator = ">=")
  split2 <- list(column = "income", value = 50000, operator = ">")
  exprs <- list(
    rect_split_to_expr(split1),
    rect_split_to_expr(split2)
  )
  result <- combine_rule_elements(exprs)
  expect_equal(deparse(result), "age >= 30 & income > 50000")
})

test_that("combine_rule_elements() integrates with multiple rect_split_to_expr() calls", {
  split1 <- list(column = "age", value = 30, operator = ">=")
  split2 <- list(column = "income", value = 50000, operator = ">")
  split3 <- list(column = "city", value = c("NYC", "LA"), operator = "%in%")
  exprs <- list(
    rect_split_to_expr(split1),
    rect_split_to_expr(split2),
    rect_split_to_expr(split3)
  )
  result <- combine_rule_elements(exprs)

  # Check structure rather than exact deparse since %in% may format differently
  expect_equal(as.character(result[[1]]), "&")
  expect_equal(as.character(result[[2]][[1]]), "&")
})

test_that("combine_rule_elements() evaluates correctly with AND", {
  expr1 <- rlang::expr(x > 5)
  expr2 <- rlang::expr(y < 10)
  combined <- combine_rule_elements(list(expr1, expr2))

  test_data1 <- data.frame(x = 6, y = 9)
  expect_equal(eval(combined, test_data1), TRUE)

  test_data2 <- data.frame(x = 4, y = 9)
  expect_equal(eval(combined, test_data2), FALSE)

  test_data3 <- data.frame(x = 6, y = 11)
  expect_equal(eval(combined, test_data3), FALSE)
})

test_that("combine_rule_elements() evaluates correctly with OR", {
  expr1 <- rlang::expr(x > 5)
  expr2 <- rlang::expr(y < 10)
  combined <- combine_rule_elements(list(expr1, expr2), operator = "|")

  test_data1 <- data.frame(x = 6, y = 11)
  expect_equal(eval(combined, test_data1), TRUE)

  test_data2 <- data.frame(x = 4, y = 9)
  expect_equal(eval(combined, test_data2), TRUE)

  test_data3 <- data.frame(x = 4, y = 11)
  expect_equal(eval(combined, test_data3), FALSE)
})

test_that("combine_rule_elements() evaluates TRUE for empty list", {
  result <- combine_rule_elements(list())
  expect_equal(eval(result), TRUE)
})

test_that("combine_rule_elements() handles complex nested expressions", {
  expr1 <- rlang::expr(a > 1 | b < 2)
  expr2 <- rlang::expr(c == 3)
  result <- combine_rule_elements(list(expr1, expr2))

  # Verify it's an AND of the two expressions
  expect_equal(as.character(result[[1]]), "&")
  expect_equal(as.character(result[[2]][[1]]), "|")
})

test_that("combine_rule_elements() handles expressions with special characters", {
  expr1 <- rlang::expr(`my.var` > 5)
  expr2 <- rlang::expr(`my_var` < 10)
  result <- combine_rule_elements(list(expr1, expr2))

  expect_equal(as.character(result[[2]][[2]]), "my.var")
  expect_equal(as.character(result[[3]][[2]]), "my_var")
})

test_that("combine_rule_elements() handles expressions with %in% operator", {
  expr1 <- rlang::expr(x %in% c("a", "b"))
  expr2 <- rlang::expr(y > 5)
  result <- combine_rule_elements(list(expr1, expr2))

  expect_equal(as.character(result[[1]]), "&")
  expect_equal(as.character(result[[2]][[1]]), "%in%")
})

test_that("combine_rule_elements() validates exprs is a list", {
  expect_snapshot(
    combine_rule_elements("not a list"),
    error = TRUE
  )

  expect_snapshot(
    combine_rule_elements(rlang::expr(x > 5)),
    error = TRUE
  )

  expect_snapshot(
    combine_rule_elements(123),
    error = TRUE
  )
})

test_that("combine_rule_elements() validates operator", {
  expr <- rlang::expr(x > 5)

  expect_snapshot(
    combine_rule_elements(list(expr), operator = "invalid"),
    error = TRUE
  )

  expect_snapshot(
    combine_rule_elements(list(expr), operator = "&&"),
    error = TRUE
  )

  expect_snapshot(
    combine_rule_elements(list(expr), operator = "and"),
    error = TRUE
  )
})

test_that("combine_rule_elements() validates list elements are expressions", {
  expr <- rlang::expr(x > 5)

  expect_snapshot(
    combine_rule_elements(list(expr, "not an expression")),
    error = TRUE
  )

  expect_snapshot(
    combine_rule_elements(list(expr, 123)),
    error = TRUE
  )

  expect_snapshot(
    combine_rule_elements(list(expr, TRUE)),
    error = TRUE
  )
})

test_that("combine_rule_elements() validates against NULL elements", {
  expr <- rlang::expr(x > 5)

  expect_snapshot(
    combine_rule_elements(list(expr, NULL)),
    error = TRUE
  )

  expect_snapshot(
    combine_rule_elements(list(NULL, expr)),
    error = TRUE
  )
})

test_that("combine_rule_elements() accepts symbols", {
  sym1 <- rlang::sym("x")
  sym2 <- rlang::sym("y")
  result <- combine_rule_elements(list(sym1, sym2))
  expect_equal(deparse(result), "x & y")
})

# ------------------------------------------------------------------------------

test_that("rule_text() returns simple expressions", {
  expr <- rlang::expr(x > 5)
  expect_equal(rule_text(expr), "x > 5")
})

test_that("rule_text() returns combined expressions", {
  expr <- rlang::expr(x > 5 & y < 10)
  expect_equal(rule_text(expr), "x > 5 & y < 10")
})

test_that("rule_text() formats numeric values with digits", {
  expr <- rlang::expr(x > 1.23456789)
  expect_equal(rule_text(expr, digits = 2), "x > 1.2")
  expect_equal(rule_text(expr, digits = 4), "x > 1.235")
  expect_equal(rule_text(expr, digits = 6), "x > 1.23457")
})

test_that("rule_text() formats multiple numeric values", {
  expr <- rlang::expr(x > 1.23456 & y < 9.87654)
  expect_equal(rule_text(expr, digits = 2), "x > 1.2 & y < 9.9")
})

test_that("rule_text() returns bullets format", {
  expr <- rlang::expr(x > 5 & y < 10)
  result <- rule_text(expr, bullets = TRUE)
  expect_equal(result, "* x > 5\n* y < 10")
})

test_that("rule_text() returns three conditions with bullets", {
  expr <- rlang::expr(x > 5 & y < 10 & z == 3)
  result <- rule_text(expr, bullets = TRUE)
  expect_equal(result, "* x > 5\n* y < 10\n* z == 3")
})

test_that("rule_text() truncates long output with max_width", {
  expr <- rlang::expr(very_long_variable_name > 100 & another_long_name < 50)
  output <- rule_text(expr, max_width = 30)
  expect_equal(nchar(output), 30)
  expect_true(grepl("\\.\\.\\.$", output))
})

test_that("rule_text() truncation includes ... in width", {
  expr <- rlang::expr(x > 5 & y < 10)
  output <- rule_text(expr, max_width = 10)
  expect_equal(nchar(output), 10)
  expect_true(grepl("\\.\\.\\.$", output))
})

test_that("rule_text() does not truncate when under max_width", {
  expr <- rlang::expr(x > 5)
  expect_equal(rule_text(expr, max_width = 100), "x > 5")
})

test_that("rule_text() returns character string", {
  expr <- rlang::expr(x > 5)
  result <- rule_text(expr)
  expect_type(result, "character")
  expect_equal(result, "x > 5")
})

test_that("rule_text() works with rect_split_to_expr()", {
  split <- list(column = "age", value = 30.5, operator = ">=")
  expr <- rect_split_to_expr(split)
  expect_equal(rule_text(expr, digits = 2), "age >= 30")
})

test_that("rule_text() works with combine_rule_elements()", {
  split1 <- list(column = "age", value = 30, operator = ">=")
  split2 <- list(column = "income", value = 50000, operator = ">")
  rule <- combine_rule_elements(list(
    rect_split_to_expr(split1),
    rect_split_to_expr(split2)
  ))
  expect_equal(rule_text(rule), "age >= 30 & income > 50000")
})

test_that("rule_text() handles %in% operator", {
  expr <- rlang::expr(x %in% c("a", "b", "c"))
  expect_equal(rule_text(expr), 'x %in% c("a", "b", "c")')
})

test_that("rule_text() handles single condition with bullets", {
  expr <- rlang::expr(x > 5)
  result <- rule_text(expr, bullets = TRUE)
  expect_equal(result, "* x > 5")
})

test_that("rule_text() validates expr is an expression", {
  expect_snapshot(
    rule_text("not an expression"),
    error = TRUE
  )

  expect_snapshot(
    rule_text(123),
    error = TRUE
  )

  expect_snapshot(
    rule_text(NULL),
    error = TRUE
  )
})

test_that("rule_text() validates bullets is logical", {
  expr <- rlang::expr(x > 5)

  expect_snapshot(
    rule_text(expr, bullets = "true"),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, bullets = 1),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, bullets = c(TRUE, FALSE)),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, bullets = NA),
    error = TRUE
  )
})

test_that("rule_text() validates digits is a positive integer", {
  expr <- rlang::expr(x > 5)

  expect_snapshot(
    rule_text(expr, digits = "4"),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, digits = -1),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, digits = 0),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, digits = 1.5),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, digits = c(1, 2)),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, digits = NA),
    error = TRUE
  )
})

test_that("rule_text() validates max_width is numeric and >= 4", {
  expr <- rlang::expr(x > 5)

  expect_snapshot(
    rule_text(expr, max_width = "10"),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, max_width = 3),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, max_width = c(10, 20)),
    error = TRUE
  )

  expect_snapshot(
    rule_text(expr, max_width = NA),
    error = TRUE
  )
})

test_that("rule_text() handles negative numbers", {
  expr <- rlang::expr(x > -5.5)
  expect_equal(rule_text(expr, digits = 2), "x > -5.5")
})

test_that("rule_text() handles scientific notation", {
  expr <- rlang::expr(x > 1e10)
  output <- rule_text(expr, digits = 2)
  expect_true(grepl("x >", output))
})

test_that("rule_text() handles complex nested expressions with bullets", {
  expr <- rlang::expr(a > 1 & b < 2 & c == 3 & d >= 4)
  result <- rule_text(expr, bullets = TRUE)
  expect_equal(result, "* a > 1\n* b < 2\n* c == 3\n* d >= 4")
})
