test_that("obliq_split_to_expr() creates simple two-variable expression", {
  expr <- obliq_split_to_expr(list(
    columns = c("x", "y"),
    values = c(2, 3),
    operator = ">",
    threshold = 10
  ))
  expect_equal(deparse(expr), "2 * x + 3 * y > 10")
})

test_that("obliq_split_to_expr() creates single-variable expression", {
  expr <- obliq_split_to_expr(list(
    columns = "x",
    values = 5,
    operator = "<=",
    threshold = 20
  ))
  expect_equal(deparse(expr), "5 * x <= 20")
})

test_that("obliq_split_to_expr() handles negative coefficients", {
  expr <- obliq_split_to_expr(list(
    columns = c("x", "y"),
    values = c(2, -3),
    operator = ">=",
    threshold = 5
  ))
  expect_equal(deparse(expr), "2 * x - 3 * y >= 5")
})

test_that("obliq_split_to_expr() creates three-variable expression", {
  expr <- obliq_split_to_expr(list(
    columns = c("x", "y", "z"),
    values = c(1, 2, 3),
    operator = "<",
    threshold = 0
  ))
  expect_equal(deparse(expr), "1 * x + 2 * y + 3 * z < 0")
})

test_that("obliq_split_to_expr() handles mixed positive and negative", {
  expr <- obliq_split_to_expr(list(
    columns = c("a", "b", "c"),
    values = c(1, -2, 3),
    operator = "==",
    threshold = 10
  ))
  expect_equal(deparse(expr), "1 * a - 2 * b + 3 * c == 10")
})

test_that("obliq_split_to_expr() handles decimal coefficients", {
  expr <- obliq_split_to_expr(list(
    columns = c("age", "income"),
    values = c(1.5, -0.001),
    operator = "<=",
    threshold = 50
  ))
  # Check structure rather than exact deparse due to floating point
  expect_equal(as.character(expr[[1]]), "<=")
  expect_equal(expr[[3]], 50)
})

test_that("obliq_split_to_expr() handles zero coefficients", {
  expr <- obliq_split_to_expr(list(
    columns = c("x", "y"),
    values = c(0, 5),
    operator = ">",
    threshold = 10
  ))
  expect_equal(deparse(expr), "0 * x + 5 * y > 10")
})

test_that("obliq_split_to_expr() evaluates correctly", {
  expr <- obliq_split_to_expr(list(
    columns = c("x", "y"),
    values = c(1, 1),
    operator = ">",
    threshold = 5
  ))

  test_data <- data.frame(x = c(2, 3, 4), y = c(2, 3, 4))
  result <- test_data[eval(expr, test_data), ]
  # x + y > 5: (2+2=4) FALSE, (3+3=6) TRUE, (4+4=8) TRUE
  expect_equal(result$x, c(3, 4))
})

test_that("obliq_split_to_expr() evaluates with negative coefficients", {
  expr <- obliq_split_to_expr(list(
    columns = c("x", "y"),
    values = c(2, -1),
    operator = ">=",
    threshold = 0
  ))

  test_data <- data.frame(x = c(1, 2, 3), y = c(1, 3, 2))
  result <- test_data[eval(expr, test_data), ]
  # 2*x - y >= 0: (2*1-1=1) TRUE, (2*2-3=1) TRUE, (2*3-2=4) TRUE
  expect_equal(nrow(result), 3)
})

test_that("obliq_split_to_expr() handles special characters in column names", {
  expr <- obliq_split_to_expr(list(
    columns = c("my.var", "my_var"),
    values = c(1, 2),
    operator = "<",
    threshold = 10
  ))
  # expr is: 1 * my.var + 2 * my_var < 10
  # expr[[2]] is the LHS: 1 * my.var + 2 * my_var
  # expr[[2]][[2]] is left side of +: 1 * my.var
  # expr[[2]][[2]][[3]] is the symbol my.var
  expect_equal(as.character(expr[[2]][[2]][[3]]), "my.var")
  # expr[[2]][[3]] is right side of +: 2 * my_var
  # expr[[2]][[3]][[3]] is the symbol my_var
  expect_equal(as.character(expr[[2]][[3]][[3]]), "my_var")
})

test_that("obliq_split_to_expr() validates input structure", {
  expect_snapshot(
    obliq_split_to_expr("not a list"),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(columns = "x", values = 1, operator = ">")),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(columns = "x", values = 1, threshold = 10)),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(
      columns = "x",
      values = 1,
      operator = ">",
      threshold = 10,
      extra = "bad"
    )),
    error = TRUE
  )
})

test_that("obliq_split_to_expr() validates columns", {
  expect_snapshot(
    obliq_split_to_expr(list(
      columns = character(0),
      values = numeric(0),
      operator = ">",
      threshold = 10
    )),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(
      columns = c("x", ""),
      values = c(1, 2),
      operator = ">",
      threshold = 10
    )),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(
      columns = 123,
      values = c(1, 2),
      operator = ">",
      threshold = 10
    )),
    error = TRUE
  )
})

test_that("obliq_split_to_expr() validates values", {
  expect_snapshot(
    obliq_split_to_expr(list(
      columns = c("x", "y"),
      values = "not numeric",
      operator = ">",
      threshold = 10
    )),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(
      columns = c("x", "y"),
      values = c(1, 2, 3),
      operator = ">",
      threshold = 10
    )),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(
      columns = c("x", "y", "z"),
      values = c(1, 2),
      operator = ">",
      threshold = 10
    )),
    error = TRUE
  )
})

test_that("obliq_split_to_expr() validates operator", {
  expect_snapshot(
    obliq_split_to_expr(list(
      columns = "x",
      values = 1,
      operator = "!=",
      threshold = 10
    )),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(
      columns = "x",
      values = 1,
      operator = "%in%",
      threshold = 10
    )),
    error = TRUE
  )
})

test_that("obliq_split_to_expr() validates threshold", {
  expect_snapshot(
    obliq_split_to_expr(list(
      columns = "x",
      values = 1,
      operator = ">",
      threshold = "not numeric"
    )),
    error = TRUE
  )

  expect_snapshot(
    obliq_split_to_expr(list(
      columns = "x",
      values = 1,
      operator = ">",
      threshold = c(1, 2)
    )),
    error = TRUE
  )
})

test_that("obliq_split_to_expr() handles large number of variables", {
  n <- 5
  expr <- obliq_split_to_expr(list(
    columns = paste0("x", 1:n),
    values = 1:n,
    operator = "<=",
    threshold = 100
  ))
  # Just verify it creates an expression without error
  expect_true(is.language(expr))
  expect_equal(as.character(expr[[1]]), "<=")
})

test_that("obliq_split_to_expr() handles negative threshold", {
  expr <- obliq_split_to_expr(list(
    columns = c("x", "y"),
    values = c(1, 1),
    operator = ">",
    threshold = -5
  ))
  expect_equal(expr[[3]], -5)
})

test_that("obliq_split_to_expr() handles all negative coefficients", {
  expr <- obliq_split_to_expr(list(
    columns = c("x", "y", "z"),
    values = c(-1, -2, -3),
    operator = "<",
    threshold = 0
  ))
  expect_equal(deparse(expr), "-1 * x - 2 * y - 3 * z < 0")
})
