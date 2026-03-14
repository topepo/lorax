test_that("rect_split_to_expr() creates numeric comparisons", {
  expr <- rect_split_to_expr(list(column = "age", value = 25, operator = "<"))
  expect_equal(deparse(expr), "age < 25")

  expr <- rect_split_to_expr(list(column = "age", value = 30, operator = "<="))
  expect_equal(deparse(expr), "age <= 30")

  expr <- rect_split_to_expr(list(column = "age", value = 40, operator = ">"))
  expect_equal(deparse(expr), "age > 40")

  expr <- rect_split_to_expr(list(column = "age", value = 50, operator = ">="))
  expect_equal(deparse(expr), "age >= 50")
})

test_that("rect_split_to_expr() creates equality comparisons", {
  expr <- rect_split_to_expr(list(column = "count", value = 5, operator = "=="))
  expect_equal(deparse(expr), "count == 5")

  expr <- rect_split_to_expr(list(
    column = "color",
    value = "red",
    operator = "=="
  ))
  expect_equal(deparse(expr), "color == \"red\"")
})

test_that("rect_split_to_expr() creates %in% comparisons", {
  expr <- rect_split_to_expr(
    list(column = "color", value = c("red", "blue"), operator = "%in%")
  )
  expect_equal(as.character(expr[[1]]), "%in%")
  expect_equal(as.character(expr[[2]]), "color")
  expect_equal(expr[[3]], c("red", "blue"))
})

test_that("rect_split_to_expr() evaluates correctly", {
  test_data <- data.frame(
    age = c(20, 30, 40, 50),
    color = c("red", "green", "blue", "red")
  )

  # Numeric less than
  expr <- rect_split_to_expr(list(column = "age", value = 35, operator = "<"))
  result <- test_data[eval(expr, test_data), ]
  expect_equal(result$age, c(20, 30))

  # Numeric greater or equal
  expr <- rect_split_to_expr(list(column = "age", value = 30, operator = ">="))
  result <- test_data[eval(expr, test_data), ]
  expect_equal(result$age, c(30, 40, 50))

  # Character equality
  expr <- rect_split_to_expr(list(
    column = "color",
    value = "red",
    operator = "=="
  ))
  result <- test_data[eval(expr, test_data), ]
  expect_equal(result$age, c(20, 50))

  # Character %in%
  expr <- rect_split_to_expr(
    list(column = "color", value = c("red", "blue"), operator = "%in%")
  )
  result <- test_data[eval(expr, test_data), ]
  expect_equal(result$age, c(20, 40, 50))
})

test_that("rect_split_to_expr() handles special characters in column names", {
  expr <- rect_split_to_expr(list(column = "my.var", value = 1, operator = "<"))
  expect_equal(as.character(expr[[2]]), "my.var")

  expr <- rect_split_to_expr(list(column = "my_var", value = 1, operator = "<"))
  expect_equal(as.character(expr[[2]]), "my_var")

  expr <- rect_split_to_expr(list(column = "my var", value = 1, operator = "<"))
  expect_equal(as.character(expr[[2]]), "my var")
})

test_that("rect_split_to_expr() handles special characters in values", {
  expr <- rect_split_to_expr(
    list(column = "text", value = "don't", operator = "==")
  )
  expect_equal(expr[[3]], "don't")

  expr <- rect_split_to_expr(
    list(column = "text", value = c("a\"b", "c"), operator = "%in%")
  )
  expect_equal(expr[[3]], c("a\"b", "c"))
})

test_that("rect_split_to_expr() handles edge cases", {
  # Very large numbers
  expr <- rect_split_to_expr(list(column = "x", value = 1e10, operator = ">"))
  expect_equal(deparse(expr), "x > 1e+10")

  # Negative numbers
  expr <- rect_split_to_expr(list(column = "x", value = -5.5, operator = "<="))
  expect_equal(deparse(expr), "x <= -5.5")

  # Empty character vector for %in% (edge case, may want to test behavior)
  expr <- rect_split_to_expr(list(
    column = "x",
    value = character(0),
    operator = "%in%"
  ))
  expect_equal(length(expr[[3]]), 0)
})

test_that("rect_split_to_expr() validates input structure", {
  expect_snapshot(
    rect_split_to_expr("not a list"),
    error = TRUE
  )

  expect_snapshot(
    rect_split_to_expr(list(column = "x", value = 1)),
    error = TRUE
  )

  expect_snapshot(
    rect_split_to_expr(list(column = "x", operator = "<")),
    error = TRUE
  )

  expect_snapshot(
    rect_split_to_expr(list(value = 1, operator = "<")),
    error = TRUE
  )

  expect_snapshot(
    rect_split_to_expr(list(
      column = "x",
      value = 1,
      operator = "<",
      extra = "bad"
    )),
    error = TRUE
  )
})

test_that("rect_split_to_expr() validates column", {
  expect_snapshot(
    rect_split_to_expr(list(column = "", value = 1, operator = "<")),
    error = TRUE
  )

  expect_snapshot(
    rect_split_to_expr(list(column = c("x", "y"), value = 1, operator = "<")),
    error = TRUE
  )

  expect_snapshot(
    rect_split_to_expr(list(column = 123, value = 1, operator = "<")),
    error = TRUE
  )
})

test_that("rect_split_to_expr() validates operator", {
  expect_snapshot(
    rect_split_to_expr(list(column = "x", value = 1, operator = "!=")),
    error = TRUE
  )

  expect_snapshot(
    rect_split_to_expr(list(column = "x", value = 1, operator = "<<")),
    error = TRUE
  )
})

test_that("rect_split_to_expr() validates value for operator", {
  # %in% requires character vector
  expect_snapshot(
    rect_split_to_expr(list(column = "x", value = 1, operator = "%in%")),
    error = TRUE
  )

  # %in% with single character should use ==
  expect_snapshot(
    rect_split_to_expr(list(column = "x", value = "red", operator = "%in%")),
    error = TRUE
  )

  # Other operators require length 1
  expect_snapshot(
    rect_split_to_expr(list(column = "x", value = c(1, 2), operator = "<")),
    error = TRUE
  )

  expect_snapshot(
    rect_split_to_expr(list(
      column = "x",
      value = c("a", "b"),
      operator = "=="
    )),
    error = TRUE
  )
})

test_that("rect_split_to_expr() handles NA values", {
  expr <- rect_split_to_expr(list(
    column = "x",
    value = NA_real_,
    operator = "=="
  ))
  expect_true(is.na(expr[[3]]))

  expr <- rect_split_to_expr(list(
    column = "x",
    value = NA_character_,
    operator = "=="
  ))
  expect_true(is.na(expr[[3]]))
})

test_that("rect_split_to_expr() handles unicode", {
  expr <- rect_split_to_expr(list(
    column = "text",
    value = "café",
    operator = "=="
  ))
  expect_equal(expr[[3]], "café")

  expr <- rect_split_to_expr(
    list(column = "text", value = c("café", "naïve"), operator = "%in%")
  )
  expect_equal(expr[[3]], c("café", "naïve"))
})
