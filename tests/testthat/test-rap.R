context("rap()")

test_that("rap() with no lhs a list column", {
  res <- rap(iris, x = ~ Sepal.Length *2)
  expect_is(res, "data.frame")
  expect_equal(names(res), c(names(iris), "x"))
  expect_is(res[["x"]], "list")
  expect_equal(length(res[["x"]]), 150L)
  expect_equal(unlist(res[["x"]]), iris$Sepal.Length * 2)
})

test_that("rap() formulas must be named", {
  expect_error(
    rap(iris, ~Sepal.Length),
    "`...` should be a named list of formulas"
  )
})

test_that("rap() respects lhs of formula for purrr variants", {
  res <- iris %>%
    rap(
      int = integer()   ~ 1L,
      dbl = double()    ~ 1L,
      chr = character() ~ "",
      raw = raw()       ~ as.raw(1),
      lgl = logical()   ~ Sepal.Length < Sepal.Width
    )
  expected <- iris %>%
    tibble::add_column(
      int = 1L,
      dbl = 1,
      chr = "",
      raw = as.raw(1),
      lgl = iris$Sepal.Length < iris$Sepal.Width
    )
  expect_equal(res, expected)
})

test_that("rap() can make data frame columns", {
  res <- rap(iris,
    x = data.frame() ~ data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width)
  )
  expected <- tibble::add_column(
    iris,
    x = with(iris, data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width))
  )
  expect_equal(res, expected)
})

test_that("rap() can make factors", {
  expect_equal(
    rap(iris, x = iris$Species ~ Species),
    tibble::add_column(iris, x = iris$Species)
  )
})

test_that("rap() only accepts results with 1 observation when type is not list()", {
  expect_error(rap(iris, x = integer()    ~ 1:2))
  expect_error(rap(iris, x = double()     ~ c(1,2)))
  expect_error(rap(iris, x = character()  ~ letters))
  expect_error(rap(iris, x = logical()    ~ c(TRUE, FALSE)))
  expect_error(rap(iris, x = raw()        ~ as.raw(0:255)))
  expect_error(rap(iris, x = data.frame() ~mtcars))
})
