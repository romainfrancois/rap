context("wap()")

test_that("wap() creates a list", {
  res <- wap(iris, ~ Sepal.Length *2)
  expect_is(res, "list")
  expect_equal(length(res), 150L)
  expect_equal(unlist(res), iris$Sepal.Length * 2)
})

test_that("wap() respects basic types", {
  expect_equal(wap(iris, integer()    ~ 1L), rep(1L, 150))
  expect_equal(wap(iris, double()     ~ 1L), rep(1, 150))
  expect_equal(wap(iris, character()  ~ ""), rep("", 150))
  expect_equal(wap(iris, raw()        ~ as.raw(1L)), rep(as.raw(1L), 150))
  expect_equal(wap(iris, logical()    ~ Sepal.Length < Sepal.Width), iris$Sepal.Length < iris$Sepal.Width)
})

test_that("wap() with data.frame() in lhs", {
  res <- wap(iris, data.frame() ~ data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width))
  expected <- with(iris, data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width))

  expect_equal(res, expected)
})

test_that("wap() can make factors", {
  f <- wap(iris, iris$Species ~ Species)
  expect_equal(f, iris$Species)
})

test_that("wap() only accepts results with 1 observation when .ptype is specified", {
  expect_error(wap(iris, integer()    ~ 1:2))
  expect_error(wap(iris, double()     ~ c(1,2)))
  expect_error(wap(iris, character()  ~ letters))
  expect_error(wap(iris, logical()    ~ c(TRUE, FALSE)))
  expect_error(wap(iris, raw()        ~ as.raw(0:255)))
  expect_error(wap(iris, data.frame() ~ mtcars))
})
