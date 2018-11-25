context("wap()")

test_that("wap() creates a list", {
  res <- wap(iris, ~ Sepal.Length *2)
  expect_is(res, "list")
  expect_equal(length(res), 150L)
  expect_equal(unlist(res), iris$Sepal.Length * 2)
})

test_that("wap() aborts when ... is not length 1", {
  expect_error(wap(iris, ~Sepal.Length, ~Sepal.Length))
})

test_that("wap() respects .ptype for purrr variants", {
  expect_equal(wap(iris, ~1L, .ptype = integer()), rep(1L, 150))
  expect_equal(wap(iris, ~1L, .ptype = double()), rep(1, 150))
  expect_equal(wap(iris, ~"", .ptype = character()), rep("", 150))
  expect_equal(wap(iris, ~as.raw(1L), .ptype = raw()), rep(as.raw(1L), 150))
  expect_equal(wap(iris, ~Sepal.Length < Sepal.Width, .ptype = logical()), iris$Sepal.Length < iris$Sepal.Width)

  expect_equal(wap_int(iris, ~1L), rep(1L, 150))
  expect_equal(wap_dbl(iris, ~1L), rep(1, 150))
  expect_equal(wap_chr(iris, ~""), rep("", 150))
  expect_equal(wap_raw(iris, ~as.raw(1L)), rep(as.raw(1L), 150))
  expect_equal(wap_lgl(iris, ~Sepal.Length < Sepal.Width), iris$Sepal.Length < iris$Sepal.Width)
})

test_that("wap(.ptype = data.frame() and wap_dfr()", {
  res1 <- wap_dfr(iris, ~data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width))
  res2 <- wap(iris, ~data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width), .ptype = data.frame())
  expected <- with(iris, data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width))

  expect_equal(res1, expected)
  expect_equal(res2, expected)
})

test_that("wap() can make factors", {
  f <- wap(iris, ~Species, .ptype = factor(levels = levels(iris$Species)))
  expect_equal(f, iris$Species)
})

test_that("wap() only accepts results with 1 observation when .ptype is specified", {
  expect_error(wap(iris, ~1:2, .ptype = integer()))
  expect_error(wap(iris, ~c(1,2), .ptype = double()))
  expect_error(wap(iris, ~letters, .ptype = character()))
  expect_error(wap(iris, ~c(TRUE, FALSE), .ptype = logical()))
  expect_error(wap(iris, ~as.raw(0:255), .ptype = raw()))
  expect_error(wap(iris, ~mtcars, .ptype = data.frame()))
})
