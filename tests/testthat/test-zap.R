context("zap()")

test_that("zap() creates a list column", {
  res <- zap(iris, x = ~ Sepal.Length *2)
  expect_is(res, "data.frame")
  expect_equal(names(res), c(names(iris), "x"))
  expect_is(res[["x"]], "list")
  expect_equal(length(res[["x"]]), 150L)
  expect_equal(unlist(res[["x"]]), iris$Sepal.Length * 2)
})

test_that("zap() aborts when ... is not length 1", {
  expect_error(zap(iris, ~Sepal.Length, ~Sepal.Length))
})

test_that("zap() formulas must be named", {
  expect_error(zap(iris, ~Sepal.Length))
})


test_that("wap() respects .ptype for purrr variants", {
  expect_equal(zap(iris, x = ~1L, .ptype = integer()), tibble::add_column(iris, x = rep(1L, 150)))
  expect_equal(zap(iris, x = ~1L, .ptype = double()), tibble::add_column(iris, x = rep(1, 150)))
  expect_equal(zap(iris, x = ~"", .ptype = character()), tibble::add_column(iris, x = rep("", 150)))
  expect_equal(zap(iris, x = ~as.raw(1L), .ptype = raw()), tibble::add_column(iris, x = rep(as.raw(1L), 150)))
  expect_equal(zap(iris, x = ~Sepal.Length < Sepal.Width, .ptype = logical()), tibble::add_column(iris, x = iris$Sepal.Length < iris$Sepal.Width))

  expect_equal(zap_int(iris, x = ~1L), tibble::add_column(iris, x = rep(1L, 150)))
  expect_equal(zap_dbl(iris, x = ~1L), tibble::add_column(iris, x = rep(1, 150)))
  expect_equal(zap_chr(iris, x = ~""), tibble::add_column(iris, x = rep("", 150)))
  expect_equal(zap_raw(iris, x = ~as.raw(1L)), tibble::add_column(iris, x = rep(as.raw(1L), 150)))
  expect_equal(zap_lgl(iris, x = ~Sepal.Length < Sepal.Width), tibble::add_column(iris, x = iris$Sepal.Length < iris$Sepal.Width))
})

test_that("wap(.ptype = data.frame() and wap_dfr()", {
  res1 <- zap_dfr(iris, x = ~data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width))
  res2 <- zap(iris, x = ~data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width), .ptype = data.frame())
  expected <- tibble::add_column(iris, x = with(iris, data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width)))

  expect_equal(res1, expected)
  expect_equal(res2, expected)
})

test_that("wap() can make factors", {
  expect_equal(
    zap(iris, x = ~Species, .ptype = factor(levels = levels(iris$Species))),
    tibble::add_column(iris, x = iris$Species)
  )
})

test_that("wap() only accepts results with 1 observation when .ptype is specified", {
  expect_error(zap(iris, x = ~1:2, .ptype = integer()))
  expect_error(zap(iris, x = ~c(1,2), .ptype = double()))
  expect_error(zap(iris, x = ~letters, .ptype = character()))
  expect_error(zap(iris, x = ~c(TRUE, FALSE), .ptype = logical()))
  expect_error(zap(iris, x = ~as.raw(0:255), .ptype = raw()))
  expect_error(zap(iris, x = ~mtcars, .ptype = data.frame()))
})
