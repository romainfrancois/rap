context("rap()")

test_that("rap() creates a list column", {
  res <- rap(iris, x = ~ Sepal.Length *2)
  expect_is(res, "data.frame")
  expect_equal(names(res), c(names(iris), "x"))
  expect_is(res[["x"]], "list")
  expect_equal(length(res[["x"]]), 150L)
  expect_equal(unlist(res[["x"]]), iris$Sepal.Length * 2)
})

test_that("rap() aborts when ... is not length 1", {
  expect_error(
    rap(iris, ~Sepal.Length, ~Sepal.Length),
    "`...` should be a single formula"
  )
})

test_that("rap() formulas must be named", {
  expect_error(
    rap(iris, ~Sepal.Length),
    "The formula supplied in `...` must be named."
  )
})


test_that("rap() respects .ptype for purrr variants", {
  expect_equal(rap(iris, x = ~1L, .ptype = integer()), tibble::add_column(iris, x = rep(1L, 150)))
  expect_equal(rap(iris, x = ~1L, .ptype = double()), tibble::add_column(iris, x = rep(1, 150)))
  expect_equal(rap(iris, x = ~"", .ptype = character()), tibble::add_column(iris, x = rep("", 150)))
  expect_equal(rap(iris, x = ~as.raw(1L), .ptype = raw()), tibble::add_column(iris, x = rep(as.raw(1L), 150)))
  expect_equal(rap(iris, x = ~Sepal.Length < Sepal.Width, .ptype = logical()), tibble::add_column(iris, x = iris$Sepal.Length < iris$Sepal.Width))

  expect_equal(rap_int(iris, x = ~1L), tibble::add_column(iris, x = rep(1L, 150)))
  expect_equal(rap_dbl(iris, x = ~1L), tibble::add_column(iris, x = rep(1, 150)))
  expect_equal(rap_chr(iris, x = ~""), tibble::add_column(iris, x = rep("", 150)))
  expect_equal(rap_raw(iris, x = ~as.raw(1L)), tibble::add_column(iris, x = rep(as.raw(1L), 150)))
  expect_equal(rap_lgl(iris, x = ~Sepal.Length < Sepal.Width), tibble::add_column(iris, x = iris$Sepal.Length < iris$Sepal.Width))
})

test_that("wap(.ptype = data.frame() and wap_dfr()", {
  res1 <- rap_dfr(iris, x = ~data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width))
  res2 <- rap(iris, x = ~data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width), .ptype = data.frame())
  expected <- tibble::add_column(iris, x = with(iris, data.frame(Sepal = Sepal.Length * Sepal.Width, Petal = Petal.Length * Petal.Width)))

  expect_equal(res1, expected)
  expect_equal(res2, expected)
})

test_that("rap() can make factors", {
  expect_equal(
    rap(iris, x = ~Species, .ptype = factor(levels = levels(iris$Species))),
    tibble::add_column(iris, x = iris$Species)
  )
})

test_that("rap() only accepts results with 1 observation when .ptype is specified", {
  expect_error(rap(iris, x = ~1:2, .ptype = integer()))
  expect_error(rap(iris, x = ~c(1,2), .ptype = double()))
  expect_error(rap(iris, x = ~letters, .ptype = character()))
  expect_error(rap(iris, x = ~c(TRUE, FALSE), .ptype = logical()))
  expect_error(rap(iris, x = ~as.raw(0:255), .ptype = raw()))
  expect_error(rap(iris, x = ~mtcars, .ptype = data.frame()))
})
