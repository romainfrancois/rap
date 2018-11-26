context("test-nap")

test_that("nap() returns its input", {
  expect_equal(nap(iris, ~Sepal.Length), iris)
})
