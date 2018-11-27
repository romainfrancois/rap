
#' zest join
#'
#' a zest join is similar to a [dplyr::nest_join()] but the rows of `y` that are
#' included in the list column are controlled by a predicate.
#'
#' @param x a tibble
#'
#' @param y another tibble
#'
#' @param ... a single predicate formula.
#'
#' The rhs of the formula is used y [dplyr::filter()] on `y` for each row of `x`.
#'
#'   - Literal column names refer to columns of `y`. Alternatively you can use `.data$`.
#'
#'   - To use the current value for a column of `x` you can use unquoting, e.g. `!!cyl`
#'
#' @return a tibble that contains all columns and rows of `x`, plus an additional list column:
#'   - its name is given by the name of the formula
#'   - each element of the column is a tibble
#'   - each of the tibbles is a subset of `y` according to the rhs of the formula
#'
#' @examples
#'
#' tbl <- tibble::tibble(cyl = c(4, 6, 8), mpg = c(30, 25, 20))
#'
#' # zest join of tbl and mtcars
#' # - the created column is called `data`
#' # - each element of the data column is the result of filter(mtcars, cyl == !!cyl & mpg < !!mpg)
#' #    - `cyl` and `mpg` refer to columns of `y`
#' #    - `!!cyl` and `!!mpg` refer to the current
#' tbl %>%
#'   zest_join(mtcars, data = ~cyl == !!cyl & mpg < !!mpg)
#'
#' # similar to
#' tbl %>%
#'   zap(data = ~filter(mtcars, cyl == !!cyl & mpg < !!mpg))
#'
#' @export
zest_join <- function(x, y, ...) {
  c(lambda, mapper, name) %<-% prepare_wap(x, ..., .ptype = list(), .named = TRUE)

  predicate <- body(lambda)[[2]][[2]]
  body(lambda)[[2]][[2]] <- call('filter', sym(".::rhs::."), predicate)

  environment(lambda) <- env(`.::rhs::.` = y, filter = dplyr::filter, environment(lambda))

  x[[name]] <- mapper(seq_len(nrow(x)), lambda)
  x
}
