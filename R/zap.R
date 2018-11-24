#' @importFrom rlang set_names list2 quos expr new_function eval_tidy missing_arg caller_env is_formula f_rhs abort
#' @importFrom assertthat assert_that
#' @importFrom purrr map map_dbl map_lgl map_int map_chr
#' @importFrom tibble add_column
#' @importFrom dplyr is_grouped_df tbl_vars
zapper_args <- function(.tbl) {
  args <- set_names(
    map(.tbl, ~ if (is.data.frame(.) || is.matrix(.)){
      expr( (!!.)[[`.::index::.`]])
    } else {
      expr( (!!.)[[`.::index::.`]])
    }),
    tbl_vars(.tbl)
  )
  list2(`.::index::.` = missing_arg(), !!!args )
}

#' Map over columns of a data frame simultaneously
#'
#' @param .tbl A data frame
#' @param ... A single named expression that returns
#' - a list for `zap()`
#' - a single integer for `zap_int()`
#' - a single double for `zap_dbl()`
#' - a single string for `zap_chr()`
#' - a single raw for `zap_raw()`
#' - a single boolean for `zap_lgl()`
#' - a data frame with one row for `zap_dfr()`
#'
#' @param .map the mapping function, eg. [map][purrr::map()] for `zap`, [map_int][purrr::map_int()] for `zap_int`, ...
#'
#' @return `.tbl` with an additional column
#'
#' @details
#'
#' `zip_*(x = <expr>)` is similar to `mutate(x = pmap_*(., function(<args>) <expr>))` where :
#'
#' - `<expr>` is an expression using the columns of `.tbl`
#' - `<args>` is the names of `.tbl` as formal arguments
#'
#' @examples
#'
#' library(purrr)
#' library(dplyr)
#' library(tibble)
#' tbl <- tibble(cyl = c(4, 6, 8), mpg = c(30, 25, 20))
#'
#' # inspired from https://github.com/tidyverse/purrr/issues/280#issuecomment-270844528
#' #
#' # with the mutate + pmap idiom:
#' tbl %>%
#'   mutate(x = pmap(.l = list(cyl, mpg), function(cc, mm) filter(mtcars, cyl == cc, mpg < mm))) %>%
#'   mutate(n = map_int(x, nrow))
#'
#' # with zap
#' tbl %>%
#'   zap(x = ~filter(mtcars, cyl == !!cyl, mpg < !!mpg)) %>%
#'   zap_int(n = ~nrow(x))
#'
#' @export
zap <- function(.tbl, ..., .map = map) {
  fs <- list(...)
  stopifnot(length(fs) == 1L, is_formula(fs[[1L]]))
  name <- names(fs)[[1L]]
  formula <- fs[[1L]]
  if (is_grouped_df(.tbl)) {
    abort("cannot zap a grouping variable")
  }

  body <- expr({
    rlang::eval_tidy(!!(f_rhs(formula)))
  })

  f <- new_function(zapper_args(.tbl), body, env = caller_env())

  .tbl[[name]] <-  .map(seq_len(nrow(.tbl)), f)
  .tbl
}

#' @rdname zap
#' @export
zap_dbl <- function(...) zap(..., .map = map_dbl)

#' @rdname zap
#' @export
zap_lgl <- function(...) zap(..., .map = map_lgl)

#' @rdname zap
#' @export
zap_int <- function(...) zap(..., .map = map_int)

#' @rdname zap
#' @export
zap_chr <- function(...) zap(..., .map = map_chr)

#' @rdname zap
#' @export
zap_raw <- function(...) zap(..., .map = map_chr)

#' @rdname zap
#' @export
zap_dfr <- function(...) zap(..., .map = map_dfr)

