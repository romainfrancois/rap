#' @importFrom rlang set_names list2 quos expr new_function eval_tidy missing_arg caller_env is_formula f_rhs abort is_vector dots_n f_env env sym
#' @importFrom assertthat assert_that
#' @importFrom purrr map map_dbl map_lgl map_int map_chr map_dfr map_raw
#' @importFrom tibble add_column
#' @importFrom dplyr is_grouped_df tbl_vars group_vars
#' @importFrom utils globalVariables
#' @importFrom vctrs vec_size vec_c vec_rbind
#' @importFrom zeallot %<-%
#' @importFrom magrittr %>%
is_bare_vector <- function(x) {
  is_vector(x) && !is.object(x) && is.null(attr(x, "class"))
}

#' @export
magrittr::`%>%`

globalVariables(c(".::index::.", ".::rhs::.", "lambda", "mapper", "name", "."))

observation_matrix <- function(.) {
  expr( (!!.)[`.::index::.`, , drop = FALSE])
}

observation_data_frame <- function(.) {
  expr( (!!.)[`.::index::.`, , drop = FALSE])
}

observation_object <- function(.) {
  expr( (!!.)[[`.::index::.`]])
}

observation_bare_vector <- function(.) {
  expr( .subset2(!!., `.::index::.`))
}

rapper_args <- function(.tbl) {
  args <- set_names(
    map(.tbl, ~ if (is.data.frame(.) ){
      observation_data_frame(.)
    } else if(is.matrix(.)){
      observation_matrix(.)
    } else if (is_bare_vector(.)) {
      observation_bare_vector(.)
    } else {
      observation_object(.)
    }),
    tbl_vars(.tbl)
  )
  list2(`.::index::.` = missing_arg(), !!!args )
}

map_for_type <- function(.ptype, combine = vec_c) {
  function(.x, .f, ...) {
    out <- map(.x, function(x){
      res <- .f(x, ...)
      stopifnot(vec_size(res) == 1L)
      res
    })
    combine(!!!out, .ptype = .ptype)
  }
}

map_for <- function(.ptype) {
  if (identical(.ptype, list())) {
    map
  } else if(identical(.ptype, integer())) {
    map_int
  } else if(identical(.ptype, double())) {
    map_dbl
  } else if(identical(.ptype, raw())) {
    map_raw
  } else if(identical(.ptype, character())) {
    map_chr
  } else if(identical(.ptype, logical())) {
    map_lgl
  } else if(is.data.frame(.ptype)) {
    if (ncol(.ptype) == 0L){
      map_for_type(NULL, vec_rbind)
    } else {
      map_for_type(.ptype, vec_rbind)
    }
  } else {
    map_for_type(.ptype, vec_c)
  }
}

prepare_wap <- function(.tbl, ..., .ptype, .named = TRUE) {
  fs <- list(...)
  assert_that(
    length(fs) == 1L,
    is_formula(fs[[1L]]),
    msg  = "`...` should be a single formula"
  )
  names <- names(fs)
  if(isTRUE(.named)) {
    assert_that(
      !is.null(names),
      msg = "The formula supplied in `...` must be named."
    )
  }

  # derive a function from the lambda
  formula <- fs[[1L]]

  body <- expr({
    rlang::eval_tidy(!!(f_rhs(formula)))
  })
  lambda <- new_function(rapper_args(.tbl), body, env = f_env(formula))
  attr(lambda, "class") <- "rap_lamdda"

  # get the map function
  .map <- map_for(.ptype)

  list(
    lambda = lambda,
    mapper = .map,
    name = if(.named) names[[1L]]
  )
}

#' Map over columns of a data frame simultaneously
#'
#' @param .tbl A data frame
#'
#' @param ... A single named formula.
#'
#'  The rhs of the formula uses columns of `.tbl`, and each stands for a single
#'  observation.
#'
#'  Evaluating the rhs of the formula should return a single observation of
#'  a type identified by `.ptype`. For example if `.ptype` is `integer()` the
#'  expression should evaluate to a single integer ...
#'
#' @param .ptype output type. The default `list()` uses [purrr::map()] to
#' iterate. No checks are performed on the results.
#'
#' Can be one of these special cases that take advantage of functions from `purrr`:
#'
#' - `integer()` : the iteration is performed by [purrr::map_int()]
#' - `double()` : the iteration is performed by [purrr::map_dbl()]
#' - `raw()` : the iteration is performed by [purrr::map_raw()]
#' - `logical()` : the iteration is performed by [purrr::map_lgl()]
#' - `character()` : the iteration is performed by [purrr::map_chr()]
#'
#' Can be `data.frame()`. Each result of the formula must be a data frame of
#' one observation. The data frames are combined with [vctrs::vec_rbind()].
#'
#' Can be a data frame of a specific type, e.g. `data.frame(x = integer(), y = double())`.
#' In that case the data frames must also be of one observation, but also must
#' be of the specified type. They are aggregated with [vctrs::vec_rbind()], passing along
#' the `.ptype`.
#'
#' Finally, for any other value of `.ptype`, the expression should give one
#' observation of that type, and they are combined eventually with [vctrs::vec_c()].
#'
#' @return
#'   - `wap()` and its variants return a vector of the appropriate type, e.g. `wap_dbl()` returns
#'             a numeric vector, `wap_int()` returns an integer vector, ...
#'
#'   - `rap()` and its variants return a data frame with the additional column
#'
#'   - `nap()` returns *n*othing, and can be used for side effects, similar to [purrr:::pwalk()]
#'
#' @details
#'
#' Suffixed versions of `wap()` and `rap()` are conveniences set the `.ptype`, e.g.
#' `wap_int(...)` is `wap(..., .ptype = integer())`, `rap_lgl(...)` is `rap(..., .ptype = logical())`
#'
#' @examples
#'
#' library(purrr)
#' library(dplyr)
#' library(tibble)
#'
#' tbl <- tibble(cyl = c(4, 6, 8), mpg = c(30, 25, 20))
#'
#' # inspired from https://github.com/tidyverse/purrr/issues/280#issuecomment-270844528
#' # wap returns a list
#' tbl %>%
#'   wap(~ filter(mtcars, cyl == !!cyl, mpg < !!mpg))
#'
#' # can use the .ptype to indicate the type of result
#' # in the vctrs sense:
#' tbl %>%
#'   wap(~ nrow(filter(mtcars, cyl == !!cyl, mpg < !!mpg)), .ptype = integer())
#'
#' # or alternatively use the suffixed versions, à la purrr:
#' tbl %>%
#'   wap_int(~ nrow(filter(mtcars, cyl == !!cyl, mpg < !!mpg)))
#'
#' # wap(.ptype = data.frame()) or wap_dfr row binds data frames
#' tbl %>%
#'   wap_dfr(~ data.frame(a = cyl * 2, b = mpg + 1))
#'
#' # rap adds a column to a data frame
#' tbl %>%
#'   rap(x = ~filter(mtcars, cyl == !!cyl, mpg < !!mpg)) %>%
#'   rap(n = ~nrow(x), .ptype = integer())
#'
#' @rdname rap
#' @export
wap <- function(.tbl, ..., .ptype = list()) {
  c(lambda, mapper, .) %<-% prepare_wap(.tbl, ..., .ptype = .ptype, .named = FALSE)
  mapper(seq_len(nrow(.tbl)), lambda)
}

#' @rdname rap
#' @export
nap <- function(.tbl, ...) {
  wap(.tbl, ...)
  invisible(.tbl)
}

#' @rdname rap
#' @export
lap <- function(.tbl, ..., .ptype = list()) {
  prepare_wap(.tbl, ..., .ptype = .ptype, .named = FALSE)$lambda
}

#' @export
print.rap_lamdda <- function(x, ...) {
  # TODO
  NextMethod()
}

#' @rdname rap
#' @export
wap_dbl <- function(...) wap(..., .ptype = double())

#' @rdname rap
#' @export
wap_lgl <- function(...) wap(..., .ptype = logical())

#' @rdname rap
#' @export
wap_int <- function(...) wap(..., .ptype = integer())

#' @rdname rap
#' @export
wap_chr <- function(...) wap(..., .ptype = character())

#' @rdname rap
#' @export
wap_raw <- function(...) wap(..., .ptype = raw())

#' @rdname rap
#' @export
wap_dfr <- function(...) wap(..., .ptype = data.frame())

#' @rdname rap
#' @export
rap <- function(.tbl, ..., .ptype = list()) {
  c(lambda, mapper, name) %<-% prepare_wap(.tbl, ..., .ptype = .ptype, .named = TRUE)

  if (is_grouped_df(.tbl) && name %in% group_vars(.tbl)) {
    abort("cannot rap() a grouping variable")
  }

  .tbl[[name]] <- mapper(seq_len(nrow(.tbl)), lambda)
  .tbl
}

#' @rdname rap
#' @export
rap_dbl <- function(...) rap(..., .ptype = double())

#' @rdname rap
#' @export
rap_lgl <- function(...) rap(..., .ptype = logical())

#' @rdname rap
#' @export
rap_int <- function(...) rap(..., .ptype = integer())

#' @rdname rap
#' @export
rap_chr <- function(...) rap(..., .ptype = character())

#' @rdname rap
#' @export
rap_raw <- function(...) rap(..., .ptype = raw())

#' @rdname rap
#' @export
rap_dfr <- function(...) rap(..., .ptype = data.frame())
