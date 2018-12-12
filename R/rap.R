#' @importFrom rlang set_names list2 quos expr new_function eval_tidy missing_arg caller_env
#' @importFrom rlang is_formula f_rhs abort is_vector dots_n f_env env sym f_lhs env_parent expr_print
#' @importFrom rlang eval_bare enexpr
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr map map_dbl map_lgl map_int map_chr map_dfr map_raw iwalk
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

rapper_args <- function(.tbl, env) {
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
  list2(`.::index::.` = missing_arg(), !!!args, ..data = quote(environment()), ..env = env )
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

# borrowed from https://github.com/r-lib/rlang/blob/148a166481ba19551afec649570efe2de53f0248/R/eval.R#L314
value <- function(expr) {
  eval_bare(enexpr(expr), caller_env())
}

prepare_wap <- function(.tbl, .f, check = TRUE) {
  if (check) {
      assert_that(
      is_formula(.f),
      msg  = ".f should be formula"
    )
  }
  lhs <- f_lhs(.f)

  # the type
  .ptype <- if (is.null(lhs)) list() else eval_tidy(lhs, env = f_env(.f))

  # the lambda
  body <- expr({
    value(!!(f_rhs(.f)))
  })
  env <- f_env(.f)
  lambda <- new_function(rapper_args(.tbl, env = env_parent(env)), body, env = env(value = value, env))
  attr(lambda, "class") <- "rap_lambda"

  # the mapper
  .map <- map_for(.ptype)

  list(
    lambda = lambda,
    mapper = .map
  )
}

#' Map over columns of a data frame simultaneously
#'
#' @param .tbl A data frame
#' @param .f a single formula
#' @param ... formulas
#'
#'  The *rhs* of each formula uses columns of `.tbl`, and each stands for a single
#'  observation.
#'
#'  The *lhs* of each formula indicates the type, in the [vctrs::vec_c()] sense.
#'
#'  - empty or `list()`: no check is performed on the results of
#'  the rhs expression and a list is returned.
#'
#'  - `data.frame()`:  to indicate that the rhs should evaluate
#'  to a data frame of 1 row. The data frames don't need to be of a specific types
#'  and are are combined with [vctrs::vec_rbind()].
#'
#'  - A data frame of a specific type, e.g. `data.frame(x = integer(), y = double())`
#'  The rhs should evaluate to a data frame of that type with 1 row.
#'
#'  - Any other ptype that makes sense for [vctrs::vec_c()]. Each result must
#'  validate `vctrs::vec_size(.) == 1L` and are combined with
#'  `vctrs::vec_c(!!!, .ptype = .ptype)`
#'
#'  In `rap()` if the formula is named, the result becomes a new column of the
#'  `tbl`, otherwise the formula is only used for side effects.
#'
#' @return
#'   - `wap()` returns a vector of the type specified by the lhs of the formula.
#'             The vector validates `vec_size() == nrow(.tbl)`. This is similar
#'             to [purrr::pmap()]
#'
#'   - `rap()` adds a column to `.tbl` per formula in `...`
#'
#' @examples
#'
#' library(purrr)
#' library(dplyr)
#' library(tibble)
#'
#' tbl <- tibble(cyl_threshold = c(4, 6, 8), mpg_threshold = c(30, 25, 20))
#'
#' # ----- wap
#' # returns a list of 3 elements
#' tbl %>%
#'   wap(       ~ filter(mtcars, cyl == cyl_threshold, mpg < mpg_threshold))
#'
#' # same, i.e. list() is equivalent to empty
#' tbl %>%
#'   wap(list() ~ filter(mtcars, cyl == cyl_threshold, mpg < mpg_threshold))
#'
#' # can specify the output type with the formula lhs
#' tbl %>%
#'   wap(integer() ~ nrow(filter(mtcars, cyl == cyl_threshold, mpg < mpg_threshold)))
#'
#' # to make data frames
#' starwars %>%
#'   wap(data.frame() ~ data.frame(species = length(species), films = length(films)))
#'
#' # ----- rap: add columns
#' tbl %>%
#'   rap(
#'      x =           ~ filter(mtcars, cyl == cyl_threshold, mpg < mpg_threshold),
#'      n = integer() ~ nrow(x)
#'   )
#'
#' # rap is especially useful for iterating over multiple models
#' starwars %>%
#'   group_nest(gender) %>%
#'   rap(
#'     model =          ~ lm(height ~ mass + birth_year, data = data),
#'     perf  = double() ~ summary(model)$adj.r.squared
#'   )
#'
#' @rdname rap
#' @export
wap <- function(.tbl, .f) {
  c(lambda, mapper) %<-% prepare_wap(.tbl, .f = .f, check = TRUE)
  mapper(seq_len(nrow(.tbl)), lambda)
}

#' @rdname rap
#' @export
lap <- function(.tbl, .f) {
  prepare_wap(.tbl, .f, check = TRUE)$lambda
}

#' @export
print.rap_lambda <- function(x, ...) {
  expr_print(unclass(x))
  invisible(x)
}

#' @rdname rap
#' @export
rap <- function(.tbl, ...) {
  formulas <- list(...)
  if(is.null(formulas)) {
    names(formulas) <- rep("", length(formulas))
  }
  assert_that(
    all(map_lgl(formulas, is_formula)),
    msg = "`...` should be a named list of formulas"
  )

  iwalk(formulas, ~{
    c(lambda, mapper) %<-% prepare_wap(.tbl, .x, check = FALSE)

    if (is_grouped_df(.tbl) && .y %in% group_vars(.tbl)) {
      abort("cannot rap() a grouping variable")
    }

    res <- mapper(seq_len(nrow(.tbl)), lambda)
    if (.y != "") {
      .tbl[[.y]] <<- res
    }

  })
  .tbl
}
