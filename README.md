
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rap

[![Lifecycle
Status](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/)
[![Travis build
status](https://travis-ci.org/romainfrancois/rap.svg?branch=master)](https://travis-ci.org/romainfrancois/rap)

![](https://media.giphy.com/media/l41Yy7rv1mVZNQCT6/giphy.gif)

Experimenting with yet another way to do rowwise operations.

## Installation

You can install `rap` from gitub

``` r
# install.packages("devtools")
detools::install_github("romainfrancois/rap")
```

## Why

This offers `rap()` as an alternative to some versions of:

  - `rowwise()` + `do()`
  - `mutate()` + `pmap()`
  - maybe `purrrlyr` ?
  - probably other approaches

`rap()` works with a lambda, similar to `purrr::map()` but instead of
`.x`, `.y`, `..1`, `..2`, … the lambda can use the column names, which
stand for a single element of the associated vector, in the `[[` sense.

## rap

``` r
library(tidyverse)
#> ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.0.0.9000      ✔ purrr   0.2.5.9000 
#> ✔ tibble  1.4.99.9005     ✔ dplyr   0.7.99.9000
#> ✔ tidyr   0.8.2.9000      ✔ stringr 1.3.1      
#> ✔ readr   1.1.1           ✔ forcats 0.3.0.9000
#> ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(rap)

tbl <- tibble(cyl = c(4, 6, 8), mpg = c(30, 25, 20)) 
tbl
#> # A tibble: 3 x 2
#>     cyl   mpg
#>   <dbl> <dbl>
#> 1     4    30
#> 2     6    25
#> 3     8    20
```

Based on this [issue](https://github.com/tidyverse/purrr/issues/280), we
want subset of rows of `mtcars` where `cyl` is equal to the `tbl$cyl`
and `mpg` is smaller than `tbl$mpg`

There are many ways to do that, on the issue Jenny uses a `mutate +
pmap` idiom:

``` r
tbl %>%
  mutate(x = pmap(
    .l = list(cyl, mpg),
    function(cc, mm) filter(mtcars, cyl == cc, mpg < mm))
  )
#> # A tibble: 3 x 3
#>     cyl   mpg x                     
#>   <dbl> <dbl> <list>                
#> 1     4    30 <data.frame [7 × 11]> 
#> 2     6    25 <data.frame [7 × 11]> 
#> 3     8    20 <data.frame [14 × 11]>
```

`rap` attempts to simplify this by iterating over the columns
simultaneously, and exposing them with their name:

``` r
tbl %>% 
  rap(x = ~filter(mtcars, cyl == !!cyl, mpg < !!mpg) )
#> # A tibble: 3 x 3
#>     cyl   mpg x                     
#>   <dbl> <dbl> <list>                
#> 1     4    30 <data.frame [7 × 11]> 
#> 2     6    25 <data.frame [7 × 11]> 
#> 3     8    20 <data.frame [14 × 11]>
```

In `purrr::` fashion, `rap()` has variants to force the type of output,
e.g. `rap_int()`:

``` r
tbl %>% 
  rap(x = ~filter(mtcars, cyl == !!cyl, mpg < !!mpg) ) %>% 
  rap_int( n = ~nrow(x))
#> # A tibble: 3 x 4
#>     cyl   mpg x                          n
#>   <dbl> <dbl> <list>                 <int>
#> 1     4    30 <data.frame [7 × 11]>      7
#> 2     6    25 <data.frame [7 × 11]>      7
#> 3     8    20 <data.frame [14 × 11]>    14
```

## wap

`wap()` is the same but just returns the columns, `wap()` is `rap()` +
`dplyr::pull()`

``` r
library(dplyr)

starwars <- head(starwars)

# creates a list of length 1 integer vectors
starwars %>% 
  wap(~length(films)) 
#> [[1]]
#> [1] 5
#> 
#> [[2]]
#> [1] 6
#> 
#> [[3]]
#> [1] 7
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 
#> [[6]]
#> [1] 3

# using .ptype or the _int suffix to get a integer vector instead
starwars %>% 
  wap(~length(films), .ptype = integer())
#> [1] 5 6 7 4 5 3
starwars %>% 
  wap_int(~length(films))
#> [1] 5 6 7 4 5 3

# list of data frames
starwars %>% 
  wap(~data.frame(vehicles = length(vehicles), starships = length(starships)))
#> [[1]]
#>   vehicles starships
#> 1        2         2
#> 
#> [[2]]
#>   vehicles starships
#> 1        0         0
#> 
#> [[3]]
#>   vehicles starships
#> 1        0         0
#> 
#> [[4]]
#>   vehicles starships
#> 1        0         1
#> 
#> [[5]]
#>   vehicles starships
#> 1        1         0
#> 
#> [[6]]
#>   vehicles starships
#> 1        0         0

# using .ptype = data.frame() or _dfr to rbind them
starwars %>% 
  wap(~ data.frame(vehicles = length(vehicles), starships = length(starships)), .ptype = data.frame())
#>   vehicles starships
#> 1        2         2
#> 2        0         0
#> 3        0         0
#> 4        0         1
#> 5        1         0
#> 6        0         0
starwars %>% 
  wap_dfr(~data.frame(vehicles = length(vehicles), starships = length(starships)))
#>   vehicles starships
#> 1        2         2
#> 2        0         0
#> 3        0         0
#> 4        0         1
#> 5        1         0
#> 6        0         0
```

## zest\_join

🍋 `zest_join()` is similar to `dplyr::nest_join()` but you control what
goes in the nested column. `Z` is `N` but ⤵️.

``` r
tbl <- tibble(cyl = c(4, 6, 8), mpg = c(30, 25, 20)) 
tbl %>%
  zest_join(mtcars, data = ~cyl == !!cyl & mpg < !!mpg)
#> # A tibble: 3 x 3
#>     cyl   mpg data                  
#>   <dbl> <dbl> <list>                
#> 1     4    30 <data.frame [7 × 11]> 
#> 2     6    25 <data.frame [7 × 11]> 
#> 3     8    20 <data.frame [14 × 11]>
```

In the rhs of the formula : - `cyl` and `mpg` refer to columns of
`mtcars` - `!!cyl` and `!!mpg` refer to the current value from `tbl`
