
<!-- README.md is generated from README.Rmd. Please edit that file -->
rap <img src="man/figures/logo.png" align="right" />
====================================================

[![Lifecycle Status](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/) [![Travis build status](https://travis-ci.org/romainfrancois/rap.svg?branch=master)](https://travis-ci.org/romainfrancois/rap)

![](https://media.giphy.com/media/l41Yy7rv1mVZNQCT6/giphy.gif)

Experimenting with yet another way to do rowwise operations.

Installation
------------

You can install `rap` from gitub

``` r
# install.packages("devtools")
devtools::install_github("romainfrancois/rap")
```

Why
---

This offers `rap()` as an alternative to some versions of:

-   `rowwise()` + `do()`
-   `mutate()` + `pmap()`
-   maybe `purrrlyr` ?
-   probably other approaches

`rap()` works with lambdas supplied as formulas, similar to `purrr::map()` but instead of `.x`, `.y`, `..1`, `..2`, ...the lambda can use the column names, which stand for a single element of the associated vector, in the `[[` sense.

rap
---

``` r
library(tidyverse)
#> ── Attaching packages ──────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.1.0           ✔ purrr   0.2.5.9000 
#> ✔ tibble  1.4.99.9006     ✔ dplyr   0.7.8      
#> ✔ tidyr   0.8.1           ✔ stringr 1.3.1      
#> ✔ readr   1.1.1           ✔ forcats 0.3.0
#> ── Conflicts ─────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(rap)

tbl <- tibble(cyl_threshold = c(4, 6, 8), mpg_threshold = c(30, 25, 20)) 
tbl
#> # A tibble: 3 x 2
#>   cyl_threshold mpg_threshold
#>           <dbl>         <dbl>
#> 1             4            30
#> 2             6            25
#> 3             8            20

tbl %>% 
  rap(x = ~filter(mtcars, cyl == cyl_threshold, mpg < mpg_threshold))
#> # A tibble: 3 x 3
#>   cyl_threshold mpg_threshold x                     
#>           <dbl>         <dbl> <list>                
#> 1             4            30 <data.frame [7 × 11]> 
#> 2             6            25 <data.frame [7 × 11]> 
#> 3             8            20 <data.frame [14 × 11]>
```

If the lhs of the formula is empty, `rap()` adds a list column. Otherwise the lhs can be used to specify the type:

``` r
tbl %>% 
  rap(
    x =           ~ filter(mtcars, cyl == cyl_threshold, mpg < mpg_threshold), 
    n = integer() ~ nrow(x)
  )
#> # A tibble: 3 x 4
#>   cyl_threshold mpg_threshold x                          n
#>           <dbl>         <dbl> <list>                 <int>
#> 1             4            30 <data.frame [7 × 11]>      7
#> 2             6            25 <data.frame [7 × 11]>      7
#> 3             8            20 <data.frame [14 × 11]>    14
```

this example is based on this [issue](https://github.com/tidyverse/purrr/issues/280), which has equivalent with `pmap`:

``` r
tbl %>%
  mutate(
    x = pmap(
      .l = list(cyl_threshold, mpg_threshold),
      function(cc, mm) filter(mtcars, cyl == cc, mpg < mm)
    ), 
    n = map_int(x, nrow)
  )
#> # A tibble: 3 x 4
#>   cyl_threshold mpg_threshold x                          n
#>           <dbl>         <dbl> <list>                 <int>
#> 1             4            30 <data.frame [7 × 11]>      7
#> 2             6            25 <data.frame [7 × 11]>      7
#> 3             8            20 <data.frame [14 × 11]>    14
```

wap
---

``` r
library(dplyr)

starwars <- head(starwars)

# creates a list of length 1 integer vectors
# because type not specified
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

# using the lhs to specify the type
starwars %>% 
  wap(integer() ~ length(films))
#> [1] 5 6 7 4 5 3

# list of data frames
starwars %>% 
  wap(~ data.frame(vehicles = length(vehicles), starships = length(starships)))
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

# Specify type as data.frame() row binds them
starwars %>% 
  wap(data.frame() ~ data.frame(vehicles = length(vehicles), starships = length(starships)))
#>   vehicles starships
#> 1        2         2
#> 2        0         0
#> 3        0         0
#> 4        0         1
#> 5        1         0
#> 6        0         0
```

zest\_join
----------

🍋 `zest_join()` is similar to `dplyr::nest_join()` but you control what goes in the nested column. `Z` is `N` but ⤵️.

``` r
tbl <- tibble(cyl_threshold = c(4, 6, 8), mpg_threshold = c(30, 25, 20)) 
tbl %>%
  zest_join(mtcars, data = ~cyl == !!cyl_threshold & mpg < !!mpg_threshold)
#> # A tibble: 3 x 3
#>   cyl_threshold mpg_threshold data                  
#>           <dbl>         <dbl> <list>                
#> 1             4            30 <data.frame [7 × 11]> 
#> 2             6            25 <data.frame [7 × 11]> 
#> 3             8            20 <data.frame [14 × 11]>
```

In the rhs of the formula :

-   `cyl` and `mpg` refer to columns of `mtcars`
-   `cyl_threshold` and `mpg_threshold` refer to the current value from `tbl` because these columns don't exist in mtcars. If you wanted to refer to columns that are present both in mtcars and tbl you would have to unquote the columns in tbl with the unquoting operator, e.g. !!cyl
