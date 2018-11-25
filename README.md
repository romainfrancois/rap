
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zap

[![Lifecycle
Status](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/)

Experimenting with yet another way to do rowwise operations. It might
never be a thing, and I’m not yet sure this is `quo()`rrect.

This offers `zap()` as an alternative to some versions of:

  - `rowwise()` + `do()`
  - `mutate()` + `pmap()`
  - maybe `purrrlyr` ?
  - probably other approaches

`zap()` works with a lambda, similar to `purrr::map()` but instead of
`.x`, `.y`, `..1`, `..2`, … the lambda can use the column names, which
stand for a single element of the associated vector, in the `[[` sense.

## Installation

You can install `zap` from gitub

``` r
# install.packages("devtools")
detools::install_github("romainfrancois/zap")
```

## Example

``` r
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.0.0.9000      ✔ purrr   0.2.5.9000 
#> ✔ tibble  1.4.99.9005     ✔ dplyr   0.7.99.9000
#> ✔ tidyr   0.8.2.9000      ✔ stringr 1.3.1      
#> ✔ readr   1.1.1           ✔ forcats 0.3.0.9000
#> ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(zap)

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

`zap` attemps to simplify this by iterating over the columns
simultaneously, and exposing them with their name:

``` r
tbl %>% 
  zap(x = ~filter(mtcars, cyl == !!cyl, mpg < !!mpg) )
#> # A tibble: 3 x 3
#>     cyl   mpg x                     
#>   <dbl> <dbl> <list>                
#> 1     4    30 <data.frame [7 × 11]> 
#> 2     6    25 <data.frame [7 × 11]> 
#> 3     8    20 <data.frame [14 × 11]>
```

In `purrr::` fashion, `zap()` has variants to force the type of output,
e.g. `zap_int()`:

``` r
tbl %>% 
  zap(x = ~filter(mtcars, cyl == !!cyl, mpg < !!mpg) ) %>% 
  zap_int( n = ~nrow(x))
#> # A tibble: 3 x 4
#>     cyl   mpg x                          n
#>   <dbl> <dbl> <list>                 <int>
#> 1     4    30 <data.frame [7 × 11]>      7
#> 2     6    25 <data.frame [7 × 11]>      7
#> 3     8    20 <data.frame [14 × 11]>    14
```

## wap

`wap()` is the same but just returns the columns, `wap()` is `zap()` +
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
