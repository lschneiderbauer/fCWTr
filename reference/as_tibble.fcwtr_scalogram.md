# Convert the scalogram matrix to a tibble

Internally, the scalogram resulting from
[`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md) is
represented by a numeric
[`matrix()`](https://rdrr.io/r/base/matrix.html). This method converts
this matrix into a three-columned
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
(two columns to represent the matrix indices, and one column for the
corresponding value). Note that this conversion has a significant run
time cost.

## Usage

``` r
# S3 method for class 'fcwtr_scalogram'
as_tibble(x, ...)
```

## Arguments

- x:

  An "fcwtr_scalogram" object resulting from
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

- ...:

  Additional arguments are ignored.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object representing the scalogram data with four columns:

- time_index:

  An integer index uniquely identifying time slices.

- time:

  The time difference to the first time slice in physical units. The
  time unit is the inverse of the frequency unit chosen by the user for
  the `sample_freq` argument of
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

- freq:

  The frequency in the same units as the `sample_freq` argument of
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

- value:

  The fCWT result for the particular time-frequency combination.

## Details

This method is conceptually identical to
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), it
simply returns a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
instead of a [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Examples

``` r
library(tibble)

fcwt(
  sin((1:5000) * 2 * pi * 440 / 44100),
  x_sample_freq = u(44100, "Hz"),
  n_freqs = 10
) |>
  as_tibble()
#> # A tibble: 3,130 × 4
#>    time_index     time   freq      value
#>         <dbl>      [s]   [Hz]      <dbl>
#>  1          0 0        22050. NA        
#>  2          1 0.000380 22050.  0.000262 
#>  3          2 0.000760 22050.  0.000153 
#>  4          3 0.00114  22050.  0.000108 
#>  5          4 0.00152  22050.  0.0000879
#>  6          5 0.00190  22050.  0.0000762
#>  7          6 0.00228  22050.  0.0000686
#>  8          7 0.00266  22050.  0.0000633
#>  9          8 0.00304  22050.  0.0000595
#> 10          9 0.00342  22050.  0.0000565
#> # ℹ 3,120 more rows
```
