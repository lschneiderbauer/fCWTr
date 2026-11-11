# Convert the scalogram matrix to a tibble

Internally, the scalogram resulting from
[`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md) is
represented by a numeric
[`matrix()`](https://rdrr.io/r/base/matrix.html). This method converts
this matrix into a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
in long format, with one row per matrix entry. Note that this conversion
has a significant run time cost.

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

  An integer index uniquely identifying time slices, starting at 0.

- time:

  The time of the time slice in physical units, relative to the
  beginning of the input signal (time slices that were cut off, e.g. by
  subsetting, are taken into account). The time unit (s, ms, us, or ns)
  is chosen depending on the magnitude of the values, and can be
  converted with
  [`units::set_units()`](https://r-quantities.github.io/units/reference/units.html).

- freq:

  The frequency, in the units chosen by the user for the `freq_begin`
  and `freq_end` arguments of
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
#>    time_index  time   freq      value
#>         <dbl>  [ms]   [Hz]      <dbl>
#>  1          0 0     22050. NA        
#>  2          1 0.363 22050.  0.000262 
#>  3          2 0.726 22050.  0.000153 
#>  4          3 1.09  22050.  0.000108 
#>  5          4 1.45  22050.  0.0000879
#>  6          5 1.81  22050.  0.0000762
#>  7          6 2.18  22050.  0.0000686
#>  8          7 2.54  22050.  0.0000633
#>  9          8 2.90  22050.  0.0000595
#> 10          9 3.27  22050.  0.0000565
#> # ℹ 3,120 more rows
```
