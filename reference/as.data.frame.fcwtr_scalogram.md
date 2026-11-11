# Convert the scalogram matrix to a data frame

Internally, the scalogram resulting from
[`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md) is
represented by a numeric
[`matrix()`](https://rdrr.io/r/base/matrix.html). This method converts
this matrix into a
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) in long format,
with one row per matrix entry. Note that this conversion has a
significant run time cost.

## Usage

``` r
# S3 method for class 'fcwtr_scalogram'
as.data.frame(x, ...)
```

## Arguments

- x:

  An "fcwtr_scalogram" object resulting from
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

- ...:

  Additional arguments are ignored.

## Value

A [`data.frame()`](https://rdrr.io/r/base/data.frame.html) object
representing the scalogram data with four columns:

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

## Examples

``` r
fcwt(
  sin((1:5000) * 2 * pi * 440 / 44100),
  x_sample_freq = u(44100, "Hz"),
  n_freqs = 10
) |>
  as.data.frame() |>
  head()
#>   time_index           time       freq        value
#> 1          0 0.0000000 [ms] 22050 [Hz]           NA
#> 2          1 0.3628118 [ms] 22050 [Hz] 2.616390e-04
#> 3          2 0.7256236 [ms] 22050 [Hz] 1.527935e-04
#> 4          3 1.0884354 [ms] 22050 [Hz] 1.081133e-04
#> 5          4 1.4512472 [ms] 22050 [Hz] 8.788526e-05
#> 6          5 1.8140590 [ms] 22050 [Hz] 7.622579e-05
```
