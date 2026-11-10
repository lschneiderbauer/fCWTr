# Convert the scalogram matrix to a data frame

Internally, the scalogram resulting from
[`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md) is
represented by a numeric
[`matrix()`](https://rdrr.io/r/base/matrix.html). This method converts
this matrix into a three-columned
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) (two columns to
represent the matrix indices, and one column for the corresponding
value). Note that this conversion has a significant run time cost.

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

## Examples

``` r
fcwt(
  sin((1:5000) * 2 * pi * 440 / 44100),
  x_sample_freq = u(44100, "Hz"),
  n_freqs = 10
) |>
  as.data.frame() |>
  head()
#>   time_index             time       freq        value
#> 1          0 0.0000000000 [s] 22050 [Hz]           NA
#> 2          1 0.0003628118 [s] 22050 [Hz] 2.616390e-04
#> 3          2 0.0007256236 [s] 22050 [Hz] 1.527935e-04
#> 4          3 0.0010884354 [s] 22050 [Hz] 1.081133e-04
#> 5          4 0.0014512472 [s] 22050 [Hz] 8.788526e-05
#> 6          5 0.0018140590 [s] 22050 [Hz] 7.622579e-05
```
