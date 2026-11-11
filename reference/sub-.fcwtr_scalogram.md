# Extract parts of a scalogram

Selects a time range and/or a frequency range of a scalogram. The time
offset and the frequency range of the result are adjusted accordingly.

## Usage

``` r
# S3 method for class 'fcwtr_scalogram'
x[i, j]
```

## Arguments

- x:

  An "fcwtr_scalogram" object resulting from
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

- i, j:

  Indices selecting time slices (rows, `i`) and frequencies (columns,
  `j`; ordered from the highest to the lowest frequency). Positive,
  negative, or logical indices are allowed, as long as they select a
  contiguous, ascending range (e.g. `10:20` or `-(1:5)`), since the
  result must again have evenly spaced time and frequency axes. A
  missing index selects everything. Use
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html) for arbitrary
  subsetting.

## Value

Another "fcwtr_scalogram" object that contains only part of the data.

## Examples

``` r
fcwt(
  sin((1:5000) * 2 * pi * 440 / 44100),
  x_sample_freq = u(44100, "Hz"),
  y_sample_freq = u(44100, "Hz"),
  n_freqs = 10
)[1:1000, 2:7]
#> _Scalogram_
#> * (Time/Frequency) dimension:  ( 1000 , 6 )
#> * Sampling rate: 44100 [Hz]
#> * Frequency scale: 190.0211 [Hz] - 9984.067 [Hz], log
#> * Time offset: 0 [s] 
#> * Sigma: 6.283185
#>   o Time resolution at  190.0211 [Hz] :  132.2629 [ms] 
#>   o Time resolution at  9984.067 [Hz] :  2.517285 [ms] 
#>   o Relative frequency resolution:  0.1013212 
#> * Time/frequency matrix summary
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.       NAs 
#> 1.141e-07 7.500e-07 1.505e-06 8.172e-05 6.844e-06 1.578e-03      3044 
```
