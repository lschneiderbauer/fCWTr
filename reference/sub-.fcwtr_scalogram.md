# Extract parts of a scalogram

Extract parts of a scalogram

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

  Indices corresponding to time slices of the spectogram which specify
  elements to extract. Indices are numeric vectors or empty (missing) or
  NULL. Numeric values are coerced to integer or whole numbers as by
  as.integer or for large values by
  [`trunc()`](https://rdrr.io/r/base/Round.html) (and hence truncated
  towards zero).

  The time offset of the scalogram is adjusted to correspond to
  `min(i)`.

  For \[-indexing only: i, j, ... can be logical vectors, indicating
  elements/slices to select. Such vectors are recycled if necessary to
  match the corresponding extent. i, j, ... can also be negative
  integers, indicating elements/slices to leave out of the selection.
  When indexing arrays by \[ a single argument i can be a matrix with as
  many columns as there are dimensions of x; the result is then a vector
  with elements corresponding to the sets of indices in each row of i.
  An index value of NULL is treated as if it were integer(0).

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
#> * Frequency scale: 17.64 [Hz] - 22050 [Hz], log
#> * Time offset: 0 [s] 
#> * Sigma: 6.283185
#>   o Time resolution at  17.64 [Hz] :  1.424759 [1/Hz] 
#>   o Time resolution at  22050 [Hz] :  0.001139807 [1/Hz] 
#>   o Relative frequency resolution:  0.1013212 
#> * Time/frequency matrix summary
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.       NAs 
#> 7.432e-06 3.889e-05 4.135e-05 1.139e-04 5.253e-05 1.624e-03      3044 
```
