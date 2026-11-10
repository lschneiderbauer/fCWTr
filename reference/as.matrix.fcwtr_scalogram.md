# Extract the data matrix from a scalogram

Strips attributes and class from a scalogram object to retrieve a pure
matrix.

## Usage

``` r
# S3 method for class 'fcwtr_scalogram'
as.matrix(x, ...)
```

## Arguments

- x:

  An "fcwtr_scalogram" object resulting from
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

- ...:

  additional arguments to be passed to or from methods.

## Value

A two dimensional numeric vector, inheriting the class "matrix".

## Examples

``` r
options(max.print = 10)

fcwt(
  sin((1:5000) * 2 * pi * 440 / 44100),
  x_sample_freq = u(44100, "Hz"),
  n_freqs = 10
) |>
  as.matrix()
#>                [,1]         [,2]         [,3]         [,4]         [,5]
#>   [1,]           NA           NA           NA           NA           NA
#>               [,6] [,7] [,8] [,9] [,10]
#>   [1,]          NA   NA   NA   NA    NA
#>  [ reached 'max' / getOption("max.print") -- omitted 312 rows ]
```
