# Check for OpenMP support

This function checks if OpenMP support is enabled. It is responsible for
the multithreading capabilities of
[`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).
If OpenMP is not enabled the parameter `n_threads` of
[`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md) is
ignored.

## Usage

``` r
openmp_enabled()
```

## Value

( `TRUE` \| `FALSE` ) Returns `TRUE` if OpenMP support is enabled,
`FALSE` otherwise.

## Details

OpenMP can be used by the underlying fCWT library if

- the fftw library on your platform was compiled with OpenMP support and

- the fCWTr package itself was compiled with OpenMP support.

When using pre-built package binaries (like it is typical when you are
using R on Windows), it depends on the (CRAN) build server whether or
not these conditions are met. They cannot be influenced by the package
author.

If the user is building the R package yourself, the user needs to make
sure that the fftw library on her platform are built with OpenMP
support. The fCWTr package is configured to use OpenMP if fftw-OpenMP
support is available.

## Examples

``` r
openmp_enabled()
#> [1] TRUE
```
