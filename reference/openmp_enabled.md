# Check for OpenMP support

This function checks if fCWTr was compiled with OpenMP support. It is
responsible for the multithreading capabilities of
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

OpenMP is used by fCWTr if the compiler supports it. In addition, the
Fourier transforms are multithreaded only if the fftw library on your
platform comes with OpenMP support (`libfftw3f_omp`); otherwise they run
single-threaded.

On Windows, OpenMP support is disabled, since the fftw library provided
by Rtools is compiled without OpenMP support.

When using pre-built package binaries, it depends on the (CRAN) build
server whether or not these conditions are met. When installing the
package from source, the installation script detects whether these
conditions are met.

## Examples

``` r
openmp_enabled()
#> [1] TRUE
```
