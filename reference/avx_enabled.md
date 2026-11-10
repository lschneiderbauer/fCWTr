# Check for AVX instruction set support

This function checks if fCWTr was compiled with AVX instruction set
support. AVX instructions need to be supported by the users' CPU in
order for this to work.

## Usage

``` r
avx_enabled()
```

## Value

( `TRUE` \| `FALSE` ) Returns `TRUE` if AVX support is enabled, `FALSE`
otherwise.

## Details

By default, most compiler setups do not make use of AVX to increase
portability of the binary. If you are an R user that has a CPU
supporting AVX and want to make use of it, you might need to manually
enable compiler flags to let R know about it, and install the package
from source (so that it gets compiled on your machine).

## Examples

``` r
avx_enabled()
#> [1] TRUE
```
