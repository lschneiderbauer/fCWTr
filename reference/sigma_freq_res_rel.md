# Calculates Relative Frequency Resolution

For a given `sigma` this function calculates the expected relative
frequency resolution respectively.

## Usage

``` r
sigma_freq_res_rel(sigma)
```

## Arguments

- sigma:

  The dimensionless positive parameter in
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md)
  controlling the wavelet spread.

## Value

A number `p` representing the relative frequency resolution. I.e. at a
given resolution `f`, the frequency resolution is `f*p`.

## See also

Other sigma:
[`sigma_from_freq_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res.md),
[`sigma_from_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res_rel.md),
[`sigma_from_time_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_time_res.md),
[`sigma_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_res.md)

## Examples

``` r
sigma_freq_res_rel(1)
#> [1] 0.6366198
```
