# Determine Sigma from a frequency resolution requirement

Setting the correct value for `sigma` in
[`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md)
for a particular use case is important. Sigma controls the wavelet
spread used by the FCWT and so determines the time-/frequency resolution
(depending on a given reference frequency).

## Usage

``` r
sigma_from_freq_res(res, freq)
```

## Arguments

- res:

  The required frequency resolution in frequency units (see
  [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md)).

- freq:

  The reference frequency where the required resolution must hold. Must
  be a quantity with frequency units (see
  [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md)).

## Value

A dimensionless number controlling the FCWT wavelet spread,
corresponding to the `sigma` argument in
[`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

## Details

If you set a high frequency resolution, the time resolution of the
output signal will suffer, and vice versa. Determining the correct
balance for a particular use case can be crucial. This function
determines the value of sigma for a particular frequency resolution
requirement.

## See also

Other sigma:
[`sigma_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_freq_res_rel.md),
[`sigma_from_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res_rel.md),
[`sigma_from_time_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_time_res.md),
[`sigma_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_res.md)

## Examples

``` r
ref_freq <- u(440, "Hz")
sigma <- sigma_from_freq_res(u(7, "Hz"), ref_freq)

sigma_res(sigma, ref_freq)
#> $time
#> 0.3637827 [1/Hz]
#> 
#> $freq
#> 7 [Hz]
#> 
```
