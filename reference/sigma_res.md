# Calculates Time/Frequency Resolution

For a given `sigma` and reference frequency `freq` this function
calculates the expected time and frequency resolution respectively.

## Usage

``` r
sigma_res(sigma, freq)
```

## Arguments

- sigma:

  The dimensionless positive parameter in
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md)
  controlling the wavelet spread.

- freq:

  The reference frequency where the frequency and time resolution are
  calculated. Must be a quantity with frequency units (see
  [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md)).

## Value

A named list with two elements: "time" and "freq", in time and frequency
units. They obey the property \\time \times freq = \frac{8}{\pi}\\.

## Details

Note that this resolution depends on the reference frequency, since the
FCWT scales the wavelet spread depending on the frequency in a natural
way. (in contrast to a simple fixed-windowed Fourier Transform).

## See also

Other sigma:
[`sigma_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_freq_res_rel.md),
[`sigma_from_freq_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res.md),
[`sigma_from_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res_rel.md),
[`sigma_from_time_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_time_res.md)

## Examples

``` r
sigma_res(1, u(440, "Hz"))
#> $time
#> 0.009090909 [1/Hz]
#> 
#> $freq
#> 280.1127 [Hz]
#> 
```
