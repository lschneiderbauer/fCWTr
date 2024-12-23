
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fCWTr

<!-- badges: start -->

[![R-CMD-check](https://github.com/lschneiderbauer/fcwtr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lschneiderbauer/fCWTr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fCWTr)](https://CRAN.R-project.org/package=fCWTr)
[![Codecov test
coverage](https://codecov.io/gh/lschneiderbauer/fCWTr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/lschneiderbauer/fcwtr?branch=master)

<!-- badges: end -->

The R package fCWTr is a simple wrapper invoking the [fCWT
library](https://github.com/fastlib/fCWT), a library implementing a
[continuous wavelet
transform](https://en.wikipedia.org/wiki/Continuous_wavelet_transform)
with a Morlet wavelet, utilizing the power of
[fftw](https://www.fftw.org/), a fast fourier transform implementation.

See the original paper by Arts, L.P.A., van den Broek, E.L. The fast
continuous wavelet transformation (fCWT) for real-time, high-quality,
noise-resistant time–frequency analysis. Nat Comput Sci 2, 47–58 (2022).
<https://doi.org/10.1038/s43588-021-00183-z>

## Dependencies

- R \>= 4.1
- [fftw](https://www.fftw.org/) library (used by
  [fCWT](https://github.com/fastlib/fCWT))
- Optional: a CPU/compiler supporting the
  [AVX](https://en.wikipedia.org/wiki/Advanced_Vector_Extensions)
  instruction set
- Optional: OpenMP
  - On Windows, OpenMP support is disabled since rtools decided to
    compile fftw without OpenMP support.
  - On Linux and MacOS the build scripts should automatically detect
    whether OpenMP support is available.

By default, most compiler setups do not make use of AVX to increase
portability of the binary. If you are an R user that has a CPU
supporting AVX and want to make use of it, you might need to manually
enable compiler flags to let R know about it, and install the package
from source (so that it gets compiled on your machine). One way to
enable the flags is to create a file `~/.R/Makevars` with the following
content:

``` bash
CPPFLAGS = -mavx
CXXFLAGS = -mavx
```

## Installation

You can install the latest CRAN release of fCWTr with:

``` r
install.packages("fCWTr")
```

Alternatively, you can install the development version of fCWTr like so
(requiring installed [devtools](https://devtools.r-lib.org/) package):

``` r
devtools::install_github("lschneiderbauer/fCWTr")
```

## Example

This is a basic example that invokes the fCWT library to calculate the
continuous wavelet transform and plot the result.

``` r
library(fCWTr)

# A signal encoded in a numeric vector.
# In this example we use some superimposed sin signals.
signal <- ts_sin_superpos

output <-
  fcwt(
    signal,
    sample_freq = u(44.1, "kHz"),
    freq_begin = u(16, "Hz"),
    freq_end = u(2100, "Hz"),
    n_freqs = 200,
    sigma = 5
  )

# The result is a numeric matrix
dim(output)
#> [1] 6000  200
```

The result can be easily coerced into a data frame:

``` r
head(as.data.frame(output), 10)
#>    time_index               time      freq value
#> 1           1 0.02267574 [1/kHz] 2100 [Hz]    NA
#> 2           2 0.04535147 [1/kHz] 2100 [Hz]    NA
#> 3           3 0.06802721 [1/kHz] 2100 [Hz]    NA
#> 4           4 0.09070295 [1/kHz] 2100 [Hz]    NA
#> 5           5 0.11337868 [1/kHz] 2100 [Hz]    NA
#> 6           6 0.13605442 [1/kHz] 2100 [Hz]    NA
#> 7           7 0.15873016 [1/kHz] 2100 [Hz]    NA
#> 8           8 0.18140590 [1/kHz] 2100 [Hz]    NA
#> 9           9 0.20408163 [1/kHz] 2100 [Hz]    NA
#> 10         10 0.22675737 [1/kHz] 2100 [Hz]    NA
```

We can also directly plot the resulting scalogram:

``` r
plot(output)
```

<img src="man/figures/README-example_plot-1.png" width="100%" />

For long sequences, the required memory can exceed your local memory. In
this case it can be useful to reduce the time resolution of the result
and process the data in batches. This can be done with `fcwt_batch()`.
In case the batch size is not explicitly provided, some heuristics are
used to determine a batch size automatically:

``` r
batch_result <-
  fcwt_batch(
    rep(ts_sin_sin, 10),
    sample_freq = u(44.1, "kHz"),
    freq_begin = u(10, "Hz"),
    freq_end = u(12, "kHz"),
    n_freqs = 200,
    sigma = 4,
    time_resolution = u(10, "ms")
  )

plot(batch_result)
```

<img src="man/figures/README-example_long_df-1.png" width="100%" />

<!-- regenerate with devtools::build_readme() -->
