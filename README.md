
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fcwtr

<!-- badges: start -->

[![R-CMD-check](https://github.com/lschneiderbauer/fcwtr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lschneiderbauer/fcwtr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fcwtr)](https://CRAN.R-project.org/package=fcwtr)

<!-- badges: end -->

The R package fcwtr is a simple wrapper invoking the [fcwt
library](https://github.com/fastlib/fCWT), a library implementing a
[continuous wavelet
transform](https://en.wikipedia.org/wiki/Continuous_wavelet_transform)
with a Morlet wavelet, utilizing the power of
[fftw](https://www.fftw.org/), a fast fourier transform implementation.

## Dependencies

- R \>= 4.1
- [fftw](https://www.fftw.org/) library (used by fcwt)
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

You can install the development version of fcwtr like so (requiring
installed [devtools](https://devtools.r-lib.org/) package):

``` r
devtools::install_github("lschneiderbauer/fcwtr")
```

## Example

This is a basic example that invokes the fcwt library to calculate the
continuous wavelet transform and plot the result.

``` r
library(fcwtr)

# You are given some signal encoded in a numeric vector.
# In this example we use some superimposed sin signals.
signal <- ts_sin_superpos

output <-
  fcwt(
    signal,
    sample_freq = 44100,
    freq_begin = 16,
    freq_end = 2100,
    n_freqs = 200,
    sigma = 5
  )

# In this case the result is a matrix,
# which can be used for further processing.
dim(output)
#> [1] 6000  200
```

Conversion functions to a meaningful data frame is also provided:

``` r
head(as.data.frame(output), 10)
#>    time_ind freq value         time
#> 1         0 2100    NA 0.000000e+00
#> 2         1 2100    NA 2.267574e-05
#> 3         2 2100    NA 4.535147e-05
#> 4         3 2100    NA 6.802721e-05
#> 5         4 2100    NA 9.070295e-05
#> 6         5 2100    NA 1.133787e-04
#> 7         6 2100    NA 1.360544e-04
#> 8         7 2100    NA 1.587302e-04
#> 9         8 2100    NA 1.814059e-04
#> 10        9 2100    NA 2.040816e-04
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
    sample_freq = 44100,
    freq_begin = 10,
    freq_end = 12000,
    n_freqs = 200,
    sigma = 4,
    time_resolution = 1 / 44100
  )

plot(batch_result)
```

<img src="man/figures/README-example_long_df-1.png" width="100%" />

<!-- regenerate with devtools::build_readme() -->
