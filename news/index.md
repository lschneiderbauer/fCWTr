# Changelog

## fCWTr 0.3.1

- fix CRAN issues

- bug fixes in the transform (results change numerically):

  - values of a frequency band no longer leak into the following
    (lower-frequency) bands.

  - for odd signal lengths, every other frequency band was shifted by
    one sample in time.

  - averaging over time windows (`y_sample_freq` lower than
    `x_sample_freq`) under-estimated the mean.

  - `n_freqs = 1` with `freq_scale = "log"` returned garbage.

  - fix undefined behavior (memory alignment) that could lead to crashes
    on some CPUs.

- [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md):
  `n_threads` must be at least 1.

## fCWTr 0.3.0

- `fcwt():`

  - fix a bug where `fcwt(..., n_freqs = 1, ...)` leads to a faulty data
    representation.

  - add `freq_scale` argument to choose between a linear or logarithmic
    frequency scale.

  - allow (and prefer) `sample_freq`, `freq_begin`, `freq_end` arguments
    to be passed as a dimensionful quantities, via
    [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md).

  - minor argument name changes.

  - add new argument `y_sample_freq` which allows to specify the
    sampling rate of the output signal.

  - add meaningful default values for most of the arguments.

- [`fcwt_batch()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt_batch.md):

  - allow (and prefer) `sample_freq`, `freq_begin`, `freq_end` and
    `time_resolution` arguments to be passed as a dimensionful
    quantities, via
    [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md).

  - in some cases, the averaging procedure together with the batching
    could lead to accidentally dropped or duplicated time slices: the
    mechanism has been overhauled, every slice should be perfectly
    aligned now, even when averaged.

  - shared argument with
    [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md):
    same changes as in
    [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

- new unit helper functions:
  [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md) and
  [`du()`](https://lschneiderbauer.github.io/fCWTr/reference/du.md):
  include improved physical unit treatment with the
  [units](https://r-quantities.github.io/units/) package. Frequency and
  time parameters can now be “units” objects, created with
  [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md). Allow
  user to adjust
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) scales to use
  arbitrary time/frequency units.

- new helper functions to determine the correct value of sigma for a
  given use case:
  [`sigma_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_res.md),
  [`sigma_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_freq_res_rel.md),
  [`sigma_from_time_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_time_res.md),
  [`sigma_from_freq_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res.md),
  [`sigma_from_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res_rel.md).

- add more convenience S3 methods:
  [`print()`](https://rdrr.io/r/base/print.html), `[]`,
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html),
  [`rbind()`](https://rdrr.io/r/base/cbind.html),
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).

- [`openmp_enabled()`](https://lschneiderbauer.github.io/fCWTr/reference/openmp_enabled.md)/[`avx_enabled()`](https://lschneiderbauer.github.io/fCWTr/reference/avx_enabled.md)
  new functions to check for system OpenMP/AVX support.

- new vignette about controlling the time-frequency resolution
  [`vignette("sigma")`](https://lschneiderbauer.github.io/fCWTr/articles/sigma.md).

- add a [package logo](https://lschneiderbauer.github.io/fCWTr/logo.svg)

## fCWTr 0.2.1

CRAN release: 2024-06-17

- Initial CRAN submission.
