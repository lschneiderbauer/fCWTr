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

  - the sampling frequency `x_sample_freq` was truncated to an integer
    (in Hz), so non-integer sampling frequencies lead to wrong
    frequencies, and sampling frequencies below 1 Hz did not work at
    all.

  - the time axis of the result was wrong if
    `x_sample_freq / y_sample_freq` is not an integer: the result now
    records the actual output sampling rate (which can be slightly
    higher than `y_sample_freq`).

  - with `freq_scale = "linear"`, the frequencies actually computed did
    not match the reported ones (only the lowest one did): they now
    range from `freq_begin` to `freq_end` (both inclusive), as for
    `freq_scale = "log"`.

- [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md):
  `n_threads` must be at least 1.

- [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md)
  no longer changes the global OpenMP thread setting of the R session.

- very large inputs (more than 2^31 output values) are now supported
  (previously: integer overflow), and failing memory allocations raise
  an error instead of leading to undefined behavior.

- [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md)
  needs considerably less memory: the complex transform is no longer
  stored as a whole, but processed frequency by frequency (e.g. about 4x
  less peak memory when averaging over time windows).

- [`fcwt_batch()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt_batch.md):

  - the defaults of `sigma` and `n_freqs` are now the same as for
    [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md)
    (previously `sigma = 1`, and `n_freqs` was a third lower).

  - the default `max_batch_size` is now chosen such that processing a
    batch needs about 500 MB of memory, taking into account the reduced
    memory usage of
    [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md)
    and the averaging over time windows (`y_sample_freq`). Previously,
    the batches were often unnecessarily small, which lead to large
    losses from boundary effects.

- subsetting a scalogram with `[`:

  - selecting frequencies (columns) now adjusts the frequency range;
    previously, the frequency labels were wrong.

  - negative and logical indices now adjust the time offset correctly.
    Indices that do not select a contiguous range raise an error (they
    cannot be represented by evenly spaced axes), use
    [`as.matrix()`](https://rdrr.io/r/base/matrix.html) instead.

- times are now given in a unit that suits their magnitude (s, ms, us,
  or ns): the `time` column of
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) /
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
  the time offset and time resolutions in
  [`print()`](https://rdrr.io/r/base/print.html), and the time axis of
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) /
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  (whose `time_unit` argument now defaults to `NULL`, i.e. automatic).

- documentation fixes and improvements.

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
