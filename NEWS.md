# fCWTr 0.4.1

## Breaking changes

-   Due to the bug fixes below, the results of `fcwt()` and `fcwt_batch()` change numerically, in some cases considerably (in particular when averaging over time windows, for odd signal lengths, and for `freq_scale = "linear"`).

-   `fcwt_batch()`: the defaults of `sigma` and `n_freqs` are now the same as for `fcwt()` (previously `sigma = 1`, and `n_freqs` was a third lower).

-   `[` (scalogram subsetting): indices that do not select a contiguous range now raise an error, since the result cannot be represented by evenly spaced time and frequency axes. Use `as.matrix()` for arbitrary subsetting.

-   `fcwt()`: `n_threads` must be at least 1.

-   R \>= 4.2 is required.

## Bug fixes

### `fcwt()` (and hence `fcwt_batch()`)

-   Values of a frequency band no longer leak into the following (lower-frequency) bands.

-   For odd signal lengths, every other frequency band was shifted by one sample in time.

-   Averaging over time windows (`y_sample_freq` lower than `x_sample_freq`) under-estimated the mean.

-   With `freq_scale = "linear"`, the frequencies actually computed did not match the reported ones (only the lowest one did): they now range from `freq_begin` to `freq_end` (both inclusive), as for `freq_scale = "log"`.

-   `n_freqs = 1` with `freq_scale = "log"` returned garbage (and could crash on CPUs with AVX support).

-   The sampling frequency `x_sample_freq` was truncated to an integer (in Hz), so non-integer sampling frequencies lead to wrong frequencies, and sampling frequencies below 1 Hz did not work at all.

-   The time axis of the result was wrong if `x_sample_freq / y_sample_freq` is not an integer: the result now records the actual output sampling rate (which can be slightly higher than `y_sample_freq`).

-   Fix undefined behavior (memory alignment) that could lead to crashes on some platforms.

-   Very large inputs (more than 2^31 output values) lead to an integer overflow, failing memory allocations to undefined behavior. The former are now supported, the latter raise an error.

### `fcwt_batch()`

-   `n_freqs = 1` did not work.

### Scalogram methods

-   `[`: selecting frequencies (columns) now adjusts the frequency range (previously, the frequency labels were wrong), and negative and logical indices now adjust the time offset correctly.

-   `plot()` / `autoplot()`: the axis labels show the units again with ggplot2 \>= 4.0.

## New features and improvements

### `fcwt()`

-   Considerably less memory is needed: the complex transform is no longer stored as a whole, but processed frequency by frequency (e.g. about 4x less peak memory when averaging over time windows).

-   With `rm_coi = TRUE` (the default), the boundary values are now removed with much less memory overhead (peak memory about 2.5x instead of 6x the size of the result).

-   The global OpenMP thread setting of the R session is no longer changed.

### `fcwt_batch()`

-   The default `max_batch_size` is now chosen such that processing a batch needs about 500 MB of memory, taking into account the reduced memory usage of `fcwt()` and the averaging over time windows (`y_sample_freq`). Previously, the batches were often unnecessarily small, which lead to large losses from boundary effects.

### Scalogram methods

-   Times are now given in a unit that suits their magnitude (s, ms, us, or ns): the `time` column of `as.data.frame()` / `as_tibble()`, the time offset and time resolutions in `print()`, and the time axis of `plot()` / `autoplot()` (whose `time_unit` argument now defaults to `NULL`, i.e. automatic).

### Installation

-   The installation now explicitly checks for the single-precision fftw library (falling back to a direct link test if pkg-config does not find it), and gives clear instructions on which packages to install if it is missing. `SystemRequirements` describes the dependencies in more detail (fixes CRAN issues).

-   The installation no longer fails if the compiler supports OpenMP, but the fftw library comes without OpenMP support (`libfftw3f_omp`): the Fourier transforms then run single-threaded.

### Documentation

-   Many documentation fixes and improvements, and a package help page (`?fCWTr`).

# fCWTr 0.3.0

-   `fcwt():`

    -   fix a bug where `fcwt(..., n_freqs = 1, ...)` leads to a faulty data representation.

    -   add `freq_scale` argument to choose between a linear or logarithmic frequency scale.

    -   allow (and prefer) `sample_freq`, `freq_begin`, `freq_end` arguments to be passed as a dimensionful quantities, via `u()`.

    -   minor argument name changes.

    -   add new argument `y_sample_freq` which allows to specify the sampling rate of the output signal.

    -   add meaningful default values for most of the arguments.

-   `fcwt_batch()`:

    -   allow (and prefer) `sample_freq`, `freq_begin`, `freq_end` and `time_resolution` arguments to be passed as a dimensionful quantities, via `u()`.

    -   in some cases, the averaging procedure together with the batching could lead to accidentally dropped or duplicated time slices: the mechanism has been overhauled, every slice should be perfectly aligned now, even when averaged.

    -   shared argument with `fcwt()`: same changes as in `fcwt()`.

-   new unit helper functions: `u()` and `du()`: include improved physical unit treatment with the [units](https://r-quantities.github.io/units/) package. Frequency and time parameters can now be "units" objects, created with `u()`. Allow user to adjust `plot()` scales to use arbitrary time/frequency units.

-   new helper functions to determine the correct value of sigma for a given use case: `sigma_res()`, `sigma_freq_res_rel()`, `sigma_from_time_res()`, `sigma_from_freq_res()`, `sigma_from_freq_res_rel()`.

-   add more convenience S3 methods: `print()`, `[]`, `as.matrix()`, `rbind()`, `tibble::as_tibble()`.

-   `openmp_enabled()`/`avx_enabled()` new functions to check for system OpenMP/AVX support.

-   new vignette about controlling the time-frequency resolution `vignette("sigma")`.

-   add a [package logo](https://lschneiderbauer.github.io/fCWTr/logo.svg)

# fCWTr 0.2.1

-   Initial CRAN submission.
