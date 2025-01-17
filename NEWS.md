# fCWTr 0.3.1

-   fix CRAN issues

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
