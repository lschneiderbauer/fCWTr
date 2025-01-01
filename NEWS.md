# fCWTr 0.2.9000

-   `fcwt():`

    -   fix a bug where `fcwt(..., n_freqs = 1, ...)` leads to a faulty data representation.

    -   add `freq_scale` argument to choose between a linear or logarithmic frequency scale.

    -   allow (and prefer) `sample_freq`, `freq_begin`, `freq_end` arguments to be passed as a dimensionful quantities, via `u()`.

-   `fcwt_batch()`:

    -   allow (and prefer) `sample_freq`, `freq_begin`, `freq_end` and `time_resolution` arguments to be passed as a dimensionful quantities, via `u()`.

    -   in some cases, the averaging procedure together with the batching could lead to accidentally dropped or duplicated time slices: the mechanism has been overhauled, every slice should be perfectly aligned now, even when averaged.

-   new unit helper functions: `u()` and `du()`: include improved physical unit treatment with the [units](https://r-quantities.github.io/units/) package. Frequency and time parameters can now be "units" objects, created with `u()`. Allow user to adjust `plot()` scales to use arbitrary time/frequency units.

-   new helper functions to determine the correct value of sigma for a given use case: `sigma_resolution()`, `sigma_from_time_resolution()`, `sigma_from_frequency_resolution()`.

-   add more convenience S3 methods: `print()`, `[]`, `as.matrix()`, `rbind()`, `as_tibble()`.

-   `openmp_enabled()`/`avx_enabled()` new functions to check for system OpenMP/AVX support.

-   add a [package logo](https://lschneiderbauer.github.io/fCWTr/logo.svg)

# fCWTr 0.2.1

-   Initial CRAN submission.
