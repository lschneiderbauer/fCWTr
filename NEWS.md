# fcwtr 0.2.9000

-   Add more convenience S3 methods: `print()`, `[]`, `as.matrix()`, `rbind()`, `as_tibble()`.

-   New helper: `u()` and `ddu()`: include improved physical unit treatment with the [units](https://r-quantities.github.io/units/) package. Frequency and time parameters can now be "units" objects, created with `u()`. Allow user to adjust `plot()` scales to use arbitrary time/frequency units.

-   `fcwt_batch()`: in some cases, time slices were accidentally discarded, this should now be fixed. In general the batching mechanism should be much more reliable now.

-   `fcwt()`: fix a bug where `fcwt(..., n_freqs = 1, ...)` leads to a faulty data representation.

-   `fcwt()`: added the option to choose between a linear or logarithmic frequency scale.

# fcwtr 0.2.1

-   Initial CRAN submission.
