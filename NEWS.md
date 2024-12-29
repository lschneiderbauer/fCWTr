# fcwtr 0.2.9000

-   `fcwt()`

    -   fix a bug where `fcwt(..., n_freqs = 1, ...)` leads to a faulty data representation.

    -   add `freq_scale` argument to choose between a linear or logarithmic frequency scale.

-   `fcwt_batch()`: in some cases, time slices were accidentally discarded, this should now be fixed. In general the batching mechanism should be much more reliable now.

-   add more convenience S3 methods: `print()`, `[]`, `as.matrix()`, `rbind()`, `as_tibble()`.

-   new unit helper functions: `u()` and `du()`: include improved physical unit treatment with the [units](https://r-quantities.github.io/units/) package. Frequency and time parameters can now be "units" objects, created with `u()`. Allow user to adjust `plot()` scales to use arbitrary time/frequency units.

-   add a package logo

# fcwtr 0.2.1

-   Initial CRAN submission.
