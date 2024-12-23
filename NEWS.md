# fcwtr 0.2.9999

-   Include improved physical unit treatment with the 'units' package. Frequency and time parameters can now be passed a bestowed unit.

-   Overhaul `fcwt_batch()`: in some cases, time slices were accidentally discarded, this should now be fixed.

-   Fix a bug where `fcwt(..., n_freqs = 1, ...)` leads to a faulty data representation.

-   `fcwt()`: added the option to choose between a linear or logarithmic frequency scale.

# fcwtr 0.2.1

-   Initial CRAN submission.
