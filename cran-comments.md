## Resubmission

This is a resubmission. In this version I have

-   added the macos recipe 'fftw-s' to 'SystemRequirements'
-   rewrote the configure script to use pkg-config in order to test for fftw library with single precision support.

## R CMD check results

0 errors \| 0 warnings \| 2 notes

-   CRAN incoming feasability

    -   New submission - Package was archived on CRAN

    -   Note: "Possibly misspelled words in DESCRIPTION: Broek (17:32) Lukas (16:57)."

        -   *Remark*: I double-checked the spelling and it is correct.

    -   X-CRAN-Comment: Archived on 2024-07-01 as issues were not corrected in time.

    -   Incorrect 'SystemRequirements' (needs single-precision 'fftw' and does not test for it).

-   checking for future file timestamps

    -   unable to verify current time

        -   *Remark*: apparently the web service in use is not reachable, the problem seems not to be on my end.
