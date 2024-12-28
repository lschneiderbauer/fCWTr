## New submission - Package was archived on CRAN

This is a resubmission (after package being archived). In this version I have:

-   Changed 'SystemRequirements' to directly refer to single precision 'fftw3f' and added 'fftw3f_omp'. I also indicated the OpenMP requirement as optional. I removed version requirements (since '3' implies version 3).
-   The comment "[The package] does not test for [fftwf]" seems incorrect to me: The file ./configure.ac, line #40, contains a paragraph that tests for single precision functions. The configuration fails with a descriptive error message "The fftw3f library is required." in case it does not find a fitting library. I now added additional information to this error message to make it more clear what is missing. I searched the "Writing R Extensions" documentation for additional mandatory places, where a check should occur, to no avail.
-   I also added other features, unrelated to the comments, since last submission/archiving.

## R CMD check results

0 errors \| 0 warnings \| 2 notes

-   New submission - Package was archived on CRAN

-   Note: "Possibly misspelled words in DESCRIPTION: Broek (17:32) Lukas (16:57)."

    -   Remark: I double-checked the spelling and it is correct.

-   X-CRAN-Comment: Archived on 2024-07-01 as issues were not corrected in time.

-   Incorrect 'SystemRequirements' (needs single-precision 'fftw' and does not test for it).
