## Resubmission

This is a resubmission. In this version I have

-   rewritten the configure error message: it now explicitly states that the
    *single-precision* fftw3 library (libfftw3f) is required, and lists the
    packages to install on common platforms (e.g. on Fedora: 'fftw-devel' and
    'fftw-libs-single'; on Debian/Ubuntu: 'libfftw3-dev'; macOS recipe 'fftw-s').
-   expanded 'SystemRequirements' to explain what 'fftw3f' means
    (fftw3 with single-precision support) and to name the corresponding
    deb/rpm/brew packages.
-   added a fallback in configure: if pkg-config is unavailable or does not
    know about 'fftw3f', configure now tries to link against -lfftw3f directly
    before failing.
-   fixed a quoting bug in configure that prevented CXXFLAGS from being set.

## R CMD check results

0 errors | 0 warnings | 1 note

-   New submission - Package was archived on CRAN
-   Possibly misspelled words in DESCRIPTION: Broek, Lukas
    (these are names of the authors and are spelled correctly).
