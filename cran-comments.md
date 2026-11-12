## Resubmission

This is a resubmission, addressing the installation issue with the
single-precision fftw library.

The previous submission (0.4.0) was flagged by the incoming checks for
excessive CPU time (CPU time > elapsed time in examples, tests and
vignettes). This is fixed: the number of OpenMP threads, including the ones
started by fftw, is now limited to `n_threads` (default 2) during the
computation.

In this version I have

-   made configure check explicitly that the *single-precision* fftw3
    library (libfftw3f) can be linked, also when pkg-config knows about
    'fftw3f'. On Fedora, 'fftw-devel' provides the headers and 'fftw3f.pc',
    while the library itself is in 'fftw-libs-single'; if the latter is
    missing, configure now stops with an explanatory error instead of a
    linker error.
-   rewritten the configure error message: it states that the
    *single-precision* fftw3 library is required, and lists the packages to
    install on common platforms (e.g. Fedora/RHEL: 'fftw-devel' and
    'fftw-libs-single'; Debian/Ubuntu: 'libfftw3-dev'; macOS: 'fftw-s'
    recipe or 'brew install fftw').
-   added a fallback in configure: if pkg-config is not available or does
    not know about 'fftw3f', configure tries to link against -lfftw3f
    directly.
-   made configure handle an fftw installation without OpenMP support
    (no libfftw3f_omp): the package then does not use fftw's thread
    functions (previously, loading the package failed in that case).
-   expanded 'SystemRequirements' to explain what 'fftw3f' means (fftw3 with
    single-precision support), to name the corresponding deb/rpm/brew
    packages, and to declare C++17 (matching CXX_STD in Makevars).

I tested the installation in a Fedora (45) container, both without
'fftw-libs-single' (configure fails with the explanatory message) and with
it (installation and tests succeed).

This version (0.4.1) also contains a number of bug fixes and improvements
of the package itself, see NEWS.md.

## Test environments

-   local: Ubuntu 26.04, R 4.5.2
-   GitHub Actions: macOS (release), Windows (release),
    Ubuntu (devel, release, oldrel-1)
-   Fedora 45 container (R from the Fedora repositories)
-   R-hub: linux, macos, macos-arm64, windows, gcc16, clang22, intel, lto,
    ubuntu-clang, gcc-asan, m1-san, rchk, valgrind (R-devel)
-   win-builder (release, R 4.6.2): 1 NOTE (CRAN incoming feasibility, see below)
-   win-builder (devel, R-devel r90874): 1 NOTE (CRAN incoming feasibility, see below)
-   mac-builder (macOS 26 arm64, R 4.6.1 patched): OK

## R CMD check results

0 errors | 0 warnings | 1 note

-   New submission - Package was archived on CRAN
-   Possibly misspelled words in DESCRIPTION: Broek, Lukas
    (these are names of the authors and are spelled correctly).
