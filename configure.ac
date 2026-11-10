AC_INIT([fCWTr],[0.3.1])


# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
# use the C++17 compiler, matching CXX_STD = CXX17 in src/Makevars
# (R-exts: SHLIB_OPENMP_CXXFLAGS/default flags need not apply to it)
CXX=`"${R_HOME}/bin/R" CMD config CXX17`
CXXFLAGS="`"${R_HOME}/bin/R" CMD config CXX17FLAGS` `"${R_HOME}/bin/R" CMD config CXX17PICFLAGS`"
if test -z "${CXX}"; then
  AC_MSG_ERROR([No C++17 compiler is available (R CMD config CXX17 is empty).])
fi
CPPFLAGS="`"${R_HOME}/bin/R" CMD config CPPFLAGS`"
LDFLAGS="`"${R_HOME}/bin/R" CMD config LDFLAGS`"
AC_LANG(C++)
AC_PROG_CPP
AC_PATH_PROG([PKG_CONFIG], [pkg-config])

AC_OPENMP

FFTWF_MISSING_MSG="
  --------------------------------------------------------------
  fCWTr requires the fftw3 library built with SINGLE-PRECISION
  support (libfftw3f, i.e. the 'fftwf_*' API), including headers.
  A double-precision-only fftw installation is not sufficient.

  Install it e.g. via:
    Debian/Ubuntu:  libfftw3-dev   (contains libfftw3f)
    Fedora/RHEL:    fftw-devel and fftw-libs-single
    openSUSE:       fftw3-devel
    Arch Linux:     fftw
    macOS:          'fftw-s' (CRAN macOS recipe) or 'brew install fftw'
    Windows:        provided by Rtools

  If you built fftw yourself, configure it with --enable-float.
  If it is installed in a non-standard location, make sure
  pkg-config finds 'fftw3f.pc' (set PKG_CONFIG_PATH), or set
  CPPFLAGS/LDFLAGS (e.g. in ~/.R/Makevars) accordingly.
  --------------------------------------------------------------
"

have_fftw3f_pc=no
AS_IF([test -n "$PKG_CONFIG"], [
  AC_MSG_CHECKING([whether pkg-config knows about fftw3f (single-precision fftw3)])
  AS_IF(["$PKG_CONFIG" --exists fftw3f],
    [AC_MSG_RESULT([yes])
     have_fftw3f_pc=yes
     CPPFLAGS="${CPPFLAGS} `$PKG_CONFIG --cflags fftw3f`"
     LIBS="`$PKG_CONFIG --libs fftw3f` ${LIBS}"],
    [AC_MSG_RESULT([no])])
])

# Fallback (no pkg-config or no fftw3f.pc): try linking directly.
AS_IF([test "$have_fftw3f_pc" = no], [
  AC_SEARCH_LIBS([fftwf_plan_dft_r2c_1d], [fftw3f], ,
    [AC_MSG_ERROR([single-precision fftw3 library (libfftw3f) not found.
$FFTWF_MISSING_MSG])])
])

# needs to be after pkg-config checks
# to have correct paths already

# Check for fftw3.h header
AC_CHECK_HEADER([fftw3.h], [
  AC_DEFINE([HAVE_FFTW3_H], [1], [Define if you have the fftw3.h header file])
], [
  AC_MSG_ERROR([fftw3.h header file not found (CPPFLAGS: $CPPFLAGS).
$FFTWF_MISSING_MSG])
])

# there are no pkg-config files for fftw3-omp,
# so check like this.
# have_fftw_omp=no
AC_SEARCH_LIBS(
 fftwf_plan_with_nthreads,
 fftw3f_omp,
 [have_fftw_omp=yes],
 [AC_MSG_RESULT([The fftw3-omp support not found.])]
)


# Write the flags into the src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${CPPFLAGS} ${OPENMP_CFLAGS}"])
AC_SUBST([PKG_CXXFLAGS], ["${CXXFLAGS} ${OPENMP_CXXFLAGS}"])
AC_SUBST([PKG_LDFLAGS], ["${LDFLAGS} ${OPENMP_CXXFLAGS}"])
AC_SUBST([PKG_LIBS], ["${LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${PKG_CPPFLAGS}
    cxxflags: ${PKG_CXXFLAGS}
    ldflags:  ${PKG_LDFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"
