AC_INIT([fcwtr],[0.2.0])

dnl Select an optional include path, from a configure option
dnl or from an environment variable.
AC_ARG_WITH([includedir],
            AS_HELP_STRING([--includedir=INCLUDE_PATH],[the location of header files]),
            [includedir=$withval])
EXT_CPPFLAGS=""
if test [ -n "$includedir" ] ; then
  EXT_CPPFLAGS="-I${includedir}"
else
  if test [ -n "${CPPFLAGS}" ] ; then
     EXT_CPPFLAGS="-I${CPPFLAGS}"
  fi
fi

dnl ditto for a library path
AC_ARG_WITH([libdir],
            AS_HELP_STRING([--libdir=LIB_PATH],[the location of libraries]),
            [libdir=$withval])
EXT_LDFLAGS=""
if test [ -n "$libdir" ] ; then
   EXT_LDFLAGS="-L${libdir}"
else
  if test [ -n "$LDFLAGS}" ] ; then
     EXT_LDFLAGS="-L${LDFLAGS}"
  fi
fi

# extra check for macos brew
AC_CHECK_PROG(BREW,brew,yes)
BREW_CPPFLAGS=""
BREW_LDFLAGS=""
if test "x${brew}" = xyes; then
  BREW_CPPFLAGS="`brew --prefix`/include"
  BREW_LDFLAGS="`brew --prefix`/lib"
fi

# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS="`"${R_HOME}/bin/R" CMD config CPPFLAGS` ${EXT_CPPFLAGS} ${BREW_CPPFLAGS}"
LDFLAGS="`"${R_HOME}/bin/R" CMD config LDFLAGS` ${EXT_LDFLAGS} ${BREW_LDFLAGS}"
AC_LANG(C++)
AC_PROG_CPP



# Check for fftw3.h header
AC_CHECK_HEADER([fftw3.h], [
  AC_DEFINE([HAVE_FFTW3_H], [1], [Define if you have the fftw3.h header file])
], [
  AC_MSG_ERROR([fftw3.h header file not found. cppflags: $CPPFLAGS])
])

have_fftw=no
AC_SEARCH_LIBS(
  fftwf_free,
  fftw3f,
  [have_fftw=yes],
  [AC_MSG_ERROR([The fftw3 library is required.])]
)

# Check for omp.h header
AC_CHECK_HEADER([omp.h], [
  AC_DEFINE([HAVE_OMP_H], [1], [Define if you have the omp.h header file])
], [
  AC_DEFINE([HAVE_OMP_H], [0], [Define if you have the omp.h header file])
])

have_fftw_omp=no
AC_SEARCH_LIBS(
  fftwf_plan_with_nthreads,
  fftw3f_omp,
  [have_fftw_omp=yes],
  [AC_MSG_RESULT([The fftw3-omp support not found.])]
)

AC_OPENMP(
  [AC_SEARCH_LIBS(fftwf_plan_with_nthreads, fftw3f_omp, [have_fftwf_omp=yes],
    [AC_MSG_ERROR([OMP-support in fftw3 library is required if OpenMP support is enabled.])]
  )]
)

# Write the flags into the src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS} ${FFTW_CPPFLAGS} ${FFTW_OMP_CPPFLAGS} ${OPENMP_CPPFLAGS}"])
AC_SUBST([PKG_CXXFLAGS], ["${PKG_CXXFLAGS} ${FFTW_CXXFLAGS} ${FFTW_OMP_CXXFLAGS} ${OPENMP_CXXFLAGS}"])
AC_SUBST([PKG_LIBS], ["${LIBS} ${PKG_LIBS} ${FFTW_LIBS} ${FFTW_OMP_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${CPPFLAGS} ${PKG_CPPFLAGS}
    cxxflags: ${CXXFLAGS} ${PKG_CXXFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"
