AC_INIT([fCWTr],[0.2.9000])


# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS` `"${R_HOME}/bin/R" CMD config CXXPICFLAGS`
CPPFLAGS="`"${R_HOME}/bin/R" CMD config CPPFLAGS`"
LDFLAGS="`"${R_HOME}/bin/R" CMD config LDFLAGS`"
AC_LANG(C++)
AC_PROG_CPP
AC_PATH_PROG([PKG_CONFIG], [pkg-config])
AC_OPENMP

AC_MSG_CHECKING([for pkg-config])

AS_IF(["$PKG_CONFIG" --version], , [AC_MSG_RESULT([no])
  AC_MSG_ERROR([Provided flags don't work and pkg-config is not present.])])

AC_MSG_CHECKING([whether pkg-config knows about fftw3f])

AS_IF([$PKG_CONFIG fftw3f], [AC_MSG_RESULT(yes)], [AC_MSG_RESULT([no])
     AC_MSG_ERROR([Install libfftw3-dev or equivalent first])])

CPPFLAGS="${CPPFLAGS} `$PKG_CONFIG --cflags fftw3f`"
LIBS="${LIBS} `$PKG_CONFIG --libs fftw3f`"

# needs to be after pkg-config checks
# to have correct paths already

# Check for fftw3.h header
AC_CHECK_HEADER([fftw3.h], [
  AC_DEFINE([HAVE_FFTW3_H], [1], [Define if you have the fftw3.h header file])
], [
  AC_MSG_ERROR([fftw3.h header file not found in: $CPPFLAGS])
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
