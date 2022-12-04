 AC_INIT(fcwtr, 0.1.0)

# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)
AC_PROG_CPP
AC_OPENMP # use openmp if possible

have_fftw=no
FFTW_CXXFLAGS=""
FFTW_LIBS="-lfftw3f -lfftw3f_threads -lfftw3f_omp"

PKG_PROG_PKG_CONFIG

if test [ -n "$PKG_CONFIG" ] ; then
  AC_MSG_CHECKING([pkg-config for fftw3f])
  if $PKG_CONFIG --exists fftw3f; then
    have_fftw=yes
    FFTW_CXXFLAGS=`"${PKG_CONFIG}" --cflags fftw3f`
    FFTW_LIBS=`"${PKG_CONFIG}" --libs fftw3f`
  fi
  AC_MSG_RESULT([${have_fftw}])

  AC_MSG_CHECKING([pkg-config for fftw3f_threads])
  have_fftwf_threads=no
  if $PKG_CONFIG --exists fftw3f_threads; then
    have_fftwf_threads=yes
    FFTW_THREADS_CXXFLAGS=`"${PKG_CONFIG}" --cflags fftw3f_threads`
    FFTW_THREADS_LIBS=`"${PKG_CONFIG}" --libs fftw3f_threads`
  fi
  AC_MSG_RESULT([${have_fftwf_threads}])

  AC_MSG_CHECKING([pkg-config for fftw3f_omp])
  have_fftwf_omp=no
  if $PKG_CONFIG --exists fftw3f_omp; then
    have_fftwf_omp=yes
    FFTW_OMP_CXXFLAGS=`"${PKG_CONFIG}" --cflags fftw3f_omp`
    FFTW_OMP_LIBS=`"${PKG_CONFIG}" --libs fftw3f_omp`
  fi
  AC_MSG_RESULT([${have_fftwf_omp}])
fi

if test "x${have_fftw}" = xno; then
  AC_CHECK_HEADERS(fftw3.h, [have_fftw=yes], [AC_MSG_ERROR(
    [The fftw3 library headers are required.]
  )])
fi

if test "x${have_fftwf_threads}" = xno; then
  AC_MSG_ERROR([Thread-support in fftw3 library is required.])
fi

if test "x${have_fftwf_omp}" = xno; then
  AC_CHECK_HEADERS(omp.h, [have_fftw=yes], [AC_MSG_ERROR(
    [OMP-support in fftw3 library is required.]
  )])
fi


# Write the flags into the src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS}"])
AC_SUBST([PKG_CXXFLAGS], ["${PKG_CXXFLAGS} ${FFTW_CXXFLAGS} ${FFTW_THREADS_CXXFLAGS} ${FFTW_OMP_CXXFLAGS}  ${OPENMP_CXXFLAGS}"])
AC_SUBST([PKG_LIBS], ["${LIBS} ${PKG_LIBS} ${FFTW_LIBS} ${FFTW_THREADS_LIBS} ${FFTW_OMP_LIBS}"])
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
