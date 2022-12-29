 AC_INIT(fcwtr, 0.1.0)

m4_include([src/m4_ax_gcc_x86_cpu_supports.m4])
m4_include([src/m4_ax_check_x86_features.m4])
m4_include([src/m4_ax_cxx_compile_stdcxx.m4])

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
AX_CXX_COMPILE_STDCXX(17, , [mandatory])

AX_GCC_BUILTIN(__builtin_cpu_init)
AX_GCC_BUILTIN(__builtin_cpu_supports)

if test [ "$ax_cv_have___builtin_cpu_init" == yes ] &&
    test [ "$ax_cv_have___builtin_cpu_supports" == yes ] ; then
  AX_GCC_X86_CPU_SUPPORTS(avx2, [PKG_CXXFLAGS="$PKG_CXXFLAGS -mavx2"])
else
  # perform the substitution anyways
  # (I do not know how these flags can be checked for other
  # architectures)
  AC_SUBST([PKG_CXXFLAGS], ["${PKG_CXXFLAGS} -mavx2"])
fi


have_fftw=no

PKG_PROG_PKG_CONFIG

if test [ -n "$PKG_CONFIG" ] ; then
  AC_MSG_CHECKING([pkg-config for fftw3f])
  if $PKG_CONFIG --exists fftw3f; then
    have_fftw=yes
    FFTW_CXXFLAGS=`"${PKG_CONFIG}" --cflags fftw3f`
    FFTW_LIBS=`"${PKG_CONFIG}" --libs fftw3f`
  fi
  AC_MSG_RESULT([${have_fftw}])
fi

if test "x${have_fftw}" = xno; then
  AC_CHECK_HEADERS(fftw3.h, [have_fftw=yes], [AC_MSG_ERROR(
    [The fftw3 library headers are required.]
  )])
fi

AX_OPENMP(
  [AC_SEARCH_LIBS(fftwf_plan_with_nthreads, fftw3f_omp, [have_fftwf_omp=yes],
    [AC_MSG_ERROR([OMP-support in fftw3 library is required if OpenMP support is enabled.])]
  )]
)

# Write the flags into the src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS} ${OPENMP_CXXFLAGS}"])
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
