
#' Check for OpenMP support
#'
#' This function checks if fCWTr was compiled with OpenMP support. It is
#' responsible for the multithreading capabilities of [fcwt()]. If OpenMP is
#' not enabled the parameter `n_threads` of [fcwt()] is ignored.
#'
#' @details
#' OpenMP is used by fCWTr if the compiler supports it. In addition, the
#' Fourier transforms are multithreaded only if the fftw library on your
#' platform comes with OpenMP support (`libfftw3f_omp`); otherwise they run
#' single-threaded.
#'
#' On Windows, OpenMP support is disabled, since the fftw library provided
#' by Rtools is compiled without OpenMP support.
#'
#' When using pre-built package binaries, it depends on the (CRAN) build server
#' whether or not these conditions are met. When installing the package from
#' source, the installation script detects whether these conditions are met.
#'
#' @return ( `TRUE` | `FALSE` )
#'  Returns `TRUE` if OpenMP support is enabled, `FALSE` otherwise.
#'
#' @examples
#' openmp_enabled()
#'
#' @export
#' @concept sys_support
openmp_enabled <- function() {
  has_openmp()
}


#' Check for AVX instruction set support
#'
#' This function checks if fCWTr was compiled with AVX instruction set support.
#' AVX instructions need to be supported by the users' CPU in order for this
#' to work.
#'
#' @details
#' By default, most compiler setups do not make use of AVX to increase
#' portability of the binary. If you are an R user that has a CPU supporting
#' AVX and want to make use of it, you might need to manually enable compiler
#' flags to let R know about it, and install the package from source
#' (so that it gets compiled on your machine).
#'
#' @return ( `TRUE` | `FALSE` )
#'  Returns `TRUE` if AVX support is enabled, `FALSE` otherwise.
#'
#' @examples
#' avx_enabled()
#'
#' @export
#' @concept sys_support
avx_enabled <- function() {
  has_avx()
}
