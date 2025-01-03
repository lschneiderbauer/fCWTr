
#' Check for OpenMP support
#'
#' This function checks if OpenMP support is enabled. It is responsible for
#' the multithreading capabilities of [fcwt()]. If OpenMP is not enabled
#' the parameter `n_threads` of [fcwt()] is ignored.
#'
#' @details
#' OpenMP can be used by the underlying fCWT library if
#' * the fftw library on your platform was compiled with OpenMP support and
#' * the fCWTr package itself was compiled with OpenMP support.
#'
#' When using pre-built package binaries (like it is typical when you are using
#' R on Windows), it depends on the (CRAN) build server whether or not
#' these conditions are met. They cannot be influenced by the package author.
#'
#' If the user is building the R package yourself, the user needs to make sure
#' that the fftw library on her platform are built with OpenMP support.
#' The fCWTr package is configured to use OpenMP if fftw-OpenMP support is
#' available.
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
