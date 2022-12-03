#' Create Optimization Schemes
#'
#' Utilizes an FFTW feature to create optimization schemes
#' before the actual calulations. In conjunction with 'fcwt(...,optplan = T)`
#' this can provide a performance boost.
#'
#' @inheritParams create_opt_schemes_raw
#' @seealso fcwt
#' @export
create_opt_schemes <- function(max_size, nthreads = 8L, flag = "measure") {
  create_opt_schemes_raw(max_size, nthreads, flag)
}
