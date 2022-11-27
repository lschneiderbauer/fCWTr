#' @inheritParams create_opt_schemes_raw
#' @export
create_opt_schemes <- function(max_size, nthreads = 8L, flag = "measure") {
  create_opt_schemes_raw(max_size, nthreads, flag)
}
