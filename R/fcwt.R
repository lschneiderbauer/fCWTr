#' Performs a fast continuous wavelet transform
#'
#' This function call is a thin wrapper for the fcwt API.
#'
#' @inheritParams fcwt_raw
#' @param noctave   The number of octaves to be processed.
#' @param optplan   If TRUE uses the optimization plans that might have been
#'                  generated before by [create_optimization_schemes_raw].
#' @return  Returns a numeric vector with dim = c(2, length(input), scales)
#'          where the first slot represents the real and imaginary part of the
#'          complex result, the second slot represents the time index, and
#'          the third slot represent different scales / frequencies.
#' @export
fcwt <- function(input,
                 startoctave = 1L,
                 noctave = 8L,
                 nsuboctaves = 12L,
                 sigma = 1,
                 nthreads = 8,
                 optplan = FALSE,
                 abs = F) {
  stopifnot(is.numeric(startoctave), startoctave >= 1)
  stopifnot(is.numeric(noctave), noctave >= 1)
  stopifnot(is.numeric(nsuboctaves), nsuboctaves >= 1)
  stopifnot(is.numeric(sigma), sigma > 0)
  stopifnot(is.logical(optplan))

  output <- fcwt_raw(
    input, startoctave, noctave,
    nsuboctaves, 2 * pi * sigma, nthreads, optplan, abs
  )

  dim(output) <-
    if(!abs) {
      c(2, length(input), noctave * nsuboctaves)
    } else {
      c(length(input), noctave * nsuboctaves)
    }

  return(output)
}
