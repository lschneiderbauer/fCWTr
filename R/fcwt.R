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
                 sample_freq,
                 freq_begin,
                 freq_end,
                 n_freqs,
                 sigma,
                 abs = FALSE,
                 nthreads = 8L) {
  stopifnot(is.numeric(sample_freq), sample_freq > 0)
  stopifnot(is.numeric(freq_begin), freq_begin > 0)
  stopifnot(is.numeric(freq_end), freq_end > freq_begin)
  stopifnot(is.numeric(n_freqs), n_freqs > 0)
  stopifnot(is.numeric(sigma), sigma > 0)
  stopifnot(is.numeric(nthreads))
  stopifnot(is.logical(abs))

  output <- fcwt_raw(
    input, as.integer(sample_freq), freq_begin, freq_end, as.integer(n_freqs),
    sigma, as.integer(nthreads), optplan, abs
  )


  if (!abs) {
    dim(output) <- c(length(input), n_freqs, 2)

    output <- output[, , 1] + output[, , 2] * 1i
  } else {
    dim(output) <- c(length(input), n_freqs)
  }

  class(output) <- c("spectogram", class(output))

  return(output)
}
