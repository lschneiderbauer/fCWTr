#' Fast continuous wavelet transform
#'
#' The core function of this package making use of the fCWT library. It processes
#' an input signal in form of a real valued numeric vector interpreted as an
#' evenly spaced time series and returns the absolute values of a spectogram,
#' i.e. a graph with a time and a frequency dimension.
#'
#' @details
#' The wavelet used in this calculation is the so called Morlet wavelet,
#' a sinusoidal wave modulated by a Gaussian whose spread is controlled by
#' the argument `sigma`.
#'
#' See the original paper
#' Arts, L.P.A., van den Broek, E.L.
#' The fast continuous wavelet transformation (fCWT) for real-time, high-quality,
#' noise-resistant time–frequency analysis.
#' Nat Comput Sci 2, 47–58 (2022). \doi{10.1038/s43588-021-00183-z}
#'
#' @param signal
#'  Real-valued time series. The time steps are assumed to be evenly spaced.
#'
#' @param sample_freq
#'  Sampling rate of input time series. This number primarily establishes
#'  a connection to physical units which is used in other frequency definitions
#'  as well as the units of the output data.
#'
#' @param n_freqs
#'  Number of frequency bins generated by the CWT. The frequencies
#'  are linearly distributed. Computation time increases when raising the
#'  number of frequency bins.
#'
#' @param freq_begin,freq_end
#'  Optionally specifies the frequency range `[freq_end, freq_begin]`. If not
#'  specified the maximal meaningful frequency range, depending on the input signal,
#'  is taken.
#'  The range and `sample_freq` need to be specified in the same units.
#'
#' @param sigma
#'  Sets a dimensionless parameter modifying the wavelet spread which in the
#'  time-domain is roughly given by \eqn{\Sigma_t \sim \sqrt{2} \frac{\sigma}{f}}.
#'  Changing this parameter adjusts the time/frequency uncertainty balance.
#'  Defaults to 1.
#'  Larger (lower) value of sigma corresponds to a better (worse) frequency
#'  resolution and a worse (better) time resolution.
#'
#' @param remove_coi
#'  Boundary effects can result in nonphysical artifacts. If `remove_coi = TRUE`,
#'  those are effectively removed by setting corresponding values to `NA`.
#'  We define the essential support of the
#'  (Gaussian) wavelet to be four times its standard deviation,
#'  \eqn{4 \Sigma_t = 2 * \sqrt{2} \frac{\sigma}{f}}, and so a wavelet touches
#'  the boundary if the distance of the center of the wavelet to the boundary
#'  is less then \eqn{4 \Sigma_t}. Values that fall into that range are removed
#'  if `remove_coi = TRUE`.
#'
#' @param n_threads
#'  Number of threads used by the computation, if supported by your platform.
#'  Defaults to 2 threads (to accomodate CRAN requirements).
#'
#' @return
#'  The spectogram, a numeric real-valued matrix with dimensions
#'  `dim = c(length(signal), n_freqs)`.
#'  This matrix is wrapped into a S3-class `fcwtr_scalogram` so that plotting and
#'  coercion functions can be used conveniently.
#'
#' @examples
#' ts_sin_440 <- sin((1:5000) * 2 * pi * 440 / 44100)
#'
#' res <-
#'   fcwt(
#'     ts_sin_440,
#'     sample_freq = 44100,
#'     freq_begin = 50,
#'     freq_end = 1000,
#'     n_freqs = 10,
#'     sigma = 5
#'   )
#' @export
fcwt <- function(signal,
                 sample_freq,
                 n_freqs,
                 freq_begin = 2 * sample_freq / length(signal),
                 freq_end = sample_freq / 2,
                 sigma = 1,
                 # abs = FALSE,
                 remove_coi = TRUE,
                 n_threads = 2L) {
  stopifnot(is.numeric(signal))
  stopifnot(is.numeric(sample_freq), sample_freq > 0)
  stopifnot(is.numeric(freq_begin), freq_begin > 0)
  stopifnot(is.numeric(freq_end), freq_end > freq_begin)
  stopifnot(is.numeric(n_freqs), n_freqs > 0)
  stopifnot(is.numeric(sigma), sigma > 0)
  stopifnot(is.numeric(n_threads))
  # stopifnot(is.logical(abs))

  output <-
    fcwt_raw(
      as.numeric(signal), as.integer(sample_freq), freq_begin, freq_end,
      as.integer(n_freqs), sigma, as.integer(n_threads), FALSE,
      abs = TRUE
    )

  # if (!abs) {
  #   dim(output) <- c(length(signal), n_freqs, 2)
  #
  #   output <- output[, , 1] + output[, , 2] * 1i
  # } else {
  dim(output) <- c(length(signal), n_freqs)
  # }

  new_fcwtr_scalogram(
    output, sample_freq, freq_begin, freq_end, sigma, remove_coi
  )
}
