#' Calculates Time/Frequency Resolution
#'
#' For a given `sigma` and reference frequency `freq` this function calculates
#' the expected time and frequency resolution respectively.
#'
#' Note that this resolution depends on the reference frequency, since the
#' FCWT scales the wavelet spread depending on the frequency in a natural way.
#' (in contrast to a simple fixed-windowed Fourier Transform).
#'
#' @param sigma
#'  The dimensionless positive parameter in [fcwt()] controlling the wavelet spread.
#'
#' @param freq
#'  The reference frequency where the frequency and time resolution are
#'  calculated. Must be a quantity with frequency units (see [u()]).
#'
#' @return
#'  A named list with two elements: "time" and "freq", in time and frequency
#'  units. They obey the property \eqn{time \times freq = \frac{8]{\pi}}}.
#'
#' @examples
#' sigma_res(1, u(440, "Hz"))
#'
#' @concept sigma
#' @family sigma
#' @export
sigma_res <- function(sigma, freq) {
  stopifnot(is.numeric(sigma), sigma > 0)
  stopifnot(inherits(freq, "units"), has_comp_unit(freq, "Hz"))

  list(
    time = 4 * sigma / freq,
    freq = 4 * 1 / (2 * pi) * freq / sigma
  )
}

#' Calculates Relative Frequency Resolution
#'
#' For a given `sigma` this function calculates
#' the expected relative frequency resolution respectively.
#'
#' @param sigma
#'  The dimensionless positive parameter in [fcwt()] controlling the wavelet spread.
#'
#' @return
#'  A number `p` representing the relative frequency resolution. I.e. at a given
#'  resolution `f`, the frequency resolution is `f*p`.
#'
#' @examples
#' sigma_freq_res_rel(1)
#'
#' @concept sigma
#' @family sigma
#' @export
sigma_freq_res_rel <- function(sigma) {
  stopifnot(is.numeric(sigma), sigma > 0)

  2 / (pi * sigma)
}

#' Determine Sigma from a frequency resolution requirement
#'
#' Setting the correct value for `sigma` in [fcwt()] for a particular
#' use case is important.
#' Sigma controls the wavelet spread used by the FCWT and so determines
#' the time-/frequency resolution (depending on a given reference frequency).
#'
#' If you set a high frequency resolution, the time resolution
#' of the output signal will suffer, and vice versa. Determining the correct
#' balance for a particular use case can be crucial. This function determines
#' the value of sigma for a particular frequency resolution requirement.
#'
#' @param res
#'  The required frequency resolution in frequency units (see [u()]).
#'
#' @param freq
#'  The reference frequency where the required resolution must hold.
#'  Must be a quantity with frequency units (see [u()]).
#'
#' @return
#'  A dimensionless number controlling the FCWT wavelet spread,
#'  corresponding to the `sigma` argument in [fcwt()].
#'
#' @examples
#' ref_freq <- u(440, "Hz")
#' sigma <- sigma_from_freq_res(u(7, "Hz"), ref_freq)
#'
#' sigma_res(sigma, ref_freq)
#'
#' @concept sigma
#' @family sigma
#' @export
sigma_from_freq_res <- function(res, freq) {
  stopifnot(inherits(res, "units"), has_comp_unit(res, "Hz"))
  stopifnot(inherits(freq, "units"), has_comp_unit(freq, "Hz"))
  stopifnot(res > u(0, "Hz"))
  stopifnot(freq > u(0, "Hz"))

  ddu(4 / (2 * pi) * freq / res)
}

#' Determine Sigma from a time resolution requirement
#'
#' Setting the correct value for `sigma` in [fcwt()] for a particular
#' use case is important.
#' Sigma controls the wavelet spread used by the FCWT and so determines
#' the time-/frequency resolution (depending on a given reference frequency).
#'
#' If you set a high frequency resolution, the time resolution
#' of the output signal will suffer, and vice versa. Determining the correct
#' balance for a particular use case can be crucial. This function determines
#' the value of sigma for a particular time resolution requirement.
#'
#' @param res
#'  The required time resolution in frequency units (see [u()]).
#'
#' @param freq
#'  The reference frequency where the required resolution must hold.
#'  Must be a quantity with frequency units (see [u()]).
#'
#' @return
#'  A dimensionless number controlling the FCWT wavelet spread,
#'  corresponding to the `sigma` argument in [fcwt()].
#'
#' @examples
#' ref_freq <- u(440, "Hz")
#' sigma <- sigma_from_time_res(u(10, "ms"), ref_freq)
#'
#' sigma_res(sigma, ref_freq)
#'
#' @concept sigma
#' @family sigma
#' @export
sigma_from_time_res <- function(res, freq) {
  stopifnot(inherits(res, "units"), has_comp_unit(res, "s"))
  stopifnot(inherits(freq, "units"), has_comp_unit(freq, "Hz"))
  stopifnot(res > u(0, "s"))
  stopifnot(freq > u(0, "Hz"))

  ddu(freq * res / 4)
}

#' Determine Sigma from a relative frequency resolution requirement
#'
#' Setting the correct value for `sigma` in [fcwt()] for a particular
#' use case is important.
#' Sigma controls the wavelet spread used by the FCWT and so determines
#' the time-/frequency resolution (depending on a given reference frequency).
#'
#' If you set a high frequency resolution, the time resolution
#' of the output signal will suffer, and vice versa. Determining the correct
#' balance for a particular use case can be crucial. This function determines
#' the value of sigma for a particular relative frequency resolution requirement.
#' Note that the relative frequency resolution does not depend on the frequency
#' itself, in contrast to the absolute frequency resolution.
#'
#' @param res
#'  The required relative frequency resolution, a number between 0 and 1.
#'
#' @return
#'  A dimensionless number controlling the FCWT wavelet spread,
#'  corresponding to the `sigma` argument in [fcwt()].
#'
#' @examples
#' ref_freq <- u(440, "Hz")
#' sigma <- sigma_from_freq_res_rel(0.2)
#'
#' sigma_res(sigma, ref_freq)
#'
#' @concept sigma
#' @family sigma
#' @export
sigma_from_freq_res_rel <- function(res) {
  stopifnot(res > 0, res < 1)

  2 / (pi * res)
}
