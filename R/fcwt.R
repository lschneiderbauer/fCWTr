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
                 optplan = FALSE) {
  stopifnot(is.numeric(startoctave), startoctave >= 1)
  stopifnot(is.numeric(noctave), noctave >= 1)
  stopifnot(is.numeric(nsuboctaves), nsuboctaves >= 1)
  stopifnot(is.numeric(sigma), sigma > 0)
  stopifnot(is.logical(optplan))

  output <- fcwt_raw(
    input, startoctave, noctave,
    nsuboctaves, 2 * pi * sigma, optplan
  )
  dim(output) <- c(2, length(input), noctave * nsuboctaves)

  return(output)
}


#' Performs a fast continuous wavelet transform and
#' returns the result in a data frame.
#'
#' Performs pooling to accommodate for `time_resolution`.
#' The maximal frequency resolved is determined by the sampling rate
#' of the input time series.
#'
#' @inheritParams fcwt
#' @param time_series     Time series as numeric vector.
#' @param sampling_rate   Sampling rate of input time series in Hz.
#'                        (to establish a connection to physical units.)
#' @param min_freq        Sets the minimal frequency (>0!) in Hz that should be
#'                        contained in the output. Computation time increases
#'                        when lowering the minimal frequency.
#' @param sigma           Sets a parameter modifying the wavelet length. Adjusts
#'                        the time/frequency uncertainty balance. Defaults to 1.
#'                        Larger (lower) value of sigma corresponds to a better
#'                        (worse) frequency resolution and a worse (better) time
#'                        resolution.
#' @param pooling         Pooling function. Defaults to `mean`. Has no effect,
#'                        if `time_resolution` is NULL.
#' @param time_resolution Defines seconds per time index (regulates the size of
#'                        the output by performing pooling (see `pooling` parameter)).
#'                        If NULL, no pooling is performed, and the effective
#'                        time_resolution will be 1 / `sampling_rate`.
#' @param rm.coi          If TRUE, sets the values outside of the "cone of
#'                        influence" to NA.
#'
#' @return The time series as a data frame with the following variables:
#' \describe{
#'   \item{time_ind}{Integer-valued time index}
#'   \item{time}{Time in seconds}
#'   \item{freq}{Frequency in Hz}
#'   \item{value}{Absolute value of correlation coefficient}
#' }
#'
#' @examples
#' # defaults to sigma = 1
#' fcwt_df(
#'   time_series_sin,
#'   sampling_rate = 44100,
#'   time_resolution = 0.001
#' )
#'
#' # enhance frequency resolution
#' fcwt_df(
#'   time_series_sin,
#'   sampling_rate = 44100,
#'   time_resolution = 0.001,
#'   sigma = 5
#' )
#'
#' @export
fcwt_df <- function(time_series,
                    sampling_rate,
                    min_freq = 22,
                    nsuboctaves = 12L,
                    time_resolution = NULL,
                    sigma = 1,
                    pooling = mean,
                    rm.coi = TRUE,
                    optplan = FALSE) {
  startoctave <- 1
  max_freq_hz <- sampling_rate / 2 # Nyquist frequency
  # determine noctave based on sampling_rate and min_freq parameter
  noctave <- ceiling(log2(max_freq_hz / min_freq))

  scale_to_freq <-
    rev(lseq(
      max_freq_hz * sqrt(1 + 1 / nsuboctaves) / 2^noctave,
      max_freq_hz / sqrt((1 + 1 / nsuboctaves)),
      noctave * nsuboctaves
    ))

  result <- fcwt(time_series,
    startoctave = startoctave,
    noctave = noctave,
    nsuboctaves = nsuboctaves,
    sigma = sigma,
    optplan = optplan
  )

  # absolute values of complex result
  result_abs <- sqrt(result[1, , ]^2 + result[2, , ]^2)

  result.df <-
    data.frame(result_abs) |>
    mutate(time_ind = row_number() - 1) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "scale",
      names_prefix = "X",
      names_transform = list(scale = as.integer)
    )

  # perform some pooling in case
  if (!is.null(time_resolution)) {
    n_time <- floor((length(time_series)) / sampling_rate / time_resolution)

    result.df <-
      result.df |>
      mutate(
        time_ind = ntile(n = !!n_time) - 1
      ) |>
      group_by(time_ind, scale) |>
      summarize(
        value = pooling(value),
        .groups = "drop"
      )
  } else {
    time_resolution <- 1 / sampling_rate
  }

  result.df <-
    result.df |>
    mutate(
      time = time_ind * time_resolution
    ) |>
    mutate(
      # include offset due to potentially missing upper frequencies
      freq = scale_to_freq[scale + !!nsuboctaves * (!!startoctave - 1)]
    ) |>
    select(-scale)



  if (rm.coi) {
    max_time <- max(result.df$time)

    # cone of influence
    Ay <- max_freq_hz # * max_freq
    Ax <- 1 / Ay
    By <- max_freq_hz / 2^noctave
    Bx <- 1 / By
    B2x <- max_time - 1 / By
    Cy <- max_freq_hz
    Cx <- max_time - 1 / Cy

    coi <-
      union(
        data.frame(
          time = seq(Ax, Bx, length.out = 100),
          freq = 1 / seq(1 / Ay, 1 / By, length.out = 100),
          side = "left"
        ),
        data.frame(
          time = seq(B2x, Cx, length.out = 100),
          freq = 1 / seq(1 / By, 1 / Cy, length.out = 100),
          side = "right"
        )
      )

    # whiten everything outside COI
    # (Bx - Ax) * (Cy - Ay) - (By - Ay) * (Cx - Ax)
    result.df <-
      result.df |>
      mutate(
        value = if_else(
          ((Bx - Ax) * (1 / freq - 1 / Ay) - (1 / By - 1 / Ay) * (time - Ax) < 0) &
            ((B2x - Cx) * (1 / freq - 1 / Cy) - (1 / By - 1 / Cy) * (time - Cx) > 0),
          value, as.numeric(NA)
        )
      )
  }

  # add our class attribute so we can define a custom
  # plot function
  class(result.df) <- c("fcwt_df", class(result.df))

  return(
    # define column order
    result.df |>
      select(time_ind, time, freq, value)
  )
}


# logarithmic spaced sequence
lseq <- function(from, to, length.out) {
  2^(seq(log2(from), log2(to), length.out = length.out))
}
