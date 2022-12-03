
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
#'                        The sampling rate also defines the highest possible
#'                        frequency resolution of the CWT: half of the sampling
#'                        rate.
#' @param min_freq        Sets the minimal frequency (>0!) in Hz that should be
#'                        contained in the output. Computation time increases
#'                        when lowering the minimal frequency.
#' @param sigma           Sets a parameter modifying the wavelet length. Adjusts
#'                        the time/frequency uncertainty balance. Defaults to 1.
#'                        Larger (lower) value of sigma corresponds to a better
#'                        (worse) frequency resolution and a worse (better) time
#'                        resolution.
#' @param time_resolution Defines seconds per time index (regulates the size of
#'                        the output by performing mean-pooling.
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
#'   ts_sin_superpos,
#'   sampling_rate = 44100,
#'   time_resolution = 0.001
#' )
#'
#' # enhance frequency resolution
#' fcwt_df(
#'   ts_sin_superpos,
#'   sampling_rate = 44100,
#'   time_resolution = 0.001,
#'   sigma = 5
#' )
#'
#' @importFrom rlang .data
#' @export
fcwt_df <- function(time_series,
                    sampling_rate,
                    min_freq = sampling_rate / 2000,
                    nsuboctaves = 12L,
                    time_resolution = NULL,
                    sigma = 1,
                    nthreads = 8L,
                    rm.coi = TRUE,
                    optplan = FALSE) {
  startoctave <- 1
  max_freq <- sampling_rate / 2 # Nyquist frequency
  # determine noctave based on sampling_rate and min_freq parameter
  noctave <- freqs_to_noctaves(min_freq, max_freq)

  scale_to_freq <-
    rev(lseq(
      max_freq * sqrt(1 + 1 / nsuboctaves) / 2^noctave,
      max_freq / sqrt((1 + 1 / nsuboctaves)),
      noctave * nsuboctaves
    ))

  result_abs <- fcwt(time_series,
    startoctave = startoctave,
    noctave = noctave,
    nsuboctaves = nsuboctaves,
    sigma = sigma,
    nthreads = nthreads,
    optplan = optplan,
    abs = T
  )

  # perform pooling before we create data frame
  # (pivot longer and pooling afterwards is expensive)
  if (!is.null(time_resolution)) {
    npoolsize <- floor(sampling_rate * time_resolution)
    newlength <- floor(length(time_series) / npoolsize)

    # we might have to cut the length in order to get an integer valued
    # size decomposition of the length
    result_abs <- result_abs[1:(newlength * npoolsize), ]
    dim(result_abs) <- c(npoolsize, newlength, dim(result_abs)[2])

    result_abs <- colMeans(result_abs, dims = 1)
  } else {
    time_resolution <- 1 / sampling_rate
  }

  result.df <-
    data.frame(result_abs) |>
    mutate(time_ind = row_number() - 1) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "scale",
      names_prefix = "X",
      names_transform = list(scale = as.integer)
    ) |>
    mutate(
      time = .data$time_ind * time_resolution
    ) |>
    mutate(
      # include offset due to potentially missing upper frequencies
      freq = scale_to_freq[.data$scale + !!nsuboctaves * (!!startoctave - 1)]
    ) |>
    select(-.data$scale)



  if (rm.coi) {
    max_time <- max(result.df$time)

    # cone of influence
    Ay <- max_freq # * max_freq
    Ax <- 1 / Ay
    By <- max_freq / 2^noctave
    Bx <- 1 / By
    B2x <- max_time - 1 / By
    Cy <- max_freq
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
        value =
          if_else(
            ((Bx - Ax) * (1 / .data$freq - 1 / Ay) -
              (1 / By - 1 / Ay) * (.data$time - Ax) < 0) &
              ((B2x - Cx) * (1 / .data$freq - 1 / Cy) -
                (1 / By - 1 / Cy) * (.data$time - Cx) > 0),
            .data$value, as.numeric(NA)
          )
      )
  }

  # add our class attribute so we can define a custom
  # plot function
  class(result.df) <- c("fcwt_df", class(result.df))

  return(
    # define column order
    result.df |>
      select(.data$time_ind, .data$time, .data$freq, .data$value)
  )
}
