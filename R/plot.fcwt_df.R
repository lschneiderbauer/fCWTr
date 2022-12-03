#' Plots a scalogram given a fcwt dataframe.
#'
#' The x-axis corresponds to time, y-axis corresponds to frequency.
#' Note, the frequency axis is scaled logarithmically.
#'
#' @param x A data frame as is returned by [fcwt.df].
#' @return ggplot plot object
#' @examples
#' # providing the sampling rate in addition to the time series is important to
#' # establishing a connection to physical units.
#' result_df <- fcwt_df(ts_sin_440,
#'   sampling_rate = 44100,
#'   time_resolution = 0.001
#' )
#' # plot the result
#' plot(result_df)
#'
#' # now change the time-frequency uncertainty
#' result_df <- fcwt_df(ts_sin_440,
#'   sampling_rate = 44100,
#'   time_resolution = 0.001, sigma = 3
#' )
#' # plot the result again
#' plot(result_df)
#'
#' @importFrom rlang .data
#' @export
plot.fcwt_df <- function(x, ...) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("viridis", quietly = TRUE))

  plot <-
    x |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$time, y = .data$freq, fill = .data$value
      )
    ) +
    ggplot2::geom_raster() +
    viridis::scale_fill_viridis(discrete = FALSE) +
    ggplot2::scale_y_continuous(
      name = "Freq [Hz]", trans = "log2", n.breaks = 20
    ) +
    ggplot2::scale_x_time(name = NULL) +
    ggplot2::theme_minimal()

  return(plot)
}
