new_fcwtr_scalogram <- function(matrix, sample_freq, freq_begin, freq_end,
                                sigma, remove_coi) {
  if (remove_coi) {
    T <- dim(matrix)[[1]] # Time dimension
    F <- dim(matrix)[[2]] # Frequency dimension

    # express everything dimensionless
    t <- rep(1:T, times = F)
    f <-
      rep(
        seq(freq_end, freq_begin, length.out = F) / sample_freq,
        each = T
      )

    cmp <- sigma

    # check if points are inside / outside hyperbolic cone
    matrix[!(f * t > cmp & f * (T - t) > cmp)] <- NA
  }

  obj <-
    structure(
      matrix,
      class = c("fcwtr_scalogram", class(matrix)),
      sample_freq = sample_freq,
      freq_begin = freq_begin,
      freq_end = freq_end,
      sigma = sigma
    )

  dimnames(obj) <-
    list(
      seq_along(matrix[, 1]) - 1,
      seq(freq_end, freq_begin, length.out = dim(matrix)[[2]])
    )

  obj
}

agg <- function(x, n) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  poolsize <- floor(dim(x)[[1]] / n)
  x_new <- x[1:(poolsize * n), ]
  dim(x_new) <- c(poolsize, n, dim(x_new)[[2]])
  x_new <- colMeans(x_new, dims = 1, na.rm = TRUE)

  new_fcwtr_scalogram(
    x_new,
    attr(x, "sample_freq") / poolsize,
    attr(x, "freq_begin"), attr(x, "freq_end"),
    attr(x, "sigma"),
    remove_coi = FALSE
  )
}

#' @export
as.data.frame.fcwtr_scalogram <- function(x, ...) {
  df <- as.data.frame(as.table(x), stringsAsFactors = FALSE)
  names(df) <- c("time_ind", "freq", "value")
  df[["time_ind"]] <- as.integer(df[["time_ind"]])
  df[["freq"]] <- as.numeric(df[["freq"]])

  df[["time"]] <- df[["time_ind"]] / attr(x, "sample_freq")

  df
}

#' @importFrom graphics plot
#' @export
plot.fcwtr_scalogram <- function(x, n = 1000, ...) {
  print(autoplot.fcwtr_scalogram(x, n, ...))
}

autoplot.fcwtr_scalogram <- function(object, n = 1000, ...) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("viridis", quietly = TRUE))
  stopifnot(requireNamespace("rlang", quietly = TRUE))

  .data <- rlang::.data
  ggplot <- ggplot2::ggplot
  aes <- ggplot2::aes
  geom_raster <- ggplot2::geom_raster

  # first aggregate the time series,
  # since we cannot really see too much resolution anyways
  as.data.frame(agg(object, n)) |>
  ggplot(aes(x = .data$time, y = .data$freq, fill = .data$value)) +
    geom_raster() +
    viridis::scale_fill_viridis(discrete = FALSE) +
    ggplot2::scale_y_continuous(name = "Frequency") +
    ggplot2::scale_x_time(name = "Time") +
    ggplot2::theme_minimal()
}
