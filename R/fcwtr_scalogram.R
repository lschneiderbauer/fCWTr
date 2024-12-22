new_fcwtr_scalogram <- function(matrix, coi_mask, sample_freq, freq_begin, freq_end,
                                freq_scale, sigma) {

  obj <-
    structure(
      matrix,
      class = c("fcwtr_scalogram", class(matrix)),
      coi_mask = coi_mask,
      sample_freq = sample_freq,
      freq_begin = freq_begin,
      freq_end = freq_end,
      freq_scale = freq_scale,
      sigma = sigma
    )

  dimnames(obj) <-
    list(
      seq_along(matrix[, 1]) - 1,
      seq2(
        freq_end, freq_begin, length.out = dim(matrix)[[2]],
        scale = freq_scale
      )
    )

  obj
}

fcwtr_scalogram <- function(matrix, sample_freq, freq_begin, freq_end,
                            freq_scale, sigma) {

  stopifnot(is.matrix(matrix))
  stopifnot(freq_scale %in% c("linear", "log"))
  stopifnot(is.numeric(sample_freq))
  stopifnot(is.numeric(freq_begin))
  stopifnot(is.numeric(freq_end))
  stopifnot(is.numeric(sigma))

  coi_mask <-
    coi_mask(
      dim_t = dim(matrix)[[1]],
      dim_f = dim(matrix)[[2]],
      sample_freq = sample_freq,
      freq_begin = freq_begin,
      freq_end = freq_end,
      sigma = sigma
    )

  new_fcwtr_scalogram(matrix, coi_mask, sample_freq, freq_begin, freq_end,
                      freq_scale, sigma)
}

sc_set_coi_na <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  x[sc_coi_mask(x)] <- NA_real_

  x
}

sc_coi_mask <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  attr(x, "coi_mask")
}

#' @return A boolean matrix of the same dimensions as `x`. `TRUE` values
#'         indicate values inside the boundary "cone of influence".
#' @noRd
coi_mask <- function(dim_t, dim_f, sample_freq, freq_begin, freq_end, sigma) {
  # The standard deviation Σ of a the Gauß like wave packet at frequency f
  # and sampling frequency f_s with given σ is given by
  # Σ = σ / sqrt(2) f_s / f
  # we choose 4Σ to define the support of a wave packet
  # (and so boundary effects are expected to occur until 2Σ)
  coi_pred <- \(f, t) t * f < sqrt(2) * sigma

  # express in dimensionless quantities
  t <- rep(1:dim_t, times = dim_f)
  f <-
    rep(
      seq(freq_end, freq_begin, length.out = dim_f) / sample_freq,
      each = dim_t
    )

  mask <- coi_pred(f, t) | coi_pred(f, dim_t - t)
  dim(mask) <- c(dim_t, dim_f)

  mask
}

sc_dim_freq <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  dim(x)[[2]]
}

sc_dim_time <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  dim(x)[[1]]
}

#' @return Returns a vector of two values, the first and the last time index
#' that guarantee that all data is available and trustable (no boundary effects).
#' @noRd
#' @importFrom utils head
#' @importFrom utils tail
sc_coi_time_interval <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  #unique(which(is.na(x), arr.ind = TRUE)[, 1])

  full_info_rows <- which(rowSums(sc_coi_mask(x)) == 0)

  if (length(full_info_rows) > 0) {
    c(head(full_info_rows, n = 1), tail(full_info_rows, n = 1))
  } else {
    c(NA_integer_, NA_integer_)
  }
}

sc_rm_coi_time_slices <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  interval <- sc_coi_time_interval(x)
  rows_to_keep <- interval[[1]]:interval[[2]]

  new_fcwtr_scalogram(
    x[rows_to_keep, ],
    attr(x, "coi_mask")[rows_to_keep, ],
    attr(x, "sample_freq"),
    attr(x, "freq_begin"), attr(x, "freq_end"),
    attr(x, "freq_scale"),
    attr(x, "sigma")
  )
}

seq2 <- function(from = 1, to = 1, length.out, scale = c("linear", "log")) {
  scale <- match.arg(scale)

  if (scale == "log") {
    # logarithmic spaced sequence
    # blatantly stolen from library("emdbook"), because need only this
    return(exp(seq(log(from), log(to), length.out = length.out)))
  }

  if (scale == "linear") {
    return(seq(from, to, length.out = length.out))
  }
}


# perform aggregation, if possible.
# if it's not possible, be identity
sc_agg <- function(x, wnd) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  poolsize <- wnd$size_n
  n <- floor(sc_dim_time(x) / poolsize)

  if (poolsize <= 1) {
    # do nothing in case we cannot aggregate
    return(x)
  }

  x_new <- x[1:(poolsize * n), , drop = FALSE]
  dim(x_new) <- c(poolsize, n, dim(x_new)[[2]])
  x_new <- colMeans(x_new, dims = 1, na.rm = TRUE)

  # replace NaN by NA
  x_new[is.nan(x_new)] <- NA_real_

  fcwtr_scalogram(
    x_new,
    attr(x, "sample_freq") / poolsize,
    attr(x, "freq_begin"), attr(x, "freq_end"),
    attr(x, "freq_scale"),
    attr(x, "sigma")
  )
}

tbind <- function(..., deparse.level = 1) {
  args <- list(...)
  stopifnot(length(args) >= 1)
  lapply(args, \(arg) stopifnot(inherits(arg, "fcwtr_scalogram")))

  # check if attributes are identical, otherwise combination
  # does not make sense
  if (length(unique(lapply(args, \(arg) round(attr(arg, "sample_freq"))))) > 1) {
    stop("Sampling frequencies need to be identical.")
  }
  if (length(unique(lapply(args, \(arg) attr(arg, "freq_begin")))) > 1) {
    stop("Frequency ranges need to be identical.")
  }
  if (length(unique(lapply(args, \(arg) attr(arg, "freq_end")))) > 1) {
    stop("Frequency ranges need to be identical.")
  }
  if (length(unique(lapply(args, \(arg) attr(arg, "freq_scale")))) > 1) {
    stop("Frequency scales need to be identical.")
  }
  if (length(unique(lapply(args, \(arg) attr(arg, "sigma")))) > 1) {
    stop("Sigma parameter needs to be identical.")
  }

  x_new <- do.call(rbind, c(lapply(args, unclass), list(deparse.level = deparse.level)))
  coi_mask_new <- do.call(rbind, c(lapply(args, sc_coi_mask), list(deparse.level = deparse.level)))

  new_fcwtr_scalogram(
    x_new,
    coi_mask_new,
    attr(args[[1]], "sample_freq"),
    attr(args[[1]], "freq_begin"), attr(args[[1]], "freq_end"),
    attr(args[[1]], "freq_scale"),
    attr(args[[1]], "sigma")
  )
}

#' Coerce the scalogram matrix to a data frame
#'
#' Internally, the scalogram resulting from [fcwt()] is represented by
#' a numeric matrix. This method coerces this matrix into a reasonable
#' data frame. Note that this conversion has a significant run time cost.
#'
#' @param x
#'  An object resulting from [fcwt()].
#'
#' @return
#'  A [data.frame()] object representing the scalogram data with four columns:
#' \describe{
#'   \item{time_ind}{An integer index uniquely identifying time slices.}
#'   \item{time}{The time difference to the first time slice in physical units.
#'               The time unit is the inverse of the frequency unit chosen by the user
#'               for the `sample_freq` argument of [fcwt()].}
#'   \item{freq}{The frequency in the same units as the `sample_freq` argument
#'               of [fcwt()].}
#'   \item{value}{The fCWT result for the particular time-frequency combination.}
#' }
#'
#' @inheritParams base::as.data.frame
#' @examples
#' fcwt(
#'   sin((1:5000) * 2 * pi * 440 / 44100),
#'   sample_freq = 44100,
#'   n_freqs = 10
#' ) |>
#' as.data.frame() |>
#' head()
#'
#' @export
as.data.frame.fcwtr_scalogram <- function(x, ...) {
  df <- as.data.frame(as.table(x), stringsAsFactors = FALSE)
  names(df) <- c("time_ind", "freq", "value")
  df[["time_ind"]] <- as.integer(df[["time_ind"]])
  df[["freq"]] <- as.numeric(df[["freq"]])

  df[["time"]] <- df[["time_ind"]] / attr(x, "sample_freq")

  df[, c("time_ind", "time", "freq", "value")]
}

#' Scalogram plotting
#'
#' Plots the scalogram resulting from [fcwt()].
#' Requires [ggplot2](https://ggplot2.tidyverse.org/).
#'
#' @param x
#'  An object resulting from [fcwt()].
#'
#' @inheritParams autoplot.fcwtr_scalogram
#' @return No return value, called for side effects.
#'
#' @importFrom graphics plot
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' ts_sin_440 <- sin((1:4410) * 2 * pi * 440 / 44100)
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
#'
#' plot(res)
plot.fcwtr_scalogram <- function(x, n = 1000, ...) {
  print(autoplot.fcwtr_scalogram(x, n, ...))
}

#' Create a ggplot object resembling a scalogram
#'
#' @param n
#'  The plotting function reduces the time resolution by averaging
#'  to generate a reasonable graphics format. `n` is the number of time
#'  steps that are plotted. Defaults to `n = 1000`.
#' @param ...
#'  other arguments passed to specific methods
#' @return
#'  A ggplot object.
#'
#' @keywords internal
autoplot.fcwtr_scalogram <- function(object, n = 1000, ...) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("viridis", quietly = TRUE))
  stopifnot(requireNamespace("rlang", quietly = TRUE))

  .data <- rlang::.data
  ggplot <- ggplot2::ggplot
  aes <- ggplot2::aes
  geom_raster <- ggplot2::geom_raster

  freq_scale <- attr(object, "freq_scale")

  scale_y <-
    if (freq_scale == "log") {
      ggplot2::scale_y_log10
    } else {
      ggplot2::scale_y_continuous
    }

  # first aggregate the time series,
  # since we cannot really see too much resolution anyways
  as.data.frame(sc_agg(object, wnd_from_target_size(n, object))) |>
    ggplot(aes(x = .data$time, y = .data$freq, fill = .data$value)) +
    geom_raster() +
    viridis::scale_fill_viridis(discrete = FALSE) +
    scale_y(name = "Frequency") +
    ggplot2::scale_x_time(name = "Time") +
    ggplot2::theme_minimal()
}
