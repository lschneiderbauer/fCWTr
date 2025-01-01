new_fcwtr_scalogram <- function(matrix,
                                time_offset,
                                sample_freq,
                                freq_begin, freq_end,
                                freq_scale, sigma) {
  structure(
    matrix,
    class = c("fcwtr_scalogram", class(matrix)),
    time_offset = time_offset,
    sample_freq = sample_freq,
    freq_begin = freq_begin,
    freq_end = freq_end,
    freq_scale = freq_scale,
    sigma = sigma
  )
}

fcwtr_scalogram <- function(matrix, time_offset = u(0, "s"), sample_freq,
                            freq_begin, freq_end,
                            freq_scale, sigma) {
  stopifnot(is.matrix(matrix))
  stopifnot(freq_scale %in% c("linear", "log"))
  stopifnot(inherits(sample_freq, "units"))
  stopifnot(inherits(freq_begin, "units"))
  stopifnot(inherits(freq_end, "units"))
  stopifnot(is.numeric(sigma))

  new_fcwtr_scalogram(
    matrix,
    time_offset = time_offset,
    sample_freq, freq_begin, freq_end,
    freq_scale, sigma
  )
}

sc_set_coi_na <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  x[sc_coi_mask(x)] <- NA_real_

  x
}

sc_coi_mask <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  coi_mask(
    dim_t = sc_dim_time(x),
    dim_f = sc_dim_freq(x),
    sample_freq = attr(x, "sample_freq"),
    freq_begin = attr(x, "freq_begin"),
    freq_end = attr(x, "freq_end"),
    freq_scale = attr(x, "freq_scale"),
    sigma = attr(x, "sigma")
  )
}

#' @return A boolean matrix of the same dimensions as `x`. `TRUE` values
#'         indicate values inside the boundary "cone of influence".
#' @noRd
coi_mask <- function(dim_t, dim_f, sample_freq, freq_begin, freq_end,
                     freq_scale, sigma) {
  # The standard deviation Σ of a the Gauß like wave packet at frequency f
  # and sampling frequency f_s with given σ is given by
  # Σ = σ / sqrt(2) f_s / f
  # we choose 4Σ to define the support of a wave packet
  # (and so boundary effects are expected to occur until 2Σ)
  coi_pred <- \(f, t) ddu(t * f) < sqrt(2) * sigma

  # express in dimensionless quantities
  t <- rep(1:dim_t, times = dim_f)
  f <-
    rep(
      seq2(freq_end, freq_begin, length.out = dim_f, scale = freq_scale) / sample_freq,
      each = dim_t
    )

  mask <- coi_pred(f, t) | coi_pred(f, dim_t - t + 1)
  dim(mask) <- c(dim_t, dim_f)

  mask
}

# invalid time steps on one side of the time series
# invalid meaning: at least one frequency in that time range has invalid value
coi_invalid_time_steps <- function(sample_freq, freq_begin, sigma) {
  as.integer(ddu(floor(sqrt(2) * sigma * sample_freq / freq_begin)))
}

sc_dim_freq <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  dim(x)[[2]]
}

sc_dim_time <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  dim(x)[[1]]
}

#' @param n how many slices do we want to remove
#' @noRd
sc_rm_bdry_time_slices <- function(x, n) {
  stopifnot(inherits(x, "fcwtr_scalogram"))
  stopifnot(is.integer(n))

  begin <- n + 1
  end <- sc_dim_time(x) - n

  if (end > begin) {
    rows_to_keep <- begin:end
  } else {
    stop("All data removed. Are you sure you want that?")
  }

  new_fcwtr_scalogram(
    unclass(x)[rows_to_keep, ],
    attr(x, "time_offset") + n / attr(x, "sample_freq"), # new time offset
    attr(x, "sample_freq"),
    attr(x, "freq_begin"), attr(x, "freq_end"),
    attr(x, "freq_scale"),
    attr(x, "sigma")
  )
}

sc_rm_coi_time_slices <- function(x) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  dt <-
    coi_invalid_time_steps(
      attr(x, "sample_freq"), attr(x, "freq_begin"), attr(x, "sigma")
    )

  sc_rm_bdry_time_slices(x, dt)
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

  x_new <- unclass(x)[1:(poolsize * n), , drop = FALSE]
  dim(x_new) <- c(poolsize, n, dim(x_new)[[2]])
  x_new <- colMeans(x_new, dims = 1, na.rm = TRUE)

  # replace NaN by NA
  x_new[is.nan(x_new)] <- NA_real_

  fcwtr_scalogram(
    unclass(x_new),
    attr(x, "time_offset"),
    attr(x, "sample_freq") / poolsize,
    attr(x, "freq_begin"), attr(x, "freq_end"),
    attr(x, "freq_scale"),
    attr(x, "sigma")
  )
}

#' Combine scalograms in "time" direction
#'
#' Given two or more scalograms with identical sampling frequencies, frequency
#' scales and sigma, it can be useful to combine several into a single object
#' creating a longer time series.
#' The function errs if these conditions are not satisfied.
#'
#' The scalograms are stitched together in chronological fashion (i.e. the first
#' argument will the initial piece, etc.).
#' Time offset information is kept from the first piece.
#'
#' @param ...
#'  One or more "fcwtr_scalogram" objects, generated by `fcwt()`.
#'
#' @return Returns a new time-wise combined "fcwtr_scalogram" object.
#'
#' @examples
#' ts_sin_440 <- sin((1:5000) * 2 * pi * 440 / 44100)
#'
#' res <-
#'   fcwt(
#'     ts_sin_440,
#'     sample_freq = u(44.1, "kHz"),
#'     freq_begin = u(50, "Hz"),
#'     freq_end = u(1000, "Hz"),
#'     n_freqs = 10,
#'     sigma = 5
#'   )
#'
#' print(res)
#'
#' # doubled scalogram
#' res_doubled <- rbind(res, res)
#'
#' print(res_doubled)
#'
#' @export
rbind.fcwtr_scalogram <- function(...) {
  args <- list(...)
  stopifnot(length(args) >= 1)
  lapply(args, \(arg) stopifnot(inherits(arg, "fcwtr_scalogram")))

  arg_attr_ident <- function(attr) {
    vals <-
      sapply(args, \(arg) attr(arg, attr))

    all.equal(max(vals), min(vals))
  }

  # check if attributes are identical, otherwise combination
  # does not make sense
  stopifnot(
    "Sampling frequencies need to be identical." = arg_attr_ident("sample_freq"),
    "Frequency ranges need to be identical." = arg_attr_ident("freq_begin"),
    "Frequency ranges need to be identical." = arg_attr_ident("freq_end"),
    "Frequency scales need to be identical." = arg_attr_ident("freq_scale"),
    "Sigma parameter needs to be identical." = arg_attr_ident("sigma")
  )

  x_new <-
    do.call(
      rbind,
      c(lapply(args, unclass))
    )

  new_fcwtr_scalogram(
    x_new,
    attr(args[[1]], "time_offset"),
    attr(args[[1]], "sample_freq"),
    attr(args[[1]], "freq_begin"), attr(args[[1]], "freq_end"),
    attr(args[[1]], "freq_scale"),
    attr(args[[1]], "sigma")
  )
}

#' Print the scalogram
#'
#' `print()` prints its argument and returns it invisibly (via `invisible(x)`).
#'
#' @param x The "fcwtr_scalogram" object resulting from [fcwt()] to print.
#' @inheritParams base::print
#'
#' @return Returns the argument `x` invisibly.
#'
#' @export
print.fcwtr_scalogram <- function(x, ...) {
  cat("_Scalogram_\n")
  cat("<> (Time/Frequency) dimension: [", sc_dim_time(x), ",",
    sc_dim_freq(x), "]\n",
    sep = ""
  )
  cat("<> Sampling rate: ", format(attr(x, "sample_freq")), "\n", sep = "")
  cat("<> Frequency scale: ", format(attr(x, "freq_begin")), " - ",
    format(attr(x, "freq_end")), ", ", attr(x, "freq_scale"), "\n",
    sep = ""
  )
  cat("<> Time offset:", format(attr(x, "time_offset")), "\n")
  cat("<> Sigma: ", attr(x, "sigma"), "\n", sep = "")

  cat("Time/frequency matrix summary\n")
  print(summary(as.vector(as.matrix(x))))

  invisible(x)
}


#' Coerce the scalogram matrix to a data frame
#'
#' Internally, the scalogram resulting from [fcwt()] is represented by
#' a numeric matrix. This method coerces this matrix into a reasonable
#' data frame. Note that this conversion has a significant run time cost.
#'
#' @param x
#'  An "fcwtr_scalogram" object resulting from [fcwt()].
#'
#' @return
#'  A [data.frame()] object representing the scalogram data with four columns:
#' \describe{
#'   \item{time_index}{An integer index uniquely identifying time slices.}
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
#'   as.data.frame() |>
#'   head()
#'
#' @export
as.data.frame.fcwtr_scalogram <- function(x, ...) {
  dim_t <- seq_along(x[, 1])
  dim_f <-
    seq2(
      attr(x, "freq_end"), attr(x, "freq_begin"),
      length.out = dim(x)[[2]],
      scale = attr(x, "freq_scale")
    )

  data.frame(
    time_index = dim_t[row(x)] - 1,
    time = attr(x, "time_offset") + (dim_t[row(x)] - 1) / attr(x, "sample_freq"),
    freq = dim_f[col(x)],
    value = c(x)
  )
}

#' @exportS3Method tibble::as_tibble
as_tibble.fcwtr_scalogram <- function(x, ...) {
  stopifnot(requireNamespace("tibble", quietly = TRUE))

  tibble::as_tibble(as.data.frame(x))
}

#' Extract the data matrix from a scalogram
#'
#' Strips attributes and class from a scalogram object to retrieve
#' a pure matrix.
#'
#' @param x
#'  An "fcwtr_scalogram" object resulting from [fcwt()].
#' @inheritParams base::as.matrix
#'
#' @return A two dimensional numeric vector, inheriting the class "matrix".
#'
#' @examples
#' options(max.print = 10)
#'
#' fcwt(
#'   sin((1:5000) * 2 * pi * 440 / 44100),
#'   sample_freq = 44100,
#'   n_freqs = 10
#' ) |>
#'   as.matrix()
#'
#' @export
as.matrix.fcwtr_scalogram <- function(x, ...) {
  t <- sc_dim_time(x)
  f <- sc_dim_freq(x)

  attributes(x) <- NULL
  dim(x) <- c(t, f)

  x
}

#' Extract parts of a scalogram
#'
#' @param x
#'  An "fcwtr_scalogram" object resulting from [fcwt()].
#' @param i,j
#'  Indices corresponding to time slices of the spectogram which specify
#'  elements to extract. Indices are numeric vectors or empty (missing)
#'  or NULL. Numeric values are coerced to integer or whole numbers as by as.integer
#'  or for large values by `trunc()` (and hence truncated towards zero).
#'
#'  The time offset of the scalogram is adjusted to correspond to `min(i)`.
#'
#'  For [-indexing only: i, j, ... can be logical vectors, indicating elements/slices
#'  to select. Such vectors are recycled if necessary to match the corresponding extent.
#'  i, j, ... can also be negative integers, indicating elements/slices to leave out of the selection.
#'  When indexing arrays by [ a single argument i can be a matrix with as many
#'  columns as there are dimensions of x; the result is then a vector with
#'  elements corresponding to the sets of indices in each row of i.
#'  An index value of NULL is treated as if it were integer(0).
#'
#' @return
#'  Another "fcwtr_scalogram" object that contains only part of the data.
#'
#' @examples
#' fcwt(
#'   sin((1:5000) * 2 * pi * 440 / 44100),
#'   sample_freq = 44100,
#'   n_freqs = 10
#' )[1:1000, 2:7]
#'
#' @export
`[.fcwtr_scalogram` <- function(x, i, j) {
  if (!missing(i)) {
    time_offset <- attr(x, "time_offset") + (min(i) - 1) / attr(x, "sample_freq")
  } else {
    time_offset <- attr(x, "time_offset")
  }

  new_fcwtr_scalogram(
    unclass(x)[i, j, drop = FALSE],
    time_offset,
    attr(x, "sample_freq"),
    attr(x, "freq_begin"), attr(x, "freq_end"),
    attr(x, "freq_scale"),
    attr(x, "sigma")
  )
}

#' Scalogram plotting
#'
#' Plots the scalogram resulting from [fcwt()].
#' Requires [ggplot2](https://ggplot2.tidyverse.org/).
#'
#' @param x
#'  An "fcwtr_scalogram" object resulting from [fcwt()].
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
#' plot(res, time_unit = "ms")
plot.fcwtr_scalogram <- function(x, n = 1000, time_unit = "s", freq_unit = "Hz",
                                 transf = identity, ...) {
  print(autoplot.fcwtr_scalogram(x, n, time_unit, freq_unit, transf, ...))
}

#' Create a ggplot object resembling a scalogram
#'
#' @param object
#'  A "fcwtr_scalogram" object resulting from [fcwt()].
#' @param n
#'  The plotting function reduces the time resolution by averaging
#'  to generate a reasonable graphics format. `n` is the number of time
#'  steps that are plotted. Defaults to `n = 1000`.
#' @param time_unit
#'  A time unit that is used for the x-axis scale. Default to "s" - seconds.
#'  See `units::valid_udunits()` and `units::valid_udunits_prefixes()` for valid
#'  expressions.
#' @param freq_unit
#'  A frequency unit that is used for the y-axis scale.
#'  Defaults to "Hz" - "Hertz".
#'  See `units::valid_udunits()` and `units::valid_udunits_prefixes()` for valid
#'  expressions.
#' @param transf
#'  A function, taking a vector and returning a vector, that is used to
#'  transform the scalogram values before plotting.
#' @param ...
#'  other arguments passed to specific methods
#' @return
#'  A ggplot object.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#'
#' res <-
#'   fcwt(
#'     sin((1:4410) * 2 * pi * 440 / 44100),
#'     sample_freq = 44100,
#'     freq_begin = 50,
#'     freq_end = 1000,
#'     n_freqs = 10,
#'     sigma = 5
#'   )
#'
#' autoplot(res, time_unit = "ms")
#'
#' @exportS3Method ggplot2::autoplot
autoplot.fcwtr_scalogram <- function(object, n = 1000,
                                     time_unit = "s", freq_unit = "Hz",
                                     transf = identity, ...) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("viridis", quietly = TRUE))
  stopifnot(requireNamespace("rlang", quietly = TRUE))

  .data <- rlang::.data
  ggplot <- ggplot2::ggplot
  aes <- ggplot2::aes
  geom_raster <- ggplot2::geom_raster

  freq_scale <- attr(object, "freq_scale")

  transform <-
    if (freq_scale == "log") {
      "log10"
    } else {
      "identity"
    }

  # first aggregate the time series,
  # since we cannot really see too much resolution anyways
  df <- as.data.frame(sc_agg(object, wnd_from_target_size(n, object)))

  # transform the value if requested
  df[["value"]] <- transf(df[["value"]])

  df |>
    ggplot(aes(x = .data$time, y = .data$freq, fill = .data$value)) +
    geom_raster() +
    viridis::scale_fill_viridis(discrete = FALSE) +
    units::scale_x_units(name = "Time", unit = time_unit) +
    units::scale_y_units(
      name = "Frequency", unit = freq_unit,
      transform = transform
    ) +
    ggplot2::theme_minimal()
}
