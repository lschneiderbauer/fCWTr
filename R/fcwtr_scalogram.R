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

# Number of time slices at each boundary that lie inside the "cone of
# influence" (and are therefore affected by boundary effects), for each
# frequency (column) of a scalogram with `dim_t` time slices.
#
# The standard deviation of the Gaussian wave packet at frequency f is
# sigma / f; we choose 4 sigma / f as the support of a wave packet, so
# boundary effects are expected for time steps t (relative to the boundary,
# in units of the sampling period) with t * f / sample_freq < 2 sigma.
#
# Computed column by column, so that only small temporary objects are needed
# (the mask of the whole scalogram has the same size as the scalogram).
coi_n_slices <- function(dim_t, sample_freq, freq_begin, freq_end, freq_scale,
                         sigma, dim_f) {
  # frequencies in units of the sampling frequency
  f_rel <- ddu(
    seq2(freq_end, freq_begin, length.out = dim_f, scale = freq_scale) /
      sample_freq
  )

  t <- seq_len(dim_t)
  vapply(f_rel, function(f) sum(t * f < 2 * sigma), integer(1))
}

# invalid time steps on one side of the time series
# invalid meaning: at least one frequency in that time range has invalid value
coi_invalid_time_steps <- function(sample_freq, freq_begin, sigma) {
  as.integer(ddu(floor(2 * sigma * sample_freq / freq_begin)))
}

# frequencies of the columns (highest frequency first),
# in the units of freq_begin / freq_end
sc_freqs <- function(x) {
  seq2(
    attr(x, "freq_end"), attr(x, "freq_begin"),
    length.out = sc_dim_freq(x),
    scale = attr(x, "freq_scale")
  )
}

# turns an index (positive, negative, or logical) into positive indices,
# and checks that they form a contiguous ascending range: only then the
# result can be represented by an evenly spaced time / frequency axis
contiguous_index <- function(idx, n, what) {
  pos <- seq_len(n)[idx]

  if (length(pos) == 0 || anyNA(pos) || any(diff(pos) != 1)) {
    stop(
      "The ", what, " index must select a contiguous, ascending range ",
      "(e.g. `10:20`) within the scalogram's dimensions. ",
      "Use `as.matrix()` for arbitrary subsetting.",
      call. = FALSE
    )
  }

  pos
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
    unclass(x)[rows_to_keep, , drop = FALSE],
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
#' argument will be the initial piece, etc.).
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
#'     x_sample_freq = u(44.1, "kHz"),
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
  cat("* (Time/Frequency) dimension:  ( ", sc_dim_time(x), " , ",
    sc_dim_freq(x), " )\n",
    sep = ""
  )
  cat("* Sampling rate: ", format(attr(x, "sample_freq")), "\n", sep = "")
  cat("* Frequency scale: ", format(attr(x, "freq_begin")), " - ",
    format(attr(x, "freq_end")), ", ", attr(x, "freq_scale"), "\n",
    sep = ""
  )
  cat("* Time offset:", format(pretty_time(attr(x, "time_offset"))), "\n")
  cat("* Sigma: ", attr(x, "sigma"), "\n", sep = "")

  cat("  o Time resolution at ", format(attr(x, "freq_begin")), ": ",
      format(pretty_time(sigma_res(attr(x, "sigma"), attr(x, "freq_begin"))$time)) , "\n")
  cat("  o Time resolution at ", format(attr(x, "freq_end")), ": ",
      format(pretty_time(sigma_res(attr(x, "sigma"), attr(x, "freq_end"))$time)) , "\n")

  cat("  o Relative frequency resolution: ",
      format(sigma_freq_res_rel(attr(x, "sigma"))), "\n")

  cat("* Time/frequency matrix summary\n")
  print(summary(as.vector(as.matrix(x))))

  invisible(x)
}


#' Convert the scalogram matrix to a data frame
#'
#' Internally, the scalogram resulting from [fcwt()] is represented by
#' a numeric [matrix()]. This method converts this matrix into a
#' [data.frame()] in long format, with one row per matrix entry.
#' Note that this conversion has a significant run time cost.
#'
#' @param x
#'  An "fcwtr_scalogram" object resulting from [fcwt()].
#'
#' @param ...
#'  Additional arguments are ignored.
#'
#' @return
#'  A [data.frame()] object representing the scalogram data with four columns:
#' \describe{
#'   \item{time_index}{An integer index uniquely identifying time slices,
#'               starting at 0.}
#'   \item{time}{The time of the time slice in physical units, relative to the
#'               beginning of the input signal (time slices that were cut off,
#'               e.g. by subsetting, are taken into account).
#'               The time unit (s, ms, us, or ns) is chosen depending on the
#'               magnitude of the values, and can be converted with
#'               `units::set_units()`.}
#'   \item{freq}{The frequency, in the units chosen by the user for the
#'               `freq_begin` and `freq_end` arguments of [fcwt()].}
#'   \item{value}{The fCWT result for the particular time-frequency combination.}
#' }
#'
#' @examples
#' fcwt(
#'   sin((1:5000) * 2 * pi * 440 / 44100),
#'   x_sample_freq = u(44100, "Hz"),
#'   n_freqs = 10
#' ) |>
#'   as.data.frame() |>
#'   head()
#'
#' @export
as.data.frame.fcwtr_scalogram <- function(x, ...) {
  dim_t <- seq_len(sc_dim_time(x))
  dim_f <- sc_freqs(x)

  data.frame(
    time_index = dim_t[row(x)] - 1,
    # in a time unit (s, ms, ...) that suits the magnitude of the values
    time = pretty_time(
      (dim_t[row(x)] - 1) / attr(x, "sample_freq") + attr(x, "time_offset")
    ),
    freq = dim_f[col(x)],
    value = c(x)
  )
}

#' Convert the scalogram matrix to a tibble
#'
#' Internally, the scalogram resulting from [fcwt()] is represented by
#' a numeric [matrix()]. This method converts this matrix into a
#' [tibble::tibble()] in long format, with one row per matrix entry.
#' Note that this conversion has a significant run time cost.
#'
#' This method is conceptually identical to [as.data.frame()],
#' it simply returns a [tibble::tibble()] instead of a [data.frame()].
#'
#' @return
#'  A [tibble::tibble()] object representing the scalogram data with four columns:
#' \describe{
#'   \item{time_index}{An integer index uniquely identifying time slices,
#'               starting at 0.}
#'   \item{time}{The time of the time slice in physical units, relative to the
#'               beginning of the input signal (time slices that were cut off,
#'               e.g. by subsetting, are taken into account).
#'               The time unit (s, ms, us, or ns) is chosen depending on the
#'               magnitude of the values, and can be converted with
#'               `units::set_units()`.}
#'   \item{freq}{The frequency, in the units chosen by the user for the
#'               `freq_begin` and `freq_end` arguments of [fcwt()].}
#'   \item{value}{The fCWT result for the particular time-frequency combination.}
#' }
#'
#' @inherit as.data.frame.fcwtr_scalogram
#' @examplesIf requireNamespace("tibble", quietly = TRUE)
#' library(tibble)
#'
#' fcwt(
#'   sin((1:5000) * 2 * pi * 440 / 44100),
#'   x_sample_freq = u(44100, "Hz"),
#'   n_freqs = 10
#' ) |>
#'   as_tibble()
#'
#' @exportS3Method tibble::as_tibble
as_tibble.fcwtr_scalogram <- function(x, ...) {
  stopifnot(requireNamespace("tibble", quietly = TRUE))

  tibble::as_tibble(as.data.frame(x, ...))
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
#'   x_sample_freq = u(44100, "Hz"),
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
#' Selects a time range and/or a frequency range of a scalogram. The time
#' offset and the frequency range of the result are adjusted accordingly.
#'
#' @param x
#'  An "fcwtr_scalogram" object resulting from [fcwt()].
#' @param i,j
#'  Indices selecting time slices (rows, `i`) and frequencies (columns, `j`;
#'  ordered from the highest to the lowest frequency).
#'  Positive, negative, or logical indices are allowed, as long as they select
#'  a contiguous, ascending range (e.g. `10:20` or `-(1:5)`), since the result
#'  must again have evenly spaced time and frequency axes.
#'  A missing index selects everything.
#'  Use [as.matrix()] for arbitrary subsetting.
#'
#' @return
#'  Another "fcwtr_scalogram" object that contains only part of the data.
#'
#' @examples
#' fcwt(
#'   sin((1:5000) * 2 * pi * 440 / 44100),
#'   x_sample_freq = u(44100, "Hz"),
#'   y_sample_freq = u(44100, "Hz"),
#'   n_freqs = 10
#' )[1:1000, 2:7]
#'
#' @export
`[.fcwtr_scalogram` <- function(x, i, j) {
  i <- if (missing(i)) seq_len(sc_dim_time(x)) else contiguous_index(i, sc_dim_time(x), "time (row)")
  j <- if (missing(j)) seq_len(sc_dim_freq(x)) else contiguous_index(j, sc_dim_freq(x), "frequency (column)")

  # columns are ordered from the highest to the lowest frequency
  freqs <- sc_freqs(x)

  new_fcwtr_scalogram(
    unclass(x)[i, j, drop = FALSE],
    attr(x, "time_offset") + (i[[1]] - 1) / attr(x, "sample_freq"),
    attr(x, "sample_freq"),
    freq_begin = freqs[[j[[length(j)]]]],
    freq_end = freqs[[j[[1]]]],
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
#'     x_sample_freq = u(44100, "Hz"),
#'     freq_begin = u(50, "Hz"),
#'     freq_end = u(1000, "Hz"),
#'     n_freqs = 10,
#'     sigma = 5
#'   )
#'
#' plot(res, time_unit = "ms")
plot.fcwtr_scalogram <- function(x, n = 1000, time_unit = NULL, freq_unit = "Hz",
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
#'  A time unit that is used for the x-axis scale, e.g. `"s"` or `"ms"`.
#'  By default (`NULL`), a suitable unit (s, ms, us, or ns) is chosen depending
#'  on the length of the scalogram.
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
#'  Additional arguments are ignored.
#' @return
#'  A ggplot object.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#'
#' res <-
#'   fcwt(
#'     sin((1:4410) * 2 * pi * 440 / 44100),
#'     x_sample_freq = u(44100, "Hz"),
#'     freq_begin = u(50, "Hz"),
#'     freq_end = u(1000, "Hz"),
#'     n_freqs = 10,
#'     sigma = 5
#'   )
#'
#' autoplot(res, time_unit = "ms")
#'
#' @exportS3Method ggplot2::autoplot
autoplot.fcwtr_scalogram <- function(object, n = 1000,
                                     time_unit = NULL, freq_unit = "Hz",
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
    # set axis names via labs(), not via the scales' `name` argument:
    # with ggplot2 >= 4.0, units only appends the unit ("[s]") to label titles,
    # an explicit scale name takes precedence and is shown without unit.
    units::scale_x_units(unit = time_unit) +
    units::scale_y_units(unit = freq_unit, transform = transform) +
    ggplot2::labs(x = "Time", y = "Frequency") +
    ggplot2::theme_minimal()
}
