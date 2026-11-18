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
  stopifnot(
    is.matrix(matrix) ||
      (length(dim(matrix)) == 3 &&
        identical(dimnames(matrix)[[3]], sc_layer_names))
  )
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

# layers (third dimension) of a scalogram with phase information
sc_layer_names <- c("modulus", "inst_freq", "coherence")

# Half widths of the cone of influence, in units of the standard deviation
# sigma / f of the wavelet in time:
# - the wavelet coefficients themselves (modulus, instantaneous frequency,
#   whose lag of one sample is negligible): half the support of the wavelet,
# - the phase coherence, whose lag products span 2 sigma / f around their
#   center: one more standard deviation.
coi_width <- 2
coi_width_coherence <- 3

# Number of time slices at each boundary that lie inside the "cone of
# influence" (and are therefore affected by boundary effects), for each
# frequency (column) of a scalogram with `dim_t` time slices.
#
# The standard deviation of the Gaussian wave packet at frequency f is
# sigma / f; we choose 4 sigma / f as the support of a wave packet, so
# boundary effects are expected for time steps t (relative to the boundary,
# in units of the sampling period) with t * f / sample_freq < 2 sigma
# (`width` = 2).
#
# Computed column by column, so that only small temporary objects are needed
# (the mask of the whole scalogram has the same size as the scalogram).
coi_n_slices <- function(dim_t, sample_freq, freq_begin, freq_end, freq_scale,
                         sigma, dim_f, width = coi_width) {
  # frequencies in units of the sampling frequency
  f_rel <- ddu(
    seq2(freq_end, freq_begin, length.out = dim_f, scale = freq_scale) /
      sample_freq
  )

  t <- seq_len(dim_t)
  vapply(f_rel, function(f) sum(t * f < width * sigma), integer(1))
}

# invalid time steps on one side of the time series
# invalid meaning: at least one frequency in that time range has invalid value
coi_invalid_time_steps <- function(sample_freq, freq_begin, sigma,
                                   width = coi_width) {
  as.integer(ddu(floor(width * sigma * sample_freq / freq_begin)))
}

# Does the scalogram contain phase information (layers)?
sc_has_phase <- function(x) {
  length(dim(x)) == 3
}

# One layer of the scalogram, as a plain matrix
sc_layer <- function(x, layer = "modulus") {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  if (sc_has_phase(x)) {
    stopifnot(layer %in% sc_layer_names)
    m <- unclass(x)[, , layer]
  } else {
    if (layer != "modulus") {
      stop(
        "The scalogram does not contain phase information. ",
        "Use `fcwt(..., phase = TRUE)`.",
        call. = FALSE
      )
    }
    m <- unclass(x)
  }

  attributes(m) <- NULL
  dim(m) <- c(sc_dim_time(x), sc_dim_freq(x))
  m
}

# Subset of the data (array without attributes) by time and frequency,
# keeping all layers
sc_subset <- function(x, i, j) {
  a <- unclass(x)

  if (sc_has_phase(x)) {
    a[i, j, , drop = FALSE]
  } else {
    a[i, j, drop = FALSE]
  }
}

# Bind (unclassed) scalogram data in time direction
bind_time <- function(arrs) {
  if (length(dim(arrs[[1]])) == 2) {
    return(do.call(rbind, arrs))
  }

  d <- dim(arrs[[1]])
  layers <- lapply(seq_len(d[[3]]), function(l) {
    do.call(rbind, lapply(arrs, function(a) a[, , l]))
  })

  array(
    unlist(layers, use.names = FALSE),
    dim = c(nrow(layers[[1]]), d[[2]], d[[3]]),
    dimnames = list(NULL, NULL, dimnames(arrs[[1]])[[3]])
  )
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
    sc_subset(x, rows_to_keep, seq_len(sc_dim_freq(x))),
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
      attr(x, "sample_freq"), attr(x, "freq_begin"), attr(x, "sigma"),
      width = if (sc_has_phase(x)) coi_width_coherence else coi_width
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
#
# The modulus is pooled as root mean square (as in the C++ code).
# Instantaneous frequency and coherence are pooled as means weighted by the
# energy (squared modulus); this approximates computing them directly from the
# lag products of the larger window.
sc_agg <- function(x, wnd) {
  stopifnot(inherits(x, "fcwtr_scalogram"))

  poolsize <- wnd$size_n
  n <- floor(sc_dim_time(x) / poolsize)

  if (poolsize <= 1) {
    # do nothing in case we cannot aggregate
    return(x)
  }

  dim_f <- sc_dim_freq(x)
  rows <- seq_len(poolsize * n)

  # (windowed) mean of a time x frequency matrix, ignoring NA values
  pool <- function(m) {
    m <- m[rows, , drop = FALSE]
    dim(m) <- c(poolsize, n, dim_f)
    res <- colMeans(m, dims = 1, na.rm = TRUE)
    dim(res) <- c(n, dim_f)
    res
  }

  energy <- sc_layer(x, "modulus")^2
  modulus <- sqrt(pool(energy))

  x_new <-
    if (sc_has_phase(x)) {
      weighted <- function(layer) {
        v <- sc_layer(x, layer)
        e <- energy
        e[is.na(v)] <- NA_real_
        pool(e * v) / pool(e)
      }

      array(
        c(modulus, weighted("inst_freq"), weighted("coherence")),
        dim = c(n, dim_f, 3L),
        dimnames = list(NULL, NULL, sc_layer_names)
      )
    } else {
      modulus
    }

  # replace NaN by NA
  x_new[is.nan(x_new)] <- NA_real_

  fcwtr_scalogram(
    x_new,
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
    "Sigma parameter needs to be identical." = arg_attr_ident("sigma"),
    "Either all or none of the scalograms need to contain phase information." =
      length(unique(vapply(args, sc_has_phase, logical(1)))) == 1
  )

  x_new <- bind_time(lapply(args, unclass))

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
  if (sc_has_phase(x)) {
    cat("* Layers: ", paste(sc_layer_names, collapse = ", "), "\n", sep = "")
  }
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

  if (sc_has_phase(x)) {
    for (layer in sc_layer_names) {
      cat("* Time/frequency matrix summary: ", layer, "\n", sep = "")
      print(summary(as.vector(sc_layer(x, layer))))
    }
  } else {
    cat("* Time/frequency matrix summary\n")
    print(summary(as.vector(as.matrix(x))))
  }

  invisible(x)
}


#' Convert the scalogram matrix to a data frame
#'
#' Internally, the scalogram resulting from [fcwt()] is represented by
#' a numeric [matrix()]. This method converts this matrix into a
#' [data.frame()] in long format, with one row per matrix entry.
#' With phase information (`fcwt(..., phase = TRUE)`), the additional
#' columns `inst_freq` and `coherence` hold the other layers.
#' Note that this conversion has a significant run time cost.
#'
#' @param x
#'  An "fcwtr_scalogram" object resulting from [fcwt()].
#'
#' @param ...
#'  Additional arguments are ignored.
#'
#' @return
#'  A [data.frame()] object representing the scalogram data with four
#'  (six, with phase information) columns:
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
#'   \item{value}{The fCWT result (modulus) for the particular time-frequency
#'                combination.}
#'   \item{inst_freq}{Only with phase information: the instantaneous frequency,
#'                in the same units as `freq`.}
#'   \item{coherence}{Only with phase information: the phase coherence.}
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

  modulus <- sc_layer(x, "modulus")
  t_idx <- dim_t[row(modulus)] - 1

  df <- data.frame(
    time_index = t_idx,
    # in a time unit (s, ms, ...) that suits the magnitude of the values
    time = pretty_time(
      t_idx / attr(x, "sample_freq") + attr(x, "time_offset")
    ),
    freq = dim_f[col(modulus)],
    value = c(modulus)
  )

  if (sc_has_phase(x)) {
    # in the same unit as freq
    df$inst_freq <- conv(
      u(c(sc_layer(x, "inst_freq")), "Hz"), units::deparse_unit(dim_f)
    )
    df$coherence <- c(sc_layer(x, "coherence"))
  }

  df
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
#'  A [tibble::tibble()] object representing the scalogram data with four
#'  (six, with phase information) columns:
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
#'   \item{value}{The fCWT result (modulus) for the particular time-frequency
#'                combination.}
#'   \item{inst_freq}{Only with phase information: the instantaneous frequency,
#'                in the same units as `freq`.}
#'   \item{coherence}{Only with phase information: the phase coherence.}
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
#' a pure matrix. For a scalogram with phase information
#' (`fcwt(..., phase = TRUE)`), this is the modulus.
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
  sc_layer(x, "modulus")
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
    sc_subset(x, i, j),
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
                                 transf = identity,
                                 layer = c("modulus", "inst_freq", "coherence"),
                                 ...) {
  print(autoplot.fcwtr_scalogram(x, n, time_unit, freq_unit, transf, layer, ...))
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
#' @param layer
#'  Which quantity to plot: `"modulus"` (default), or, for scalograms with
#'  phase information (`fcwt(..., phase = TRUE)`), `"inst_freq"` or
#'  `"coherence"`. The instantaneous frequency is shown as relative deviation
#'  from the frequency of the band, `inst_freq / freq - 1`.
#'  Both phase quantities are meaningless where there is (almost) no signal,
#'  so their tiles are faded out according to the modulus: the opacity is
#'  `sqrt(modulus / max(modulus))`.
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
                                     transf = identity,
                                     layer = c("modulus", "inst_freq", "coherence"),
                                     ...) {
  layer <- match.arg(layer)
  if (layer != "modulus" && !sc_has_phase(object)) {
    stop(
      "The scalogram does not contain phase information. ",
      "Use `fcwt(..., phase = TRUE)`.",
      call. = FALSE
    )
  }

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

  # opacity of the phase quantities: fade out tiles without signal
  # (the square root keeps weaker, but real, components visible)
  df[["opacity"]] <- sqrt(df[["value"]] / max(df[["value"]], na.rm = TRUE))

  df[["value"]] <-
    switch(layer,
      modulus = df[["value"]],
      inst_freq = ddu(df[["inst_freq"]] / df[["freq"]]) - 1,
      coherence = df[["coherence"]]
    )

  # transform the value if requested
  df[["value"]] <- transf(df[["value"]])

  fill_scale <-
    switch(layer,
      modulus = viridis::scale_fill_viridis(discrete = FALSE),
      inst_freq = {
        # color range from the tiles with signal (not the faded ones),
        # symmetric around 0; values beyond are clamped
        visible <- df[["opacity"]] >= 0.1 & is.finite(df[["value"]])
        lim <- max(abs(df[["value"]][visible]), 0)
        if (lim == 0) lim <- 1
        ggplot2::scale_fill_gradient2(
          name = "rel. freq.\ndeviation",
          limits = c(-lim, lim),
          oob = function(x, range) pmin(pmax(x, range[[1]]), range[[2]])
        )
      },
      # coherent: dark (tiles without signal fade out to the background)
      coherence = viridis::scale_fill_viridis(
        discrete = FALSE, name = "coherence", direction = -1
      )
    )

  raster <-
    if (layer == "modulus") {
      geom_raster()
    } else {
      geom_raster(aes(alpha = .data$opacity))
    }

  df |>
    ggplot(aes(x = .data$time, y = .data$freq, fill = .data$value)) +
    raster +
    fill_scale +
    ggplot2::scale_alpha_identity() +
    # set axis names via labs(), not via the scales' `name` argument:
    # with ggplot2 >= 4.0, units only appends the unit ("[s]") to label titles,
    # an explicit scale name takes precedence and is shown without unit.
    units::scale_x_units(unit = time_unit) +
    units::scale_y_units(unit = freq_unit, transform = transform) +
    ggplot2::labs(x = "Time", y = "Frequency") +
    ggplot2::theme_minimal()
}
