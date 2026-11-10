#' Fast continuous wavelet transform - Batch processing
#'
#' Performs a fast continuous wavelet transform on long sequences by sequentially
#' processing chunks (batches) of the input signal.
#' This is only useful for very long signals that cannot be processed by
#' [fcwt()] in one go.
#' It should not be used on short signals since boundary artefacts are
#' automatically discarded (and those potentially dominate for short signals).
#'
#' @details
#' [fcwt()] computes the result frequency by frequency and averages over time
#' on the fly (see its `y_sample_freq` argument), so the size of its output
#' is usually not the limiting factor. However, it performs Fourier transforms
#' over the *whole* input signal. This requires memory proportional to the
#' signal length (several single-precision complex copies of the signal,
#' zero-padded to the next power of two), and the signal length is limited to
#' \eqn{2^{30}} samples. For instance, a recording of 24 hours at a sampling
#' rate of 44100 Hz has about \eqn{3.8 \cdot 10^9} samples.
#'
#' This function splits up the input sequence into overlapping batches,
#' processes each batch separately with [fcwt()], removes the boundary
#' artefacts of each batch, and joins the outputs together.
#'
#' Attention: In contrast to [fcwt()] boundary artefacts are automatically removed,
#' so some information at the beginning and the end of the
#' sequence is lost. (The amount depends on the lowest frequency `freq_begin`,
#' since low-frequency wavelets are spread out the most in time.)
#'
#' @param max_batch_size
#'  The maximal batch size (number of input samples) that is used for
#'  splitting up the input sequence. This limits the memory that is needed
#'  while processing a batch, which is roughly
#'  \deqn{50 + 16 \cdot \frac{n_{freqs}}{k}}{50 + 16 * n_freqs / k}
#'  bytes per input sample, where \eqn{k = x_{sample\_freq} / y_{sample\_freq}}
#'  is the number of input samples that are averaged into one output sample:
#'  about 50 bytes are needed for the transform itself, the rest for the
#'  output of the batch (and a temporary copy of it).
#'  The default is the largest power of two for which this amounts to at
#'  most about 500 MB. (The Fourier transforms are fastest for powers of two,
#'  other sizes are zero-padded to the next power of two, which also increases
#'  the memory needed for the transform.)
#'
#'  Independent of the batch size, the outputs of all batches are kept and
#'  finally joined, which needs about twice the memory of the final result.
#'
#'  The actual batch size is the largest batch size that is not larger than
#'  `max_batch_size` and compatible with the requested `y_sample_freq`.
#'  Each batch loses about `4 * sigma * x_sample_freq / freq_begin` samples to
#'  boundary effects, so for low values of `freq_begin`, increase the batch
#'  size as far as your memory allows (see also the loss ratio reported with
#'  `progress_bar = TRUE`).
#'
#' @param progress_bar
#'  Monitoring progress can sometimes be useful when performing time consuming
#'  operations. Setting `progress_bar = TRUE` enables printing a progress
#'  bar to the console, printing the "loss ratio" and the number of batches.
#'  The loss ratio is a number
#'  between 0 and 1 and indicates how much of the batch computation has to be
#'  thrown away due to boundary artefacts. The higher the batch size the smaller
#'  the loss ratio will be.
#'  Defaults to `FALSE`.
#'
#' @return
#'  The scalogram, a numeric real-valued matrix with dimensions roughly
#'  `dim ~ c(length(x) * y_sample_freq / x_sample_freq, n_freqs)`.
#'  The exact length of the output depends on boundary effect details.
#'  This matrix is wrapped into a S3-class "fcwtr_scalogram" so that plotting and
#'  coercion functions can be used conveniently.
#'
#' @inheritParams fcwt
#' @seealso [fcwt()]
#' @examples
#' fcwt_batch(
#'   ts_sin_sin,
#'   x_sample_freq = u(44.1, "kHz"),
#'   sigma = 10,
#'   y_sample_freq = u(100, "Hz"),
#'   freq_begin = u(100, "Hz"),
#'   freq_end = u(11000, "Hz"),
#'   n_freqs = 30
#' )
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
fcwt_batch <- function(x,
                       x_sample_freq,
                       sigma = 2 * pi,
                       y_sample_freq = 3 / sigma_res(sigma, freq_end)$time,
                       freq_begin = 2 * x_sample_freq / length(x),
                       freq_end = x_sample_freq / 2,
                       n_freqs =
                         # same default as fcwt() (see there)
                         3 * ceiling(
                           log(
                             du(freq_end / freq_begin),
                             base = 1 + sigma_freq_res_rel(sigma)
                           )
                         ),
                       freq_scale = c("log", "linear"),
                       max_batch_size =
                         default_max_batch_size(
                           x_sample_freq, y_sample_freq, n_freqs
                         ),
                       n_threads = 2L,
                       progress_bar = FALSE) {
  x_sample_freq <- as_freq(x_sample_freq)
  freq_begin <- as_freq(freq_begin)
  freq_end <- as_freq(freq_end)
  y_sample_freq <- as_freq(y_sample_freq)

  stopifnot(
    y_sample_freq > u(0, "Hz"),
    y_sample_freq <= x_sample_freq
  )

  # From FFTW documentation:
  # FTW is best at handling sizes of the form 2^a 3^b 5^c 7^d 11^e 13^f,
  # where e+f is
  # either 0 or 1, and the other exponents are arbitrary. Other sizes are
  # computed by means of a slow, general-purpose routine (which nevertheless
  # retains O(n lg n) performance, even for prime sizes).
  # Transforms whose sizes are powers of 2 are especially fast.

  # aggregation window
  w <- wnd_from_target_sample_freq(y_sample_freq, x_sample_freq)

  # time steps we need to overlap because we need to remove the
  # boundary effects
  # make it also a multiple of the window size
  dt <-
    floor(
      coi_invalid_time_steps(x_sample_freq, freq_begin, sigma) / w$size_n
    ) * w$size_n

  # we also want the batch size to be a multiple of the
  # window size
  # batch_size <- 2^floor(log2(max_batch_size))

  # we want (batch_size - 2 * dt) be a multiple of the window size
  # batch_size <- 2 * dt + floor((max_batch_size - 2 * dt) / w$size_n) * w$size_n

  # we want batch_size be a multiple of the window size
  batch_size <- floor(max_batch_size / w$size_n) * w$size_n

  # signal_size <- w$size_n * floor(length(x) / w$size_n) # cut off the rest
  signal_size <- length(x)

  loss_ratio <- 2 * dt / batch_size

  if (loss_ratio >= 1) {
    stop(paste0(
      "The batch size is too small: low frequencies cannot be detected. ",
      "To avoid that you can either increase the batch size or raise the ",
      "minimum frequency.\n",
      "Keeping the minimum frequency unchanged, you should have at least a batch size of: ",
      ceiling(2 * dt / 0.1), "."
    ))
  }

  if (loss_ratio >= 0.3) {
    warning(paste0(
      "The loss ratio is quite high: ", format(loss_ratio), "\n",
      "Consider increasing the batch size to increase performance."
    ))
  }

  btchs <- batches(batch_size, signal_size, dt)

  if (progress_bar) {
    cat(paste0("Loss ratio: ", format(loss_ratio), "\n"))
    cat(paste0("Number of batches: ", length(btchs), "\n"))

    pb <- txtProgressBar(min = 0, max = signal_size, style = 3)

    on.exit({
      setTxtProgressBar(pb, signal_size)
      close(pb)
    })
  }

  lapply(
    btchs,
    function(batch) {
      begin <- batch[[1]]
      end <- batch[[2]]

      if (progress_bar) setTxtProgressBar(pb, begin)

      fcwt(
        x[begin:end],
        x_sample_freq = x_sample_freq,
        y_sample_freq = y_sample_freq,
        freq_begin = freq_begin,
        freq_end = freq_end,
        n_freqs = n_freqs,
        freq_scale = freq_scale,
        sigma = sigma,
        rm_coi = FALSE,
        n_threads = n_threads
      ) |>
        # we fully remove COI infected time slices
        # dt is a multiple of window size per definition
        sc_rm_bdry_time_slices(as.integer(dt/w$size_n))
    }
  ) |>
    do.call(rbind, args = _)
}

batches <- function(batch_size, signal_size, dt) {
  if (batch_size > signal_size) {
    return(list(c(1, signal_size)))
  }

  batch_starts <-
    seq(1, signal_size, by = batch_size - 2 * dt)

  batch_stops <-
    c(seq(batch_size, signal_size, by = batch_size - 2 * dt), signal_size)

  # if batch_stops is a size smaller than batch_starts, it means, that
  # we can actually get rid of the last batch altogether.
  batch_starts <- batch_starts[seq_along(batch_stops)]

  m <- matrix(c(batch_starts, batch_stops), ncol = 2)
  split(m, row(m))
}

# Default batch size, such that processing a batch needs about `memory` bytes.
# Measured memory needs per input sample of a batch:
# - about 50 bytes for the transform itself (FFT buffers, copies of the
#   signal), independent of n_freqs, and
# - the batch's output, n_freqs values (8 bytes each) per k input samples
#   (k: number of input samples averaged into one output sample), which is
#   held about twice (result, and a copy when removing the boundaries).
default_max_batch_size <- function(x_sample_freq, y_sample_freq, n_freqs,
                                   memory = 5e8) {
  k <- wnd_from_target_sample_freq(
    as_freq(y_sample_freq), as_freq(x_sample_freq)
  )$size_n
  bytes_per_sample <- 50 + 2 * 8 * n_freqs / k

  # a power of two: the FFT pads the signal to the next power of two anyway
  # (which would increase the memory needed), and is fastest for those sizes
  2^floor(log2(memory / bytes_per_sample))
}
