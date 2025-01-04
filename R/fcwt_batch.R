#' Fast continuous wavelet transform - Batch processing
#'
#' Performs a fast continuous wavelet transform on long sequences by sequentially
#' processing junks of the input signal and keeping only low-resolution output
#' data by averaging to preserve memory.
#' This is only useful for very long signals whose output does not fit into
#' the available memory as a whole.
#' It should not be used on short signals since boundary artefacts are
#' automatically discarded (and those potentially dominate for short signals).
#'
#' @details
#' In case of input sequences that exceed the a certain size, the output
#' sequence will not fit into the local memory and the fcwt cannot be
#' performed in one run.
#' For instance, in case of processing a song of 10 minutes length (assuming
#' a sampling rate of 44100 Hz), the size of the output vector is
#' `10 * 60 seconds * 44100 Hz * nfreqs * 8 bytes`,
#' which for e.g. `nfreqs = 200`, equals ~ 42 GB, hence
#' nowadays already at the limit of the hardware of a modern personal computer.
#'
#' In cases where the required output time-resolution is smaller than the time
#' resolution of the input signal, one can perform the [fcwt()] and reduce the
#' output size by averaging.
#' (The input signal time resolution can in general not be reduced since
#' high-frequency information would get lost.)
#'
#' This function splits up the input sequence into batches, processes each
#' batch separately, reduces the time resolution, and adds the outputs together.
#'
#' Attention: In contrast to [fcwt()] boundary artefacts are automatically removed,
#' so some information at the beginning and the end of the
#' sequence is lost. (The amount depends on the minimal frequency captured `min_freq`.)
#'
#' @param max_batch_size
#'  The maximal batch size that is used for splitting up the input sequence.
#'  This limits the maximal memory that is used. Defaults to roughly 1GB, being
#'  conservative and taking into account that R might make copies when further
#'  processing it.
#'  The actual batch size is the largest batch size that is smaller than
#'  `max_batch_size` and compatible with the requested `y_sample_freq`.
#'  You should aim to set the batch size as large as possible given your
#'  memory constraints (boundary effects become larger the smaller the
#'  batch size).
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
#'  The spectogram, a numeric real-valued matrix with dimensions roughly
#'  `dim ~ c(length(x) * x_sample_freq / y_sample_freq, n_freqs)`.
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
#'   y_sample_freq = u(100, "Hz"),
#'   freq_begin = u(100, "Hz"),
#'   freq_end = u(11000, "Hz"),
#'   n_freqs = 30,
#'   sigma = 10
#' )
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
fcwt_batch <- function(x,
                       x_sample_freq,
                       y_sample_freq = x_sample_freq,
                       n_freqs,
                       freq_begin = 2 * x_sample_freq / length(x),
                       freq_end = x_sample_freq / 2,
                       freq_scale = c("log", "linear"),
                       sigma = 1,
                       # factor 2 as additional security measure
                       max_batch_size = ceiling(1 * 10^9 / (n_freqs * 8) / 2),
                       n_threads = 2L,
                       progress_bar = FALSE) {
  x_sample_freq <- as_freq(x_sample_freq)
  y_sample_freq <- as_freq(y_sample_freq)
  freq_begin <- as_freq(freq_begin)
  freq_end <- as_freq(freq_end)

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
  dt <- coi_invalid_time_steps(x_sample_freq, freq_begin, sigma)

  # we also want the batch size to be a multiple of the
  # window size
  # batch_size <- 2^floor(log2(max_batch_size))

  # we want (batch_size - 2 * dt) be a multiple of the window size
  batch_size <- 2 * dt + floor((max_batch_size - 2 * dt) / w$size_n) * w$size_n

  signal_size <- w$size_n * floor(length(x) / w$size_n) # cut off the rest

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
        remove_coi = FALSE,
        n_threads = n_threads
      ) |>
        # we fully remove COI infected time slices
        sc_rm_coi_time_slices()
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
