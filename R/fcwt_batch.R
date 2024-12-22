#' Fast continuous wavelet transform - Batch processing
#'
#' Performs a fast continuous wavelet transform on long sequences by sequentially
#' processing junks of the input signal and keeping only low-resolution output
#' data to preserve memory.
#' This is only useful for very long signals whose output does not fit into
#' memory as a whole.
#' It should not be used on short signals since boundary artefacts are discarded
#' (and those potentially dominate for short sequences).
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
#' Attention: Boundary artefacts are removed, so some high frequency information
#' at the beginning and the end of the sequence is lost. (The amount depends on
#' the minimal frequency captured `min_freq`.)
#'
#' @param max_batch_size
#'  The maximal batch size that is used for splitting up the input sequence.
#'  This limits the maximal memory that is used. Defaults to roughly 1GB, being
#'  conservative and taking into account that R might make copies when further
#'  processing it.
#'  The actual batch size depends on the requested `time_resolution`.
#' @param time_resolution
#'  The time resolution in inverse units of `sample_freq` of the result.
#'  Memory consumption is directly related to that.
#'  Can not be higher than the time resolution of the input signal.
#' @param progress_bar
#'  Monitoring progress can sometimes be useful when performing time consuming
#'  operations. Setting `progress_bar = TRUE` enables printing a progress
#'  bar to the console. Defaults to `FALSE`.
#'
#' @return
#'  The spectogram, a numeric real-valued matrix with dimensions roughly
#'  `dim ~ c(length(signal) * time_resolution * sample_freq, n_freqs)`.
#'  The exact length of the output depends on boundary effect details.
#'  This matrix is wrapped into a S3-class `fcwtr_scalogram` so that plotting and
#'  coercion functions can be used conveniently.
#'
#' @inheritParams fcwt
#' @seealso [fcwt()]
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#' @examples
#' res <-
#'   fcwt_batch(
#'     ts_sin_sin,
#'     sample_freq = 44100,
#'     freq_begin = 100,
#'     freq_end = 11000,
#'     n_freqs = 10,
#'     sigma = 10,
#'     max_batch_size = 50000,
#'     time_resolution = 0.001
#'   )
fcwt_batch <- function(signal,
                       sample_freq,
                       n_freqs,
                       time_resolution,
                       freq_begin = 2 * sample_freq / length(signal),
                       freq_end = sample_freq / 2,
                       freq_scale = c("linear", "log"),
                       sigma = 1,
                       # factor 4 as additional security measure
                       max_batch_size = ceiling(1 * 10^9 / (n_freqs * 8) / 4),
                       n_threads = 2L,
                       progress_bar = FALSE) {

  # aggregation window
  w <- wnd_from_resolution(time_resolution, sample_freq)

  # From FFTW documentation:
  # FTW is best at handling sizes of the form 2^a 3^b 5^c 7^d 11^e 13^f,
  # where e+f is
  # either 0 or 1, and the other exponents are arbitrary. Other sizes are
  # computed by means of a slow, general-purpose routine (which nevertheless
  # retains O(n lg n) performance, even for prime sizes).
  # Transforms whose sizes are powers of 2 are especially fast.

  # we also want the batch size to be a multiple of the
  # window size
  # batch_size <- 2^floor(log2(max_batch_size))
  batch_size <- floor(max_batch_size / w$size_n) * w$size_n

  signal_size <- w$size_n * floor(length(signal) / w$size_n) # cut off the rest

  total_result <- NULL

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = length(signal), style = 3)

    on.exit({
      setTxtProgressBar(pb, length(signal))
      close(pb)
    })
  }

  cursor <- 0
  diff <- 0
  while (cursor < length(signal) - diff) {
    begin <- cursor + 1
    end <- pmin(cursor + batch_size, signal_size)

    result_raw <-
      fcwt(
        signal[begin:end],
        sample_freq = sample_freq,
        freq_begin = freq_begin,
        freq_end = freq_end,
        n_freqs = n_freqs,
        freq_scale = freq_scale,
        sigma = sigma,
        remove_coi = TRUE,
        n_threads = n_threads
      )

    time_index_interval <- sc_coi_time_interval(result_raw)

    if (any(is.na(time_index_interval))) {
      stop(paste0(
        "Removing COI yields empty result. Typically that happens if ",
        "the batch size is too small. ",
        "To avoid that you can either increase the batch size or raise the ",
        "frequency range."
      ))
    }

    result_agg <-
      result_raw |>
      sc_agg(w) |>
      sc_rm_coi_time_slices() # we fully remove COI infected time slices

    # take into account that some time records are lost due to boundary
    # effect cut off (that's why cursor is not just end + 1)
    # we have two compensate two times the half-boundary
    cursor <- cursor + (1 + end - begin) - (2 * (time_index_interval[[1]] - 1))
    diff <- end - cursor

    if (!is.null(total_result)) {
      total_result <-
        tbind(
          total_result,
          result_agg
        )
    } else {
      total_result <- result_agg
    }

    if (progress_bar) setTxtProgressBar(pb, cursor)
  }

  return(total_result)
}
