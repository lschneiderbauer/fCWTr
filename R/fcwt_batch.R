#' FCWT on long sequences
#'
#' Performs a fast continuous wavelet transform on long sequences. It should
#' not be used on short sequences, since boundary artefacts are discarded, and
#' those potentially dominate for short sequences.
#'
#' @details
#' In case of input sequences that exceed the a certain size, the outputl
#' sequence will not fit into the local memory and the fcwt cannot be
#' performed in one run.
#' For instance, in case of processing a song of 10 minutes length (assuming
#' a sampling rate of 44100 Hz), the size of the output vector is
#' `10 * 60 seconds * 44100 Hz * nfreqs * 4 bytes`,
#' which for e.g. `nfreqs = 200`, equals ~ 21 GB, hence
#' nowadays already at the limit of the hardware of a modern personal computer.
#'
#' In cases where the needed output time-resolution is smaller than the time
#' resolution of the input signal, one can perform the fcwt and reduce the
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
#'  This limits the maximal memory that is used. Defaults to roughly 4GB.
#'  The actual batch size is optimized for use with FFTW.
#' @param time_resolution
#'  The time resolution in inverse units of `sample_freq` of the result.
#'  Memory consumption is directly related to that.
#'  Can not be higher than the time resolution of the input signal.
#'
#' @inheritParams fcwt
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
fcwt_batch <- function(signal,
                       sample_freq,
                       freq_begin,
                       freq_end,
                       n_freqs,
                       sigma,
                       max_batch_size = ceiling(4 * 10^9 / (n_freqs * 4)),
                       time_resolution,
                       nthreads = 8L) {
  # From FFTW documentation:
  # FTW is best at handling sizes of the form 2^a 3^b 5^c 7^d 11^e 13^f,
  # where e+f is
  # either 0 or 1, and the other exponents are arbitrary. Other sizes are
  # computed by means of a slow, general-purpose routine (which nevertheless
  # retains O(n lg n) performance, even for prime sizes).
  # Transforms whose sizes are powers of 2 are especially fast.

  batch_size <- 2^floor(log2(max_batch_size))

  total_result <- NULL

  pb <- txtProgressBar(min = 0, max = length(signal), style = 3)
  cursor <- 0
  diff <- 0
  while (cursor < length(signal) - diff) {
    begin <- cursor + 1
    end <- pmin(cursor + batch_size, length(signal))

    n <- (1 + end - begin)
    reduced_n <- ceiling(n / (sample_freq * time_resolution))

    result_intermediate <-
      fcwt(
        signal[begin:end],
        sample_freq = sample_freq,
        freq_begin = freq_begin,
        freq_end = freq_end,
        n_freqs = n_freqs,
        sigma = sigma,
        remove_coi = TRUE
      ) |>
      agg(n = reduced_n)

    result <-
      result_intermediate |>
      rm_na_time_slices() # we fully remove COI infected time slices

    if (dim(result)[[1]] < 1) {
      stop(paste0(
        "Removing COI yields empty result. Typically that happens if ",
        "the batch size is too small. ",
        "To avoid that you can either increase the batch size or raise the ",
        "frequency range."
      ))
    }

    # take into account that some time records are lost due to boundary
    # effect cut off (that's why cursor is not just end + 1)
    # TODO: check if this is really correct (so that we do not have time shifts ...)
    cursor <- cursor + ceiling(dim(result)[[1]] * n / reduced_n)
    diff <- end - cursor

    if (!is.null(total_result)) {
      #print(attr(total_result, "sample_freq"))
      #print(attr(result, "sample_freq"))

      total_result <-
        tbind(
          total_result,
          result
        )
    } else {
      total_result <- result
    }

    setTxtProgressBar(pb, cursor)
  }

  setTxtProgressBar(pb, length(signal))
  close(pb)

  return(total_result)
}
