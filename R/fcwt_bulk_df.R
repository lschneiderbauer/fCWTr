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
#' For instance, in case of processing a song of three minutes length (assuming
#' a simpling rate of 44100 Hz), the size of the output vector is
#' `3 * 60 seconds * 44100 Hz * 2 * noctaves * nsuboctaves * 4 bytes`,
#' which for e.g. noctaves = 8, and nsuboctaves = 24, equals ~ 24 GB, hence
#' nowadays already at the limit of the hardware of a modern personal computer.
#'
#' In cases where the needed output time-resolution is smaller than the time
#' resolution of the input signal, one can perform the fcwt and reduce the
#' output size by pooling, see `time_resolution` parameter of [fcwt_df].
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
#' @inheritParams fcwt_df
#' @param max_batch_size  The maximal batch size that is used for splitting up
#'                        the input sequence. This limits the maximal memory
#'                        that is used.
#'                        The system tries to find the optimal batch size for
#'                        FFTW processing.
#'                        In case of the parameter not being provided,
#'                        heuristics are used to try to estimate a reasonable
#'                        size.
#' @param create_opt_schemes Creates optimization plans for the corresponding
#'                          batch size and parameters before executing
#'                          the batch-transformations.
#' @seealso fcwt_df create_opt_schemes
#' @export
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom rlang .data
fcwt_bulk_df <- function(time_series,
                         sampling_rate,
                         min_freq = sampling_rate / 2000,
                         nsuboctaves = 12L,
                         time_resolution,
                         nthreads = 8L,
                         max_batch_size = NULL,
                         optplan = F,
                         ...) {
  if (is.null(max_batch_size)) {
    # roughly 4G of memory
    max_batch_size <-
      ceiling(
        4 * 10^9 /
          (2 * freqs_to_noctaves(min_freq, sampling_rate / 2) * nsuboctaves * 4)
      )
  }

  # From FFTW documentation:
  # FTW is best at handling sizes of the form 2^a 3^b 5^c 7^d 11^e 13^f,
  # where e+f is
  # either 0 or 1, and the other exponents are arbitrary. Other sizes are
  # computed by means of a slow, general-purpose routine (which nevertheless
  # retains O(n lg n) performance, even for prime sizes).
  # Transforms whose sizes are powers of 2 are especially fast.
  batch_size <- 2^floor(log2(max_batch_size))
  rlang::inform(
    c("i" = paste0(
      "Batch Size: ", batch_size, " (~ Output Size: ",
      round(batch_size * (2 * freqs_to_noctaves(min_freq, sampling_rate / 2) *
        nsuboctaves * 4) / 10^9, 2), " GB)"
    ))
  )


  rlang::inform("Start batch process ...")
  result.df <- NULL

  pb <- txtProgressBar(min = 0, max = length(time_series), style = 3)
  cursor <- 0
  diff <- 0
  while (cursor < length(time_series) - diff) {
    begin <- cursor + 1
    end <- pmin(cursor + batch_size, length(time_series))

    result_intermediate <-
      fcwt_df(
        time_series[begin:end],
        sampling_rate = sampling_rate,
        min_freq = min_freq,
        nsuboctaves = nsuboctaves,
        time_resolution = time_resolution,
        rm.coi = TRUE,
        nthreads = nthreads,
        optplan = optplan,
        ...
      ) |>
      mutate(
        time_ind = .data$time_ind + floor(cursor / (sampling_rate * time_resolution)),
        time = .data$time_ind * time_resolution
      ) |>
      # remove timeframes w/ any NA-value in it.
      group_by(.data$time_ind, .data$time) |>
      filter(!any(is.na(.data$value))) |>
      ungroup()

    # take into account that some time records are lost due to boundary
    # effect cut off (that's why cursor is not just end + 1)
    cursor <-
      cursor +
      floor((-1 + max(result_intermediate$time_ind) -
        min(result_intermediate$time_ind)) *
        time_resolution * sampling_rate)
    diff <- end - cursor

    if (!is.null(result.df)) {
      result.df <-
        result.df |>
        rows_insert(
          result_intermediate,
          by = "time_ind",
          conflict = "ignore"
        )
    } else {
      result.df <- result_intermediate
    }

    setTxtProgressBar(pb, cursor)
  }

  setTxtProgressBar(pb, length(time_series))
  close(pb)

  class(result.df) <- c("fcwt_df", class(result.df))

  return(result.df)
}
