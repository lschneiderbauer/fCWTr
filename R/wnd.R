new_wnd <- function(size_n, size_time) {
  structure(
    list(
      size_n = size_n,
      size_time = size_time
    ),
    class = "wnd"
  )
}

#' @param n window length in discrete time steps (sampling steps)
#' @noRd
wnd_from_dim <- function(n, orig_sample_freq) {
  new_wnd(
    size_n = n,
    size_time = n / orig_sample_freq
  )
}

#' @param time target window length in seconds or similar time units.
#'              The exact window length
#'             depends on the original sample frequency `orig_sample_freq`
#' @noRd
wnd_from_time <- function(time, orig_sample_freq) {
  n <- round(ddu(time * orig_sample_freq))

  wnd_from_dim(n, orig_sample_freq)
}

#' @param new_resolution relative window specified by a target resolution
#'                       in seconds (distance between discrete time steps)
#' @noRd
wnd_from_resolution <- function(target_resolution, orig_sample_freq) {
  wnd_from_dim(
    ddu(floor(orig_sample_freq * target_resolution)),
    orig_sample_freq
  )
}

wnd_from_target_sample_freq <- function(target_sample_freq, orig_sample_freq) {
  wnd_from_dim(
    ddu(floor(orig_sample_freq / target_sample_freq)),
    orig_sample_freq
  )
}

#' @param target_size New total target size of `sc`.
#' @noRd
wnd_from_target_size <- function(target_size, sc) {
  stopifnot(inherits(sc, "fcwtr_scalogram"))

  wnd_from_dim(
    floor(sc_dim_time(sc) / target_size),
    attr(sc, "sample_freq")
  )
}

wnd_new_sc_dim <- function(wnd, sc_length) {
  ceiling(sc_length / wnd$size_n)
}
