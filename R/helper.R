# logarithmic spaced sequence
lseq <- function(from, to, length.out) {
  2^(seq(log2(from), log2(to), length.out = length.out))
}

freqs_to_noctaves <- function(min_freq, max_freq) {
  ceiling(log2(max_freq / min_freq))
}
