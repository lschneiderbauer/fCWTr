# logarithmic spaced sequence
lseq <- function(from, to, length.out) {
  2^(seq(log2(from), log2(to), length.out = length.out))
}
