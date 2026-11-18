fs <- 1000
n <- 2^14
tone <- sin(2 * pi * 73.3 * (seq_len(n) - 1) / fs)

sc_phase <- function(x, ...) {
  fcwt(x, x_sample_freq = fs, freq_begin = 20, freq_end = 200, n_freqs = 40,
       sigma = 2 * pi, phase = TRUE, ...)
}

test_that("phase = TRUE returns the layers", {
  sc <- sc_phase(tone, y_sample_freq = 100)

  expect_true(sc_has_phase(sc))
  expect_equal(dim(sc), c(ceiling(n / 10), 40, 3))
  expect_equal(dimnames(sc)[[3]], c("modulus", "inst_freq", "coherence"))
  expect_true(all(sc_layer(sc, "coherence") <= 1, na.rm = TRUE))
  expect_true(all(sc_layer(sc, "coherence") >= 0, na.rm = TRUE))
})

test_that("the modulus does not depend on `phase`", {
  sc <- sc_phase(tone, y_sample_freq = 100)
  sc0 <- fcwt(tone, x_sample_freq = fs, freq_begin = 20, freq_end = 200,
              n_freqs = 40, sigma = 2 * pi, y_sample_freq = 100)

  expect_equal(as.matrix(sc), as.matrix(sc0))
  expect_equal(sc_layer(sc, "modulus"), as.matrix(sc0))
})

test_that("a tone's frequency is recovered by all bands that respond to it", {
  sc <- sc_phase(tone, y_sample_freq = 100)
  mod <- sc_layer(sc, "modulus")
  freq <- sc_layer(sc, "inst_freq")
  coh <- sc_layer(sc, "coherence")

  # bands with a relevant response (outside the cone of influence)
  responding <- !is.na(mod) & mod > 1e-2 * max(mod, na.rm = TRUE)
  expect_gt(sum(colSums(responding) > 0), 2)

  expect_equal(freq[responding], rep(73.3, sum(responding)), tolerance = 1e-4)

  coh_responding <- coh[responding & !is.na(coh)]
  expect_gt(length(coh_responding), 0)
  expect_gt(min(coh_responding), 0.999)
})

test_that("the instantaneous frequency follows a chirp", {
  t <- (seq_len(n) - 1) / fs
  # from 40 Hz to 120 Hz (instantaneous frequency 40 + 80 * t / T)
  chirp <- sin(2 * pi * (40 * t + 40 * t^2 / max(t)))
  sc <- sc_phase(chirp, y_sample_freq = 10)
  mod <- sc_layer(sc, "modulus")
  freq <- sc_layer(sc, "inst_freq")

  # rows outside the cone of influence of all bands
  rows <- which(rowSums(is.na(mod)) == 0)
  expect_gt(length(rows), 100)
  peak <- vapply(rows, function(r) freq[r, which.max(mod[r, ])], numeric(1))
  expected <- 40 + 80 * (rows - 0.5) / 10 / max(t)

  expect_true(all(diff(peak) > 0))
  expect_equal(peak, expected, tolerance = 2e-2)
})

test_that("the coherence of noise decreases with the window length", {
  set.seed(1)
  noise <- rnorm(n)

  median_coh <- function(y_sample_freq) {
    median(sc_layer(sc_phase(noise, y_sample_freq = y_sample_freq), "coherence"), na.rm = TRUE)
  }

  # windows short compared to the lag: close to 1 by construction
  expect_gt(median_coh(fs), 0.99)
  # long windows
  expect_lt(median_coh(1), 0.7)
  expect_lt(median_coh(1), median_coh(10))
})

test_that("pooling again in R matches pooling in C++", {
  set.seed(2)
  x <- tone + rnorm(n, sd = 0.3)
  fine <- sc_phase(x, y_sample_freq = fs, rm_coi = FALSE)
  coarse <- sc_phase(x, y_sample_freq = fs / 16, rm_coi = FALSE)
  pooled <- sc_agg(fine, wnd_from_dim(16, u(fs, "Hz")))

  expect_equal(dim(pooled), dim(coarse))
  expect_equal(attributes(pooled), attributes(coarse))
  expect_equal(sc_layer(pooled, "modulus"), sc_layer(coarse, "modulus"))

  # approximate: compare where the tone dominates
  mod <- sc_layer(coarse, "modulus")
  strong <- mod > 0.1 * max(mod)
  strong[c(1:50, (nrow(strong) - 50):nrow(strong)), ] <- FALSE
  expect_equal(
    sc_layer(pooled, "inst_freq")[strong],
    sc_layer(coarse, "inst_freq")[strong],
    tolerance = 1e-3
  )
})

test_that("the cone of influence of the coherence is wider", {
  sc <- sc_phase(tone, y_sample_freq = 250)
  dim_t <- sc_dim_time(sc)
  f_rel <- ddu(sc_freqs(sc) / attr(sc, "sample_freq"))
  mask <- function(width) {
    t <- rep(seq_len(dim_t), times = length(f_rel))
    f <- rep(f_rel, each = dim_t)
    m <- t * f < width * 2 * pi | (dim_t - t + 1) * f < width * 2 * pi
    dim(m) <- c(dim_t, length(f_rel))
    m
  }

  expect_equal(is.na(sc_layer(sc, "modulus")), mask(2))
  expect_equal(is.na(sc_layer(sc, "inst_freq")), mask(2))
  expect_equal(is.na(sc_layer(sc, "coherence")), mask(3))
})

test_that("scalogram methods keep the layers", {
  sc <- sc_phase(tone, y_sample_freq = 100)

  sub <- sc[11:100, 3:7]
  expect_true(sc_has_phase(sub))
  expect_equal(dim(sub), c(90, 5, 3))
  expect_equal(sc_layer(sub, "inst_freq"), sc_layer(sc, "inst_freq")[11:100, 3:7])

  both <- rbind(sc, sc)
  expect_equal(dim(both), c(2 * sc_dim_time(sc), 40, 3))
  expect_equal(sc_layer(both, "coherence")[sc_dim_time(sc) + 1:10, ],
               sc_layer(sc, "coherence")[1:10, ])

  sc0 <- fcwt(tone, x_sample_freq = fs, freq_begin = 20, freq_end = 200,
              n_freqs = 40, sigma = 2 * pi, y_sample_freq = 100)
  expect_error(rbind(sc, sc0), "phase information")

  df <- as.data.frame(sc)
  expect_named(df, c("time_index", "time", "freq", "value", "inst_freq", "coherence"))
  expect_equal(as.character(units(df$inst_freq)), as.character(units(df$freq)))
  expect_equal(as.numeric(df$inst_freq), c(sc_layer(sc, "inst_freq")))

  # independent of the units of the arguments
  sc_khz <- fcwt(tone, x_sample_freq = u(1, "kHz"), freq_begin = u(0.02, "kHz"),
                 freq_end = u(0.2, "kHz"), n_freqs = 40, sigma = 2 * pi,
                 y_sample_freq = 100, phase = TRUE)
  df_khz <- as.data.frame(sc_khz)
  expect_equal(as.character(units(df_khz$inst_freq)), as.character(units(df_khz$freq)))
  expect_equal(du(df_khz$inst_freq, "Hz"), du(df$inst_freq, "Hz"))

  expect_output(print(sc), "coherence")
})

test_that("plotting the layers works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")

  sc <- sc_phase(tone, y_sample_freq = 100)
  for (layer in c("modulus", "inst_freq", "coherence")) {
    expect_s3_class(ggplot2::autoplot(sc, layer = layer), "ggplot")
  }

  sc0 <- fcwt(tone, x_sample_freq = fs, freq_begin = 20, freq_end = 200,
              n_freqs = 40, sigma = 2 * pi, y_sample_freq = 100)
  expect_error(ggplot2::autoplot(sc0, layer = "coherence"), "phase information")
})

test_that("fcwt_batch() supports phase information", {
  args <- list(ts_sin_440, x_sample_freq = 44100, y_sample_freq = 4410,
               freq_begin = 50, freq_end = 1000, n_freqs = 10, sigma = 1,
               phase = TRUE)

  single <- do.call(fcwt_batch, args)
  ref <- sc_rm_coi_time_slices(do.call(fcwt, args))
  expect_equal(single, ref)

  batched <- do.call(fcwt_batch, c(args, max_batch_size = 20000))
  expect_equal(dim(batched), dim(ref))
  expect_equal(as.matrix(batched), as.matrix(ref), tolerance = 1e-3)

  # phase information is meaningless without signal: compare where there is
  mod <- sc_layer(ref, "modulus")
  signal <- mod > 1e-3 * max(mod)
  for (layer in c("inst_freq", "coherence")) {
    expect_equal(
      sc_layer(batched, layer)[signal], sc_layer(ref, layer)[signal],
      tolerance = 1e-3
    )
  }
})
