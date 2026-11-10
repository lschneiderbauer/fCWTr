raw_cwt <- function(x, fs, f0, f1, fn, window = 1L, threads = 1L, linear = TRUE) {
  matrix(
    fcwt_raw(x, fs, f0, f1, fn, 2, window, threads, linear, FALSE),
    ncol = fn
  )
}

test_that("a frequency row does not depend on the other frequencies computed", {
  set.seed(1)
  x <- rnorm(4096)

  # both calls compute the 1 Hz row (last column), plus a different second one
  a <- raw_cwt(x, 1000L, 1, 401, 2L)
  b <- raw_cwt(x, 1000L, 1, 3, 2L)

  expect_equal(a[, 2], b[, 2])
})

test_that("rows are aligned in time for odd signal lengths", {
  x <- numeric(1001)
  x[500] <- 1

  r <- raw_cwt(x, 1000L, 50, 250, 6L)

  expect_equal(unname(apply(r, 2, which.max)), rep(500L, 6))
})

test_that("result does not depend on the number of threads", {
  set.seed(2)
  x <- rnorm(5000)

  expect_equal(
    raw_cwt(x, 1000L, 1, 400, 30L, threads = 1L, linear = FALSE),
    raw_cwt(x, 1000L, 1, 400, 30L, threads = 4L, linear = FALSE)
  )
})

test_that("window averaging computes the mean", {
  x <- sin(2 * pi * 100 * (1:1024) / 1000)

  full <- raw_cwt(x, 1000L, 90, 110, 2L)
  avg <- raw_cwt(x, 1000L, 90, 110, 2L, window = 4L)

  expect_equal(avg[100, ], colMeans(full[397:400, ]))
})

test_that("a single frequency on log scale works", {
  r <- raw_cwt(sin(1:64), 1000L, 10, 100, 1L, linear = FALSE)

  expect_true(all(is.finite(r)))
  expect_gt(max(r), 1e-3)
})

test_that("invalid scales raise an error instead of crashing", {
  # f0 = 0 leads to an infinite scale (bypassing the R-level argument checks)
  expect_error(
    fcwt_raw(sin(1:64), 1000L, 0, 100, 5L, 2, 1L, 1L, FALSE, FALSE),
    "scales are invalid"
  )
})

test_that("non-integer and sub-Hz sample frequencies are supported", {
  # only the ratio of frequencies and sample frequency matters:
  # 2.5 Hz with frequencies f must give the same result as 2500 Hz with 1000 * f
  x <- sin(2 * pi * 0.4 * (1:1000) / 2.5)

  expect_equal(
    fcwt_raw(x, 2.5, 0.1, 1.2, 10L, 2, 1L, 1L, FALSE, FALSE),
    fcwt_raw(x, 2500, 100, 1200, 10L, 2, 1L, 1L, FALSE, FALSE)
  )

  # previously the sample frequency was truncated to an integer (0.5 -> 0)
  sc <- fcwt(x, x_sample_freq = u(500, "mHz"), freq_begin = u(10, "mHz"),
             freq_end = u(200, "mHz"), n_freqs = 5, rm_coi = FALSE)
  expect_true(all(is.finite(as.matrix(sc))))
  expect_gt(max(as.matrix(sc)), 0)
})

test_that("Nyquist check works for odd sample frequencies", {
  # 500.4 Hz is below Nyquist (500.5 Hz) for fs = 1001 Hz
  expect_no_error(
    fcwt_raw(sin(1:256), 1001, 100, 500.4, 5L, 2, 1L, 1L, TRUE, FALSE)
  )
  expect_error(
    fcwt_raw(sin(1:256), 1001, 100, 500.6, 5L, 2, 1L, 1L, TRUE, FALSE),
    "Nyquist"
  )
})

test_that("the scalogram records the actual output sample rate", {
  x <- sin(2 * pi * 440 * (1:44100) / 44100) # 1 s

  # 44100 / 10000 is not an integer: window of 4 samples -> 11025 Hz
  sc <- fcwt(x, x_sample_freq = 44100, y_sample_freq = 10000,
             freq_begin = 100, freq_end = 1000, n_freqs = 5, rm_coi = FALSE)

  expect_equal(as.numeric(attr(sc, "sample_freq")), 11025)
  expect_equal(max(as.numeric(as.data.frame(sc)$time)), 1, tolerance = 1e-3)
})
