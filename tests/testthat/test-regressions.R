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
