test_that("fcwt() returns a vector of expected length", {
  res1 <-
    fcwt(
      ts_sin_440[1:1000],
      x_sample_freq = 44100,
      y_sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  expect_equal(sc_dim_freq(res1), 10)
  expect_equal(sc_dim_time(res1), 1000)

  res2 <-
    fcwt(
      ts_sin_440[2001:2500],
      x_sample_freq = 44100,
      y_sample_freq = 44100,
      freq_begin = 30,
      freq_end = 1100,
      n_freqs = 15,
      sigma = 10
    )

  expect_equal(sc_dim_freq(res2), 15)
  expect_equal(sc_dim_time(res2), 500)

  res3 <-
    fcwt(
      ts_sin_440[1:1000],
      x_sample_freq = 44100,
      y_sample_freq = 11025,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  expect_equal(sc_dim_freq(res3), 10)
  expect_equal(sc_dim_time(res3), 250)
})

test_that("fcwt() optional arguments do work", {
  expect_no_error(
    fcwt(
      ts_sin_440[1:1000],
      x_sample_freq = 44100,
      n_freqs = 10
    )
  )
})

test_that("fcwt() removing coi works", {
  res <-
    fcwt(
      ts_sin_440[1:1000],
      x_sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1,
      rm_coi = TRUE
    )

  # we have a time series that is 1000 / 44100 s long
  # that means we can resolve at max 2*44.1 Hz
  # so we expect all rows to be contaminated
  expect_error(
    sc_rm_coi_time_slices(res)
  )
})

test_that("fcwt() errs if frequency specs are higher than Nyquist frequency", {
  expect_error(
    fcwt(
      ts_sin_440[1:1000],
      x_sample_freq = 44100,
      freq_begin = 50,
      freq_end = 25000,
      n_freqs = 10,
      sigma = 1
    )
  )
})

test_that("fcwt() works with linear scale", {
  expect_no_error(
    fcwt(
      ts_sin_440[1:1000],
      x_sample_freq = 44100,
      freq_scale = "linear",
      n_freqs = 10,
      sigma = 1
    )
  )
})

test_that("fcwt() default arguments work", {
  expect_no_error(
    res <- fcwt(ts_sin_440[1:1000], u(44.1, "kHz"))
  )

  expect_equal(sc_dim_freq(res), 174)
  expect_equal(sc_dim_time(res), 63)
})
