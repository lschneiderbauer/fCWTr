test_that("fcwt() returns a vector of expected length", {
  expect_length(
    fcwt(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    ),
    44100 * 10
  )

  expect_length(
    fcwt(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 10
    ),
    44100 * 10
  )
})

test_that("fcwt() errs if frequency specs are higher than Nyquist frequency", {
  expect_error(
    fcwt(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 25000,
      n_freqs = 10,
      sigma = 1
    )
  )
})

test_that("fcwt() returns same result", {
  res_vec <-
    round(
      c(
        fcwt(
          ts_sin_440,
          sample_freq = 44100,
          freq_begin = 50,
          freq_end = 1000,
          n_freqs = 10,
          sigma = 1
        ) |>
          agg(1000)
      ),
      digits = 5
    )

  expect_snapshot(
    res_vec
  )
})
