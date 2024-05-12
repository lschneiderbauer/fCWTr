test_that("fctw_raw works on examples", {
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
      sigma = 1,
      abs = TRUE
    ),
    44100 * 10
  )
})
