test_that("fcwt batching yields identical result in single-batch case", {
  res0 <-
    fcwt(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    ) |>
    rm_na_time_slices()

  res_batch <-
    fcwt_batch(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1,
      time_resolution = 1 / 44100
    )

  expect_equal(
    res0,
    res_batch
  )
})

test_that("fcwt batching is roughly equal to non-batching", {
  res0 <-
    fcwt(
      ts_sin_sin,
      sample_freq = 44100,
      freq_begin = 100,
      freq_end = 11000,
      n_freqs = 300,
      sigma = 10,
    ) |>
    rm_na_time_slices()

  res_batch <-
    fcwt_batch(
      ts_sin_sin,
      sample_freq = 44100,
      freq_begin = 100,
      freq_end = 11000,
      n_freqs = 300,
      sigma = 10,
      max_batch_size = 20000,
      time_resolution = 1 / 44100
    )

  expect_equal(
    agg(res0, n = 100),
    agg(res_batch, n = 100),
    tolerance = 10^-2
  )
})
