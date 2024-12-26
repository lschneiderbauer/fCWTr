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
    sc_rm_coi_time_slices()

  res_batch <-
    fcwt_batch(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1,
      # no aggregation
      time_resolution = 1 / 44100
    )

  res_batch_2 <-
    fcwt_batch(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1,
      # 2 batches
      max_batch_size = 30000,
      # no aggregation
      time_resolution = 1 / 44100
    )

  res_batch_3 <-
    fcwt_batch(
      ts_sin_440,
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1,
      # 3 batches
      max_batch_size = 20000,
      # no aggregation
      time_resolution = 1 / 44100
    )

  expect_equal(
    sc_dim_time(res0), sc_dim_time(res_batch)
  )
  expect_equal(
    sc_dim_time(res0), sc_dim_time(res_batch_2)
  )
  expect_equal(
    sc_dim_time(res0), sc_dim_time(res_batch_3)
  )

  expect_equal(
    res0,
    res_batch
  )

  expect_equal(
    res0,
    res_batch_2,
    # we still have boundary effects that influence the values
    # to some degree
    tolerance = 10^-3
  )

  expect_equal(
    res0,
    res_batch_3,
    # we still have boundary effects that influence the values
    # to some degree
    tolerance = 10^-3
  )
})

test_that("fcwt_batch progress bar does not err", {
  expect_no_error(
    capture.output(
      fcwt_batch(
        ts_sin_440,
        sample_freq = 44100,
        freq_begin = 50,
        freq_end = 1000,
        n_freqs = 10,
        sigma = 1,
        # no aggregation
        time_resolution = 1 / 44100,
        progress_bar = TRUE
      )
    )
  )
})
