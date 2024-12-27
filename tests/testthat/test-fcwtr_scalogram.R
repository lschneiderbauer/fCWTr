test_that("rbind() for fcwtr_scalogram", {
  first <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      freq_scale = "log",
      sigma = 1
    )

  second <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      freq_scale = "log",
      sigma = 1
    )

  third <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 22100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  expect_s3_class(
    first,
    "fcwtr_scalogram"
  )
  expect_s3_class(
    second,
    "fcwtr_scalogram"
  )
  expect_s3_class(
    rbind(first, second),
    "fcwtr_scalogram"
  )
  expect_error(
    rbind(first, third)
  )
})

test_that("ggplot2::autoplot() does not err", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")

  res <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  expect_no_error(ggplot2::autoplot(res))
  expect_s3_class(ggplot2::autoplot(res), "ggplot")
})

test_that("as.data.frame() result has correct properties", {
  expect_no_error(
    res <-
      as.data.frame(
        fcwt(
          ts_sin_440[1:1000],
          sample_freq = 44100,
          freq_begin = 50,
          freq_end = 1000,
          n_freqs = 10,
          sigma = 1
        )
      )
  )

  time <- res[["time"]]

  expect_gte(min(time), u(0, "s"))
  expect_lte(max(time), u(1, "s"))
  expect_true(has_comp_unit(time, "s"))

  expect_true(has_comp_unit(res[["freq"]], "Hz"))
})

test_that("as.matrix() has the correct S3 class", {
  expect_true(
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    ) |>
      as.matrix() |>
      is.matrix()
  )
})

test_that("`agg()` does not err", {
  prep <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 30,
      sigma = 1
    )

  expect_no_error(
    res <-
      prep |>
      sc_agg(wnd_from_target_size(10, prep))
  )

  expect_s3_class(res, "fcwtr_scalogram")
})

test_that("`agg()` check n_freqs = 1 edge case", {
  prep <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 1,
      sigma = 1
    )

  expect_no_error(
    res <-
      prep |>
      sc_agg(wnd_from_target_size(10, prep))
  )

  expect_s3_class(res, "fcwtr_scalogram")
})
