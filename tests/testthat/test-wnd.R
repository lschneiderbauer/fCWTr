test_that("wnd_from_dim works", {
  w <- wnd_from_dim(10, u(10, "Hz"))

  expect_equal(w$size_n, 10)
  expect_true(w$size_time == u(1, "s"))
})

test_that("wnd_from_sec works", {
  w <- wnd_from_time(u(10, "s"), u(10, "Hz"))

  expect_equal(w$size_n, 100)
  expect_true(w$size_time == u(10, "s"))
})


test_that("wnd_from_resolution works", {
  w <- wnd_from_resolution(u(1, "s"), u(100, "Hz"))

  expect_equal(w$size_n, 100)
  expect_true(w$size_time == u(1, "s"))
})

test_that("wnd_from_target_size works", {
  res <-
    fcwt(
      ts_sin_440[1:1000],
      sample_freq = 44100,
      freq_begin = 50,
      freq_end = 1000,
      n_freqs = 10,
      sigma = 1
    )

  w <- wnd_from_target_size(100, res)

  expect_equal(w$size_n, 10)
  expect_true(w$size_time == 10 / u(44100, "Hz"))
})
