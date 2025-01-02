test_that("uncertinaty principle", {
  res <- sigma_resolution(10, u(20, "Hz"))
  res2 <- sigma_resolution(1, u(30, "Hz"))

  expect_equal(du(res$time * res$freq), 8/( pi))
  expect_equal(du(res2$time * res2$freq), 8/(pi))
})

test_that("sigma calculator", {
  expect_equal(
    sigma_from_frequency_resolution(u(1, "Hz"), u(10, "Hz")),
    6.366198,
    tolerance = 10^-4
  )
  expect_equal(
    sigma_from_time_resolution(u(1, "s"), u(10, "Hz")),
    2.5
  )
})

test_that("sigma errs", {
  expect_error(sigma_resolution(-10, u(10, "Hz")))
  expect_error(sigma_from_frequency_resolution(-u(10, "Hz"), u(10, "Hz")))
  expect_error(sigma_from_time_resolution(-u(10, "s"), u(10, "Hz")))
})
