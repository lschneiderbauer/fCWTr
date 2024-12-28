test_that("some unit function properties are ok", {
  expect_s3_class(u(10, "m"), "units")

  expect_type(ddu(u(10, "m") / u(1, "m")), "double")

  expect_true(has_comp_unit(u(10, "1/Hz"), "s"))
  expect_false(has_comp_unit(u(10, "1/Hz"), "m"))
})

test_that("number extraction works", {
  expect_equal(sec(u(1, "s")), 1)
  expect_equal(sec(u(1, "ms")), 0.001)
  expect_error(sec(u(1, "m")))
  expect_equal(hz(u(1, "Hz")), 1)
  expect_equal(hz(u(1, "kHz")), 1000)
  expect_error(hz(u(1, "m")))
})

test_that("du() works", {
  x <- u(1, "km")

  expect_equal(du(x, "m"), 1000)
  expect_error(du(x, "Hz"))
})
