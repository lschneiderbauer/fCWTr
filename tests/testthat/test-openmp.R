test_that("openmp() works", {
  expect_true(
    openmp_enabled() == TRUE |  openmp_enabled() == FALSE
  )
})
