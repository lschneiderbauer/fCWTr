test_that("openmp_enabled() works", {
  expect_true(
    openmp_enabled() == TRUE |  openmp_enabled() == FALSE
  )
})

test_that("avx_enabled() works", {
  expect_true(
    avx_enabled() == TRUE |  avx_enabled() == FALSE
  )
})
