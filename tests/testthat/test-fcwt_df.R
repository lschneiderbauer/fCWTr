test_that("multiplication works", {

  fcwt_df(
    ts_sin_440,
    sample_freq = 44100,
    min_freq = 50,
    n_freqs = 10,
    sigma = 1
  )

})
