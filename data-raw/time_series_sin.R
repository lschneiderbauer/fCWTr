## code to prepare `sin` dataset goes here

# if we assume a sampling rate of 44100 Hz
sampling_rate <- 44100

# a bunch of superimposed sin waves
# of 440, 880, 100, 500, 1200 and 50 Hz
# assuming a sampling_rate of 44100
time_series_sin <-
  c(
    sin((1:3000) * 2 * pi * 440 / sampling_rate) +
      sin((1:3000) * 2 * pi * 880 / sampling_rate) +
      2 * sin((1:3000) * 2 * pi * 100 / sampling_rate),
    sin((1:3000) * 2 * pi * 500 / sampling_rate) +
      3 * sin((1:3000) * 2 * pi * 1200 / sampling_rate) +
      sin((1:3000) * 2 * pi * 50 / sampling_rate)
  )

usethis::use_data(time_series_sin, overwrite = TRUE)
