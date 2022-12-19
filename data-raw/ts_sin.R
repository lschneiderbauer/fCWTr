## code to prepare `sin` dataset goes here

# if we assume a sampling rate of 44100 Hz
sampling_rate <- 44100

# a bunch of superimposed sin waves
# of 440, 880, 100, 500, 1200 and 50 Hz
# assuming a sampling_rate of 44100
ts_sin_superpos <-
  c(
    sin((1:3000) * 2 * pi * 440 / sampling_rate) +
      sin((1:3000) * 2 * pi * 880 / sampling_rate) +
      2 * sin((1:3000) * 2 * pi * 100 / sampling_rate),
    sin((1:3000) * 2 * pi * 500 / sampling_rate) +
      3 * sin((1:3000) * 2 * pi * 1200 / sampling_rate) +
      sin((1:3000) * 2 * pi * 50 / sampling_rate)
  )

# 1 sec sin signal
ts_sin_440 <- sin((1:44100) * 2 * pi * 440 / sampling_rate)



# 10 sec sin signal with sin-varying frequency
f <- \(t,a) { 1.01*t + (1-cos(a*t))/a }
ts_sin_sin <- sin(2 * pi * 5000 / sampling_rate * f((1:44100), 16*pi / 441000))

usethis::use_data(ts_sin_superpos, ts_sin_440, ts_sin_sin, overwrite = TRUE)

