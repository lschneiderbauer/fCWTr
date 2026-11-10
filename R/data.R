#' Pure sine wave
#'
#' Assuming a sample rate of 44100 Hz, the sine wave's frequency is 440 Hz,
#' and the signal is 1 second long.
#'
#' @format
#' A numeric vector of length 44100 containing a time series signal.
"ts_sin_440"

#' Superimposed sine waves of different frequencies and different amplitudes
#'
#' Assuming a sample rate of 44100 Hz, the signal consists of two consecutive
#' parts of 3000 samples each: superimposed sine waves of 440 Hz, 880 Hz and
#' 100 Hz in the first part, and of 500 Hz, 1200 Hz and 50 Hz in the second part.
#'
#' @format
#' A numeric vector of length 6000 containing a time series signal.
"ts_sin_superpos"

#' A sinusoidal wave with varying frequency
#'
#' The frequency itself is changing in a sinusoidal fashion. Assuming a sample
#' rate of 44100 Hz, the signal is 1 second long and its frequency varies
#' between about 50 Hz and 10 kHz.
#'
#' @format
#' A numeric vector of length 44100 containing a time series signal.
"ts_sin_sin"

#' A simple piano audio sample
#'
#' A piano playing the note A4 (440 Hz). Its sample frequency is 11025 Hz.
#'
#' @format
#' A numeric vector of length 438461 containing a time series signal.
"ts_piano_sample"
