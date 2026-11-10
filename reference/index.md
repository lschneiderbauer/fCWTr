# Package index

## Fast continuous wavelet transform

Functions for performing fast continuous wavelet transforms. These
constitue the core functionality of this package.

- [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md)
  : Fast continuous wavelet transform
- [`fcwt_batch()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt_batch.md)
  : Fast continuous wavelet transform - Batch processing

## Sigma helpers

Helpers to setup the correct wavelet spread parameter sigma.

- [`sigma_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_freq_res_rel.md)
  : Calculates Relative Frequency Resolution
- [`sigma_from_freq_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res.md)
  : Determine Sigma from a frequency resolution requirement
- [`sigma_from_freq_res_rel()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_freq_res_rel.md)
  : Determine Sigma from a relative frequency resolution requirement
- [`sigma_from_time_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_from_time_res.md)
  : Determine Sigma from a time resolution requirement
- [`sigma_res()`](https://lschneiderbauer.github.io/fCWTr/reference/sigma_res.md)
  : Calculates Time/Frequency Resolution

## Physical unit helper

- [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md) :
  Augment physical unit
- [`du()`](https://lschneiderbauer.github.io/fCWTr/reference/du.md) :
  Drop physical unit

## Scalogram S3 methods

A data.frame conversion method, data extractors, and optional ggplot2
plotting methods (they only work if ggplot is installed).

- [`as.data.frame(`*`<fcwtr_scalogram>`*`)`](https://lschneiderbauer.github.io/fCWTr/reference/as.data.frame.fcwtr_scalogram.md)
  : Convert the scalogram matrix to a data frame
- [`as_tibble(`*`<fcwtr_scalogram>`*`)`](https://lschneiderbauer.github.io/fCWTr/reference/as_tibble.fcwtr_scalogram.md)
  : Convert the scalogram matrix to a tibble
- [`as.matrix(`*`<fcwtr_scalogram>`*`)`](https://lschneiderbauer.github.io/fCWTr/reference/as.matrix.fcwtr_scalogram.md)
  : Extract the data matrix from a scalogram
- [`` `[`( ``*`<fcwtr_scalogram>`*`)`](https://lschneiderbauer.github.io/fCWTr/reference/sub-.fcwtr_scalogram.md)
  : Extract parts of a scalogram
- [`rbind(`*`<fcwtr_scalogram>`*`)`](https://lschneiderbauer.github.io/fCWTr/reference/rbind.fcwtr_scalogram.md)
  : Combine scalograms in "time" direction
- [`print(`*`<fcwtr_scalogram>`*`)`](https://lschneiderbauer.github.io/fCWTr/reference/print.fcwtr_scalogram.md)
  : Print the scalogram
- [`plot(`*`<fcwtr_scalogram>`*`)`](https://lschneiderbauer.github.io/fCWTr/reference/plot.fcwtr_scalogram.md)
  : Scalogram plotting
- [`autoplot(`*`<fcwtr_scalogram>`*`)`](https://lschneiderbauer.github.io/fCWTr/reference/autoplot.fcwtr_scalogram.md)
  : Create a ggplot object resembling a scalogram

## System support

Helper functions that let you check if your platform provides OpenMP or
AVX support.

- [`avx_enabled()`](https://lschneiderbauer.github.io/fCWTr/reference/avx_enabled.md)
  : Check for AVX instruction set support
- [`openmp_enabled()`](https://lschneiderbauer.github.io/fCWTr/reference/openmp_enabled.md)
  : Check for OpenMP support

## Sample data

Sample signal data mainly used to generate examples. The signals consist
of simple superpositions of sinusoidal patterns.

- [`ts_piano_sample`](https://lschneiderbauer.github.io/fCWTr/reference/ts_piano_sample.md)
  : A simple piano audio sample
- [`ts_sin_440`](https://lschneiderbauer.github.io/fCWTr/reference/ts_sin_440.md)
  : Pure sine wave
- [`ts_sin_sin`](https://lschneiderbauer.github.io/fCWTr/reference/ts_sin_sin.md)
  : A sinusoidal wave with varying frequency
- [`ts_sin_superpos`](https://lschneiderbauer.github.io/fCWTr/reference/ts_sin_superpos.md)
  : Superimposed sine waves of different frequencies and different
  amplitudes
