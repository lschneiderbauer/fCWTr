#include "cpp11.hpp"
#include "fcwt.h"
#include <complex>
#include <vector>
#include <cmath>
#include <algorithm>

[[cpp11::register]]
cpp11::r_bool has_openmp() {
  #ifdef SINGLE_THREAD
    return(false);
  #else
    return(true);
  #endif
}

[[cpp11::register]]
cpp11::r_bool has_avx() {
  #ifdef AVX
    return(true);
  #else
    return(false);
  #endif
}

// Returns the CWT, pooled over windows of `window_size` samples, as
// (time x frequency) matrices in column-major order:
// - the modulus, pooled as root mean square sqrt(mean |W|^2), and, if `phase`,
// - the instantaneous frequency (in the unit of fs), and
// - the phase coherence (between 0 and 1),
// stacked as layers (time x frequency x layer).
//
// Phase information is derived from lag products
//   P_d(b) = W(b + d) * conj(W(b)),
// each attributed to (the window containing) its center b + d/2:
// - Frequency of a window, from the lag products with d = 1 sample:
//   f_c + arg(sum P_1 * exp(-i w_c)) / (2 pi), where w_c = 2 pi / scale is the
//   center frequency of the bin (demodulation: the result lies within
//   f_c +- fs / 2, so it does not wrap).
// - Coherence of a window, from the lag products with d = two temporal standard
//   deviations of the wavelet, d = 2 sigma * fs / f = 2 sigma * scale (rounded):
//   |sum P_d| / sum |W(b)| |W(b + d)|.
//   (Over lags short compared to the wavelet, the narrow-band output of a bin
//   looks like a steady oscillation even for noise.)
[[cpp11::register]]
cpp11::writable::doubles fcwt_raw(
    cpp11::doubles signal,
    double fs, // sample frequency (same unit as f0, f1)
    double f0, // beginning of frequency range
    double f1, // end of frequency range
    int fn, // number of wavelets to generate across frequency range
    double sigma,
    int window_size,
    int nthreads,
    bool scaletype, // TRUE = linear, FALSE = log
    bool phase) // also compute instantaneous frequency and coherence
{
  if (signal.size() < 1 || signal.size() > (R_xlen_t)FCWT_MAX_SIZE) {
    cpp11::stop("Signal length must be between 1 and 2^30 (use `fcwt_batch()` for longer signals).");
  }
  if (fn < 1) {
    cpp11::stop("Number of frequencies must be at least 1.");
  }
  if (window_size < 1) {
    cpp11::stop("Window size must be at least 1.");
  }
  if (nthreads < 1) {
    cpp11::stop("Number of threads must be at least 1.");
  }

  int n = signal.size(); //signal length (fits into int, see check above)

  // cast double input to float
  std::vector<float> signal_f(n);
  for (int i = 0; i < n; i++) {
    signal_f[i] = static_cast<float>(signal[i]);
  }

  // number of time steps after averaging over windows
  // (integer ceil division: a float cannot represent large n exactly)
  int target_time_n = (int)(((size_t)n + window_size - 1) / window_size);

  // The result is written directly into an R vector (no copy on return).
  // The complex transform itself is never stored as a whole: every row is
  // processed as soon as it is computed (see the callback below).
  // n * fn can exceed the int range: compute sizes and indices in size_t.
  const size_t layer_size = (size_t)target_time_n * fn;
  const int n_layers = phase ? 3 : 1;
  cpp11::writable::doubles output((R_xlen_t)(layer_size * n_layers));
  double *out = REAL(static_cast<SEXP>(output));

  //Initialize a Morlet wavelet having sigma=sigma;
  Morlet morl(sigma);

  //Create the continuous wavelet transform object
  FCWT fcwt(&morl, nthreads);

  SCALETYPE st = scaletype ? FCWT_LINFREQS : FCWT_LOGSCALES;
  Scales scs(st, fs, f0, f1, fn);

  // Per-window accumulators for the lag products, re-used for every row.
  std::vector<std::complex<double>> acc_freq, acc_coh;
  std::vector<double> acc_coh_abs;
  if (phase) {
    acc_freq.resize(target_time_n);
    acc_coh.resize(target_time_n);
    acc_coh_abs.resize(target_time_n);
  }

  // Modulus (and window averaging), and phase information of one row (frequency)
  auto process_row = [&](int f, const std::complex<float> *row) {
    double *out_mod = out + (size_t)f * target_time_n;

    for (int w = 0; w < target_time_n; w++) {
      double sum = 0;
      size_t begin = (size_t)w * window_size;
      size_t end = std::min(begin + window_size, (size_t)n);
      for (size_t i = begin; i < end; i++) {
        sum += std::norm(std::complex<double>(row[i]));
      }
      out_mod[w] = std::sqrt(sum / (double)(end - begin));
    }

    if (!phase) return;

    double *out_freq = out + layer_size + (size_t)f * target_time_n;
    double *out_coh = out + 2 * layer_size + (size_t)f * target_time_n;

    const double scale = scs.scales[f]; // = fs / f_c
    const double two_pi = 6.283185307179586476925286766559;

    std::fill(acc_freq.begin(), acc_freq.end(), std::complex<double>(0));
    std::fill(acc_coh.begin(), acc_coh.end(), std::complex<double>(0));
    std::fill(acc_coh_abs.begin(), acc_coh_abs.end(), 0.0);

    // frequency: lag 1, the lag product b is attributed to b
    for (long b = 0; b + 1 < (long)n; b++) {
      acc_freq[b / window_size] +=
        std::complex<double>(row[b + 1]) * std::conj(std::complex<double>(row[b]));
    }

    // coherence: lag of two standard deviations of the wavelet,
    // the lag product b is attributed to its center b + lag / 2
    const long lag = std::max(1L, std::lround(2 * sigma * scale));
    const long half_lag = lag / 2;
    for (long b = 0; b + lag < (long)n; b++) {
      std::complex<double> w0(row[b]);
      std::complex<double> w1(row[b + lag]);
      long w = (b + half_lag) / window_size;
      acc_coh[w] += w1 * std::conj(w0);
      acc_coh_abs[w] += std::abs(w0) * std::abs(w1);
    }

    // demodulation by the center frequency
    const std::complex<double> demod = std::polar(1.0, -two_pi / scale);

    for (int w = 0; w < target_time_n; w++) {
      out_freq[w] = (acc_freq[w] == 0.0) ?
        NA_REAL :
        fs * (1 / scale + std::arg(acc_freq[w] * demod) / two_pi);

      out_coh[w] = (acc_coh_abs[w] > 0) ?
        std::min(1.0, std::abs(acc_coh[w]) / acc_coh_abs[w]) :
        NA_REAL;
    }
  };

  fcwt.cwt(signal_f.data(), n, &scs, process_row);

  return output;
}
