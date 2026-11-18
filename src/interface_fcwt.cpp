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

  // |W|^2 of the current row, re-used by the phase computations.
  // (All complex arithmetic below is written out in real numbers: with
  // std::complex, compilers call slow library functions for multiplication
  // and absolute values, to handle infinities and NaNs.)
  std::vector<double> norm_buf(phase ? (size_t)n : 0);

  // Modulus (and window averaging), and phase information of one row (frequency)
  auto process_row = [&](int f, const std::complex<float> *row_c) {
    // interleaved real and imaginary parts
    const float *row = reinterpret_cast<const float *>(row_c);
    double *out_mod = out + (size_t)f * target_time_n;

    if (window_size == 1) {
      // no pooling: avoid the overhead of the window loop
      // (separate loops, so that the compiler can vectorize them)
      if (phase) {
        for (int i = 0; i < n; i++) {
          double re = row[2 * (size_t)i];
          double im = row[2 * (size_t)i + 1];
          norm_buf[i] = re * re + im * im;
          out_mod[i] = std::sqrt(norm_buf[i]);
        }
      } else {
        for (int i = 0; i < n; i++) {
          double re = row[2 * (size_t)i];
          double im = row[2 * (size_t)i + 1];
          out_mod[i] = std::sqrt(re * re + im * im);
        }
      }
    } else for (int w = 0; w < target_time_n; w++) {
      double sum = 0;
      size_t begin = (size_t)w * window_size;
      size_t end = std::min(begin + window_size, (size_t)n);
      for (size_t i = begin; i < end; i++) {
        double re = row[2 * i];
        double im = row[2 * i + 1];
        double nrm = re * re + im * im;
        if (phase) norm_buf[i] = nrm;
        sum += nrm;
      }
      out_mod[w] = std::sqrt(sum / (double)(end - begin));
    }

    if (!phase) return;

    double *out_freq = out + layer_size + (size_t)f * target_time_n;
    double *out_coh = out + 2 * layer_size + (size_t)f * target_time_n;

    const double scale = scs.scales[f]; // = fs / f_c
    const double two_pi = 6.283185307179586476925286766559;

    // coherence: lag of two standard deviations of the wavelet,
    // the lag product b is attributed to its center b + lag / 2
    const long lag = std::max(1L, std::lround(2 * sigma * scale));
    const long half_lag = lag / 2;

    // demodulation of the frequency by the center frequency
    const double demod_re = std::cos(two_pi / scale);
    const double demod_im = -std::sin(two_pi / scale);

    for (int w = 0; w < target_time_n; w++) {
      const long begin = (long)w * window_size;
      const long end = std::min(begin + (long)window_size, (long)n);

      // frequency: lag 1, the lag product b is attributed to b
      double p_re = 0, p_im = 0;
      for (long b = begin; b < std::min(end, (long)n - 1); b++) {
        double r0 = row[2 * b], i0 = row[2 * b + 1];
        double r1 = row[2 * b + 2], i1 = row[2 * b + 3];
        // W(b + 1) * conj(W(b))
        p_re += r1 * r0 + i1 * i0;
        p_im += i1 * r0 - r1 * i0;
      }
      if (p_re == 0 && p_im == 0) {
        out_freq[w] = NA_REAL;
      } else {
        double d_re = p_re * demod_re - p_im * demod_im;
        double d_im = p_re * demod_im + p_im * demod_re;
        out_freq[w] = fs * (1 / scale + std::atan2(d_im, d_re) / two_pi);
      }

      // coherence: lag products centered in the window,
      // i.e. b + half_lag in [begin, end), and b + lag < n
      double c_re = 0, c_im = 0, c_abs = 0;
      const long b0 = std::max(0L, begin - half_lag);
      const long b1 = std::min(end - half_lag, (long)n - lag);
      for (long b = b0; b < b1; b++) {
        double r0 = row[2 * b], i0 = row[2 * b + 1];
        double r1 = row[2 * (b + lag)], i1 = row[2 * (b + lag) + 1];
        c_re += r1 * r0 + i1 * i0;
        c_im += i1 * r0 - r1 * i0;
        c_abs += std::sqrt(norm_buf[b] * norm_buf[b + lag]);
      }
      out_coh[w] = (c_abs > 0) ?
        std::min(1.0, std::sqrt(c_re * c_re + c_im * c_im) / c_abs) :
        NA_REAL;
    }
  };

  fcwt.cwt(signal_f.data(), n, &scs, process_row);

  return output;
}
