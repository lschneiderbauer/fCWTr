#include "cpp11.hpp"
#include "fcwt.h"
#include <complex>

double my_abs(const std::complex<float> &arg) {
  return std::abs(arg);
}

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

// Returns the absolute values of the CWT as a (time x frequency) matrix in
// column-major order, averaged over windows of `window_size` samples.
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
    bool scaletype) // TRUE = linear, FALSE = log
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
  cpp11::writable::doubles abs_output((R_xlen_t)((size_t)target_time_n * fn));
  double *out = REAL(static_cast<SEXP>(abs_output));

  //Initialize a Morlet wavelet having sigma=sigma;
  Morlet morl(sigma);

  //Create the continuous wavelet transform object
  FCWT fcwt(&morl, nthreads);

  SCALETYPE st = scaletype ? FCWT_LINFREQS : FCWT_LOGSCALES;
  Scales scs(st, fs, f0, f1, fn);

  // Absolute value (and window averaging) of one row (frequency)
  auto process_row = [&](int f, const std::complex<float> *row) {
    double *out_row = out + (size_t)f * target_time_n;

    if (window_size > 1) {
      for (int w = 0; w < target_time_n; w++) {
        double mean = 0;
        int count = 0;
        for (size_t i = (size_t)w * window_size; i < (size_t)(w+1) * window_size && i < (size_t)n; i++) {
          count += 1;
          // running mean (note: floating point division, count is an int)
          mean = (double)(count - 1) / count * mean + my_abs(row[i]) / count;
        }
        out_row[w] = mean;
      }
    } else {
      for (int i = 0; i < n; i++) {
        out_row[i] = my_abs(row[i]);
      }
    }
  };

  fcwt.cwt(signal_f.data(), n, &scs, process_row);

  return abs_output;
}
