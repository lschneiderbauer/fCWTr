#include "cpp11.hpp"
#include "fcwt.h"
#include <algorithm>
#include <complex>

double my_abs(const std::complex<float> &arg) {
  return std::abs(arg);
}

double my_real(const std::complex<float> &arg) {
  return std::real(arg);
}

double my_imag(const std::complex<float> &arg) {
  return std::imag(arg);
}

float dbl_to_float(const double &dbl) {
  return static_cast<float>(dbl);
}

[[cpp11::register]]
std::vector<double> fcwt_raw(
    std::vector<double> signal,
    int fs, // sample frequency
    double f0, // beginning of frequency range
    double f1, // end of frequency range
    int fn, // number of wavelets to generate across frequency range
    double sigma,
    int nthreads,
    bool optplans,
    bool abs)
{
  int n = signal.size(); //signal length

  // cast double input to float
  std::vector<float> signal_f(n);
  std::transform(signal.cbegin(), signal.cend(), signal_f.begin(), dbl_to_float);

  //output: n x scales complex numbers
  // std::vector<float> output(n*noctaves*nsuboctaves*2);
  std::vector<std::complex<float>> output(n*fn);

  //Create a wavelet object
  Wavelet *wavelet;

  //Initialize a Morlet wavelet having sigma=sigma;
  Morlet morl(sigma);
  wavelet = &morl;

  //Create the continuous wavelet transform object
  //constructor(wavelet, nthreads, optplan)
  //
  //Arguments
  //wavelet   - pointer to wavelet object
  //nthreads  - number of threads to use
  //optplan   - use FFTW optimization plans if true
  //normalization - take extra time to normalize time-frequency matrix
  FCWT fcwt(wavelet, nthreads, optplans, true);

  //Generate frequencies
  //constructor(wavelet, dist, fs, f0, f1, fn)
  //
  //Arguments
  //wavelet   - pointer to wavelet object
  //dist      - FCWT_LOGSCALES | FCWT_LINFREQS for logarithmic or linear distribution frequency range
  //fs        - sample frequency
  //f0        - beginning of frequency range
  //f1        - end of frequency range
  //fn        - number of wavelets to generate across frequency range
  Scales scs(wavelet, FCWT_LINFREQS, fs, f0, f1, fn);

  //Perform a CWT
  //cwt(input, length, output, scales)
  //
  //Arguments:
  //input     - floating pointer to input array
  //length    - integer signal length
  //output    - floating pointer to output array
  //scales    - pointer to scales object
  fcwt.cwt(&signal_f[0], n, &output[0], &scs);

  std::vector<double> abs_output;

  if (abs) {
    // convert to absolute value
    abs_output = std::vector<double> (n*fn);
    std::transform(output.cbegin(), output.cend(), abs_output.begin(), my_abs);
    //static_cast<float (*)(const &std::complex<float>)>(&std::abs));
  } else {
    // transform to an array of real and imaginary part
    abs_output = std::vector<double> (2 * n*fn);

    auto it = std::transform(output.cbegin(), output.cend(), abs_output.begin(), my_real);
    std::transform(output.cbegin(), output.cend(), it, my_imag);
  }


  return(abs_output);
}



//
//
//
// //' Wraps CPP cwt function (computing a continuous wavelet transform)
// //'
// //' @param input        A numeric vector representing the time series
// //' @param startoctave  Starting octave (corresponds to a scale of 2^startoctave)
// //' @param noctaves     Number of octaves to compute
// //' @param nsuboctaves  Computing steps per octave
// //' @param sigma        Parameter controlling time-frequency precision
// //' @param nthreads     Number of threads to use (FFTW)
// //' @param optplans     Use FFTW optimization plans
// //' @param abs          Returns the absolute values instead of full
// //'                     complex representations.
// //' @return Returns a numeric vector containing CWT information.
// //'
// std::vector<float> fcwt_raw(
//     std::vector<float> input, // implicitly converts from NumericVector
//     int startoctave,
//     int noctaves,
//     int nsuboctaves,
//     float sigma,
//     int nthreads,
//     bool optplans,
//     bool abs)
// {
//   int n = input.size(); //signal length
//
//   //output: n x scales complex numbers
//   std::vector<float> output(n*noctaves*nsuboctaves*2);
//
//   //Arguments:
//   //input     - floating pointer to input array
//   //length    - integer signal length
//   //output    - floating pointer to output array
//   //startoct  - scale range begin (2^startoct)
//   //endoct    - scale range end (2^endoct)
//   //suboct    - exponential subdivisions of each octave
//   //sigma     - parameter to control time-frequency precision
//   //nthreads  - number of threads to use
//   //optplans  - use FFTW optimization plans if true
//
//   fcwt::cwt(input.data(), n, output.data(), startoctave, startoctave + noctaves - 1,
//             nsuboctaves, sigma, nthreads, optplans);
//
//   // divide the output by input size to get a
//   // normalized version (fftw does not normalize)
//   // let's do that before we calculate the absolute values
//
//   // since fcwt::cwt only fourier-transforms the nearest smaller power of 2 size,
//   // we need to get that value in order to normalize properly.
//   int n_norm = (1 << fcwt::find2power(n)) / 2;
//   transform(output.begin(), output.end(), output.begin(),
//     [n_norm](float &c){
//       return c/n_norm;
//     }
//   );
//
//   if(abs) {
//     std::vector<float> abs_output(n*noctaves*nsuboctaves);
//
//     comp_abs(output.data(), abs_output.data(), n*noctaves*nsuboctaves);
//     return(abs_output);
//   } else {
//     return(output);
//   }
// }
//
//
//
//
// // Converting FFTW flags
// //
// // Converts a string representation of a flag
// // to the corresponding integer value understood
// // by the underlying C API.
// //
// // @inheritParams create_optimization_schemes_raw
// // @return Corresponding integer value.
// int fftw_flag(String flag) {
//
//   if (flag == "estimate") {
//     return(FFTW_ESTIMATE);
//   } else if(flag == "measure") {
//     return(FFTW_MEASURE);
//   } else if(flag == "patient") {
//     return(FFTW_PATIENT);
//   } else if(flag == "exhaustive") {
//     return(FFTW_EXHAUSTIVE);
//   } else if(flag == "wisdom_only") {
//     return(FFTW_WISDOM_ONLY);
//   } else {
//     stop("Incorrect FFTW flag provided.");
//   }
//
// }
//
// //' Wraps CPP 'create_optimization_schemes' function
// //'
// //' FFTW allows one to create optimization plans in advance,
// //' which can help to speed up individual calls of [fcwt_raw].
// //'
// //' @param max_size  Typical length of input vector
// //' @param nthreads  Number of threads used in the computation
// //' @param flag
// //' From https://www.fftw.org/fftw3_doc/Planner-Flags.html:
// //' "estimate"" specifies that, instead of actual measurements of different algorithms, a simple heuristic is used to pick a (probably sub-optimal) plan quickly. With this flag, the input/output arrays are not overwritten during planning.
// //' "measure"" tells FFTW to find an optimized plan by actually computing several FFTs and measuring their execution time. Depending on your machine, this can take some time (often a few seconds). FFTW_MEASURE is the default planning option.
// //' "patient"" is like FFTW_MEASURE, but considers a wider range of algorithms and often produces a “more optimal” plan (especially for large transforms), but at the expense of several times longer planning time (especially for large transforms).
// //' "exhaustive" is like FFTW_PATIENT, but considers an even wider range of algorithms, including many that we think are unlikely to be fast, to produce the most optimal plan but with a substantially increased planning time.
// //' "wisdom_only" is a special planning mode in which the plan is only created if wisdom is available for the given problem, and otherwise a NULL plan is returned. This can be combined with other flags, e.g. ‘FFTW_WISDOM_ONLY | FFTW_PATIENT’ creates a plan only if wisdom is available that was created in FFTW_PATIENT or FFTW_EXHAUSTIVE mode. The FFTW_WISDOM_ONLY flag is intended for users who need to detect whether wisdom is available; for example, if wisdom is not available one may wish to allocate new arrays for planning so that user data is not overwritten.
// //'
// void create_opt_schemes_raw(int max_size, int nthreads, String flag) {
//   fcwt::create_optimization_schemes(max_size, nthreads, fftw_flag(flag));
//
// }
//
