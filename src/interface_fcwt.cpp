#include <Rcpp.h>
#include "fcwt.h"

using namespace Rcpp;


//' Wraps CPP cwt function (computing a continuous wavelet transform)
//'
//' @param input        A numeric vector representing the time series
//' @param startoctave  Starting octave (corresponds to a scale of 2^startoctave)
//' @param noctaves     Number of octaves to compute
//' @param nsuboctaves  Computing steps per octave
//' @param sigma        Parameter controlling time-frequency precision
//' @param optplans     Use FFTW optimization plans
//' @return Returns a numeric vector containing CWT information.
//'
// [[Rcpp::export]]
std::vector<float> fcwt_raw(
    std::vector<float> input, // implicitly converts from NumericVector
    int startoctave,
    int noctaves,
    int nsuboctaves,
    float sigma,
    bool optplans)
{
  int n = input.size(); //signal length

  //output: n x scales complex numbers
  std::vector<float> output(n*noctaves*nsuboctaves*2);

  //Arguments:
  //input     - floating pointer to input array
  //length    - integer signal length
  //output    - floating pointer to output array
  //startoct  - scale range begin (2^startoct)
  //endoct    - scale range end (2^endoct)
  //suboct    - exponential subdivisions of each octave
  //sigma     - parameter to control time-frequency precision
  //nthreads  - number of threads to use
  //optplans  - use FFTW optimization plans if true

  fcwt::cwt(input.data(), n, output.data(), startoctave, startoctave + noctaves - 1,
            nsuboctaves, sigma, 8, optplans);

  return(output);  // wrap is called implicitly
}



// Converting FFTW flags
//
// Converts a string representation of a flag
// to the corresponding integer value understood
// by the underlying C API.
//
// @inheritParams create_optimization_schemes_raw
// @return Corresponding integer value.
int fftw_flag(String flag) {

  if (flag == "estimate") {
    return(FFTW_ESTIMATE);
  } else if(flag == "measure") {
    return(FFTW_MEASURE);
  } else if(flag == "patient") {
    return(FFTW_PATIENT);
  } else if(flag == "exhaustive") {
    return(FFTW_EXHAUSTIVE);
  } else if(flag == "wisdom_only") {
    return(FFTW_WISDOM_ONLY);
  } else {
    stop("Incorrect FFTW flag provided.");
  }

}

//' Wraps CPP 'create_optimization_schemes' function
//'
//' FFTW allows one to create optimization plans in advance,
//' which can help to speed up individual calls of [fcwt_raw].
//'
//' @param maxsize  Typical length of input vector
//' @param threads  Number of threads used in the computation
//' @param flag
//' From https://www.fftw.org/fftw3_doc/Planner-Flags.html:
//' "estimate"" specifies that, instead of actual measurements of different algorithms, a simple heuristic is used to pick a (probably sub-optimal) plan quickly. With this flag, the input/output arrays are not overwritten during planning.
//' "measure"" tells FFTW to find an optimized plan by actually computing several FFTs and measuring their execution time. Depending on your machine, this can take some time (often a few seconds). FFTW_MEASURE is the default planning option.
//' "patient"" is like FFTW_MEASURE, but considers a wider range of algorithms and often produces a “more optimal” plan (especially for large transforms), but at the expense of several times longer planning time (especially for large transforms).
//' "exhaustive" is like FFTW_PATIENT, but considers an even wider range of algorithms, including many that we think are unlikely to be fast, to produce the most optimal plan but with a substantially increased planning time.
//' "wisdom_only" is a special planning mode in which the plan is only created if wisdom is available for the given problem, and otherwise a NULL plan is returned. This can be combined with other flags, e.g. ‘FFTW_WISDOM_ONLY | FFTW_PATIENT’ creates a plan only if wisdom is available that was created in FFTW_PATIENT or FFTW_EXHAUSTIVE mode. The FFTW_WISDOM_ONLY flag is intended for users who need to detect whether wisdom is available; for example, if wisdom is not available one may wish to allocate new arrays for planning so that user data is not overwritten.
//'
// [[Rcpp::export]]
void create_optimization_schemes_raw(int maxsize, int threads, String flag) {
  fcwt::create_optimization_schemes(maxsize, threads, fftw_flag(flag));
}

