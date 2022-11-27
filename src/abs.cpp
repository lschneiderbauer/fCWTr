#include <Rcpp.h>
#include "fcwt.h"

void comp_abs(float *cplxIn, float *absOut, const int length)
{
  #pragma omp parallel for
  for (int i = 0; i < length; i += 1)
  {
    absOut[i] = sqrt(cplxIn[2*i]*cplxIn[2*i] + cplxIn[2*i+1]*cplxIn[2*i+1]);
  }
}
