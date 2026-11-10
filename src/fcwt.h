//
//  fcwt.h
//  fCWT
//
//  Created by Lukas Arts on 21/12/2020.
//  Copyright © 2021 Lukas Arts.
/*Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
/*
 Modified by Lukas Schneiderbauer for the R package fCWTr: reduced to the
 parts used by the package (real input, Morlet wavelet, FFT-based transform),
 output is handed over row by row, bug fixes. See the git history of fCWTr
 for details.
*/


#ifndef FCWT_H
#define FCWT_H

#include <stdlib.h>
#include <cstring>
#include <math.h>
#include <cmath>
#include <algorithm>
#include <complex>
#include <functional>
#include <new>
#include <stdexcept>
#include <vector>

#ifndef _OPENMP
    // if we have no platform support, enable single_threading
    #define SINGLE_THREAD
#endif
#ifdef _WIN32
    #include <malloc.h> // _aligned_malloc

    // disable on windows since rtools does compile fftw without openmp support
    #define SINGLE_THREAD
#endif
#ifndef SINGLE_THREAD
    #include <omp.h>
#endif

#include "fftw3.h"

//check if avx is supported and include the header
#if defined(__AVX__)
    #include <immintrin.h>
    #define AVX
    union U256f {
        __m256 v;
        float a[8];
    };
#endif

#define PI                    3.14159265358979323846264338327950288419716939937510582097494459072381640628620899862803482534211706798f
#define IPI4                  0.75112554446f

using namespace std;

enum SCALETYPE {FCWT_LOGSCALES,FCWT_LINFREQS};

// Mother wavelet, given in the frequency domain.
class Wavelet {
public:
    Wavelet(): width(0), mother(NULL) {};
    virtual ~Wavelet() {};

    // (Re-)generate the mother wavelet for an FFT of the given size.
    virtual void generate(int size) = 0;

    int width;
    float *mother;
};

class Morlet : public Wavelet {
public:
    Morlet(float bandwidth);
    ~Morlet() { free(mother); };

    void generate(int size);

    float fb;
};

class Scales {
public:
    // Only ratios of the frequencies and fs enter the computation, so they
    // just need to be given in the same unit.
    Scales(SCALETYPE st, double fs, float f0, float f1, int fn);
    ~Scales();

    float* scales;
    int nscales;

private:
    void calculate_logscale_array(float base, double fs, float f0, float f1, int fn);
    void calculate_linfreq_array(double fs, float f0, float f1, int fn);
};

class FCWT {
public:
    // Receives the result for one scale: the scale's index and `psize`
    // (normalized) complex values. Called for each scale, in order.
    typedef std::function<void(int, const complex<float>*)> RowCallback;

    FCWT(Wavelet *pwav, int pthreads):
        wavelet(pwav),
        threads(pthreads) {};

    // Continuous wavelet transform of the real signal pinput (length psize).
    // Instead of writing all results into one large output matrix, every
    // row (scale) is handed to row_callback right after it is computed, so
    // the caller can process it (e.g. take the absolute value, average) and
    // the full complex result never needs to be kept in memory.
    void cwt(const float *pinput, int psize, Scales *scales, const RowCallback &row_callback);

private:
    void daughter_wavelet_multiplication(fftwf_complex *input, fftwf_complex *output, float const *mother, float scale, int isize);
    void fft_normalize(complex<float>* out, int count, int fftsize);

    Wavelet *wavelet;
    int threads;
};

// aligned_alloc() requires the size to be a multiple of the alignment (32)
inline size_t aligned_size(size_t bytes)
{
    return ((bytes + 31) / 32) * 32;
}

// 32-byte aligned memory (needed by the AVX code path), owned RAII-style:
// it is freed automatically, also if an exception is thrown in between.
// Throws std::bad_alloc if the allocation fails.
class AlignedBuffer {
public:
    explicit AlignedBuffer(size_t bytes) {
        #ifdef _WIN32
            ptr = _aligned_malloc(aligned_size(bytes), 32);
        #else
            ptr = aligned_alloc(32, aligned_size(bytes));
        #endif
        if(!ptr) throw std::bad_alloc();
        memset(ptr, 0, bytes);
    }
    ~AlignedBuffer() {
        #ifdef _WIN32
            _aligned_free(ptr);
        #else
            free(ptr);
        #endif
    }
    AlignedBuffer(const AlignedBuffer&) = delete;
    AlignedBuffer& operator=(const AlignedBuffer&) = delete;

    float* float_ptr() { return (float*)ptr; }
    fftwf_complex* complex_ptr() { return (fftwf_complex*)ptr; }

private:
    void* ptr;
};

// Largest supported signal length: the FFT size (next power of 2) must fit
// into an int, which is what FFTW's planner interface expects.
const int FCWT_MAX_SIZE = 1 << 30;

inline int find2power(int n)
{
    int m, m2;

    m = 0;
    m2 = 1<<m; /* 2 to the power of m */
    while (m2-n < 0) {
        m++;
        m2 <<= 1; /* m2 = m2*2 */
    }
    return(m);
}

#endif
