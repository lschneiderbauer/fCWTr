//
//  fcwt.cpp
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

#include "fcwt.h"

//==============================================================//
//================== Morlet ====================================//
//==============================================================//

Morlet::Morlet(float bandwidth) {
    fb = bandwidth;
}

void Morlet::generate(int size) {
    //Frequency domain, because we only need size. Default scale is always 2;
    width = size;

    float tmp1;
    float toradians = (2*PI)/(float)size;
    float norm = sqrt(2*PI)*IPI4;

    free(mother); // generate() may be called repeatedly
    mother = (float*)malloc(sizeof(float)*width);
    if(!mother) throw std::bad_alloc();

    //calculate array
    for(int w = 0; w < width; w++) {
        tmp1 = (2.0f * ((float)w * toradians) * fb - 2.0f*PI*fb);
        tmp1 = -(tmp1 * tmp1)/2;
        mother[w] = (norm*exp(tmp1));
    }
}

//==============================================================//
//================== Scales =====================================//
//==============================================================//

Scales::~Scales() {
    free(scales);
}

Scales::Scales(SCALETYPE st, double fs, float f0, float f1, int fn) {

    scales = (float*)malloc(fn*sizeof(float));
    if(!scales) throw std::bad_alloc();
    nscales = fn;

    // if the constructor throws, the destructor is not called: free manually
    try {
        if(st==SCALETYPE::FCWT_LOGSCALES)
            calculate_logscale_array(2.0f, fs, f0, f1, fn);
        else
            calculate_linfreq_array(fs, f0, f1, fn);

        // Guard against invalid scales: downstream (in particular the AVX code
        // path of daughter_wavelet_multiplication) uses them to compute array
        // indices, and e.g. a NaN scale leads to out-of-bounds memory access.
        for(int i=0; i<fn; i++) {
            if(!std::isfinite(scales[i]) || scales[i] <= 0) {
                throw std::runtime_error(
                    "Internal error: computed wavelet scales are invalid (not finite or not positive)."
                );
            }
        }
    } catch(...) {
        free(scales);
        scales = NULL;
        throw;
    }
}

void Scales::calculate_logscale_array(float base, double fs, float f0, float f1, int fn) {

    //If a signal has fs=100hz and you want to measure [0.1-50]Hz, you need scales 2 to 1000;
    float nf0 = f0;
    float nf1 = f1;
    float s0 = (fs/nf1);
    float s1 = (fs/nf0);

    //Cannot pass the nyquist frequency
    if (f1 > fs/2.0)
      throw std::runtime_error(
        "Max frequency cannot be higher than the Nyquist frequency: sampling frequency / 2"
      );

    float power0 = log(s0)/log(base);
    float power1 = log(s1)/log(base);
    float dpower = power1-power0;

    for(int i=0; i<fn; i++) {
        float power = (fn > 1) ? power0 + (dpower/(fn-1))*i : power0;
        scales[i] = pow(base,power);
    }
}

void Scales::calculate_linfreq_array(double fs, float f0, float f1, int fn) {

    float nf0 = f0;
    float nf1 = f1;
    //If a signal has fs=100hz and you want to measure [0.1-50]Hz, you need scales 2 to 1000;

    //Cannot pass the nyquist frequency
    if (f1 > fs/2.0)
      throw std::runtime_error(
        "Max frequency cannot be higher than the Nyquist frequency: sampling frequency / 2"
      );

    float df = nf1-nf0;

    // Linearly spaced frequencies from f1 (first scale) down to f0 (last
    // scale), both inclusive, as on the logarithmic scale. (Previously the
    // step was df/fn, so f1 was never reached and the frequencies did not
    // match the ones reported on the R side.) For fn = 1, take f1.
    for(int i=0; i<fn; i++) {
        float f = (fn > 1) ? nf0 + (df/(fn-1))*(float)i : nf1;
        scales[fn-i-1] = (((float)fs)/f);
    }
}

//==============================================================//
//================== FCWT =====================================//
//==============================================================//

// Multiplies the Fourier transformed input with the daughter wavelet of the
// given scale (the mother wavelet, sampled at a scale-dependent step).
// Only the entries up to a scale-dependent endpoint are written.
void FCWT::daughter_wavelet_multiplication(fftwf_complex *input, fftwf_complex *output, float const *mother, float scale, int isize)
{
    float isizef = ((float)(isize));
    float endpointf = fmin(isizef/2.0,((isizef*2.0)/scale));
    float step = (scale/2.0);
    int endpoint = ((int)endpointf);

    #ifdef AVX
        // round up, so that the last (up to 3) entries are processed as well
        // (indices into mother are clamped to isize-1 below)
        int endpoint4 = (endpoint+3)>>2;

        //has avx instructions
        __m256* O8 = (__m256*)output;
        __m256* I8 = (__m256*)input;
        __m256 step4 = _mm256_set1_ps(step);
        __m256 offset = _mm256_set_ps(3,3,2,2,1,1,0,0);
        __m256 maximum = _mm256_set1_ps(isizef-1);

        int athreads = min(threads,max(1,endpoint4/16));
        int batchsize = (endpoint4/athreads);

        #ifndef SINGLE_THREAD
            #pragma omp parallel for num_threads(athreads)
        #endif
        for(int i=0; i<athreads; i++) {
            int start = batchsize*i;
            // last batch takes the remainder of the integer division
            int end = (i == athreads-1) ? endpoint4 : batchsize*(i+1);

            for(int q4=start; q4<end; q4++) {
                float q = (float)q4*4;

                __m256 qq = _mm256_set1_ps(q);

                U256f tmp = {_mm256_min_ps(maximum,_mm256_mul_ps(step4,_mm256_add_ps(qq,offset)))};

                __m256 wav = _mm256_set_ps(
                                        mother[(int)tmp.a[7]],
                                        mother[(int)tmp.a[6]],
                                        mother[(int)tmp.a[5]],
                                        mother[(int)tmp.a[4]],
                                        mother[(int)tmp.a[3]],
                                        mother[(int)tmp.a[2]],
                                        mother[(int)tmp.a[1]],
                                        mother[(int)tmp.a[0]]);

                O8[q4] = _mm256_mul_ps(I8[q4],wav);
            }
        }
    #else
        int athreads = min(threads,max(1,endpoint/16));
        int batchsize = (endpoint/athreads);
        float maximum = isizef-1;

        #ifndef SINGLE_THREAD
            #pragma omp parallel for num_threads(athreads)
        #endif
        for(int i=0; i<athreads; i++) {
            int start = batchsize*i;
            // last batch takes the remainder of the integer division
            int end = (i == athreads-1) ? endpoint : batchsize*(i+1);

            for(int q1=start; q1<end; q1++) {
                float q = (float)q1;
                float tmp = min(maximum,step*q);

                output[q1][0] = input[q1][0]*mother[(int)tmp];
                output[q1][1] = input[q1][1]*mother[(int)tmp];
            }
        }
    #endif
}

// FFTW's inverse transform is unnormalized: divide by the FFT size.
// Only the first `count` values are needed (and normalized).
void FCWT::fft_normalize(complex<float>* out, int count, int fftsize) {
    for(int i=0; i<count; i++) {
        out[i] = out[i] / (float)fftsize;
    }
}

namespace {
    // destroys an FFTW plan when going out of scope (also on exceptions)
    struct PlanGuard {
        fftwf_plan plan;
        explicit PlanGuard(fftwf_plan p): plan(p) {
            if(!plan) throw std::runtime_error("Could not create FFTW plan.");
        }
        ~PlanGuard() { fftwf_destroy_plan(plan); }
        PlanGuard(const PlanGuard&) = delete;
        PlanGuard& operator=(const PlanGuard&) = delete;
    };
}

void FCWT::cwt(const float *pinput, int psize, Scales *scales, const RowCallback &row_callback) {

    if(psize < 1 || psize > FCWT_MAX_SIZE) {
        throw std::runtime_error("Signal length must be between 1 and 2^30.");
    }

    const int size = psize;

    //Find nearest power of 2
    const int nt = find2power(size);
    const int newsize = 1 << nt;

    // Allocate all buffers up front: if an allocation fails (std::bad_alloc),
    // everything allocated so far is freed automatically.
    AlignedBuffer Ihat_buf((size_t)newsize*sizeof(fftwf_complex));
    AlignedBuffer O1_buf((size_t)newsize*sizeof(fftwf_complex));
    AlignedBuffer row_buf((size_t)newsize*sizeof(fftwf_complex));
    fftwf_complex *Ihat = Ihat_buf.complex_ptr();
    fftwf_complex *O1 = O1_buf.complex_ptr();
    fftwf_complex *row = row_buf.complex_ptr();

    // zero-padded copy of the input
    std::vector<float> input((size_t)newsize, 0.0f);
    memcpy(input.data(), pinput, sizeof(float)*(size_t)size);

    //Generate mother wavelet function
    wavelet->generate(newsize);

    #ifndef SINGLE_THREAD
        // Note: we do not call omp_set_num_threads() here, as that would change
        // the OpenMP default for the whole R session. Our own parallel regions
        // set the number of threads explicitly, FFTW gets it here:
        fftwf_init_threads();
        fftwf_plan_with_nthreads(threads);
    #endif

    //Perform forward FFT on input signal
    {
        PlanGuard p(fftwf_plan_dft_r2c_1d(newsize, input.data(), Ihat, FFTW_ESTIMATE));
        fftwf_execute(p.plan);
    }

    // The r2c transform only computes the first half of the spectrum,
    // the second half follows from the symmetry of real input.
    for(int i=1; i<(newsize>>1); i++) {
        Ihat[newsize-i][0] = Ihat[i][0];
        Ihat[newsize-i][1] = -Ihat[i][1];
    }

    // Inverse FFT: O1 -> row, for every scale.
    // The plan is only ever executed on the (aligned) arrays it was created
    // with, so FFTW may use alignment-dependent SIMD code (no FFTW_UNALIGNED).
    PlanGuard pinv(fftwf_plan_dft_1d(newsize, O1, row, FFTW_BACKWARD, FFTW_ESTIMATE));

    for(int i = 0; i < scales->nscales; i++) {
        // O1 is re-used for every scale, but daughter_wavelet_multiplication()
        // only writes the entries up to a scale-dependent endpoint. Clear it,
        // so that no values of the previous scale leak into this one.
        memset(O1, 0, sizeof(fftwf_complex)*(size_t)newsize);

        //FFT-based convolution in the frequency domain
        daughter_wavelet_multiplication(Ihat, O1, wavelet->mother, scales->scales[i], newsize);
        fftwf_execute(pinv.plan);

        // only the first `size` values belong to the (unpadded) signal
        fft_normalize((complex<float>*)row, size, newsize);
        row_callback(i, (const complex<float>*)row);
    }
}
