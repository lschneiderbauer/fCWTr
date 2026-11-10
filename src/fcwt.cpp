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

//
//  fcwt.cpp
//  fCWT-testing
//
//  Created by Lukas Arts on 21/12/2020.
//  Copyright © 2020 Lukas Arts.
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
 Minor modifications made by Lukas Schneiderbauer
*/

#include "fcwt.h"

Morlet::Morlet(float bandwidth) {
    four_wavelen = 0.9876f;
    fb = bandwidth;
    fb2 = 2.0f*fb*fb;
    ifb = 1.0f/fb;
    imag_frequency = false;
    doublesided = false;
    mother = NULL;
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

/*
void Morlet::generate(float* real, float* imag, int size, float scale) {
    //Time domain because we know size from scale
    float tmp1, tmp2;
    width = getSupport(scale);
    float norm = (float)size * ifb * IPI4;

    //cout << scale << " [";
    for(int t=0; t < width*2+1; t++) {
        tmp1 = (float)(t - width)/scale;
        tmp2 = exp(-(tmp1*tmp1)/(fb2));

        real[t] = norm*tmp2*cos(tmp1*2.0f*PI)/scale;
        imag[t] = norm*tmp2*sin(tmp1*2.0f*PI)/scale;
        //cout << real[t]*real[t]+imag[t]*imag[t] << ",";
    }
    //cout << "]" << endl;
}

void Morlet::getWavelet(float scale, complex<float>* pwav, int pn) {
    int w = getSupport(scale);

    float *real = (float*)malloc(sizeof(float)*max(w*2+1,pn));
    float *imag = (float*)malloc(sizeof(float)*max(w*2+1,pn));
    for(int t=0; t < max(w*2+1,pn); t++) {
        real[t] = 0;
        imag[t] = 0;
    }

    generate(real,imag,pn,scale);

    for(int t=0; t < pn; t++) {
        pwav[t].real(real[t]);
        pwav[t].imag(imag[t]);
    }

    free(real);
    free(imag);
};
*/

//==============================================================//
//================== Scales =====================================//
//==============================================================//

Scales::~Scales() {
    free(scales);
}

// Only ratios of frequencies and fs enter the computation, so all of them just
// need to be given in the same unit (fs need not be an integer).
Scales::Scales(Wavelet *wav, SCALETYPE st, double afs, float af0, float af1, int afn) {

    fs = afs;
    scales = (float*)malloc(afn*sizeof(float));
    if(!scales) throw std::bad_alloc();
    fourwavl = wav->four_wavelen;
    nscales = afn;

    // if the constructor throws, the destructor is not called: free manually
    try {
        if(st==SCALETYPE::FCWT_LOGSCALES)
            calculate_logscale_array(2.0f, wav->four_wavelen, afs, af0, af1, afn);
        // else if(st==SCALETYPE::FCWT_LINSCALES)
        //    calculate_linscale_array(wav->four_wavelen, afs, af0, af1, afn);
        else
            calculate_linfreq_array(wav->four_wavelen, afs, af0, af1, afn);

        // Guard against invalid scales: downstream (in particular the AVX code
        // path of daughter_wavelet_multiplication) uses them to compute array
        // indices, and e.g. a NaN scale leads to out-of-bounds memory access.
        for(int i=0; i<afn; i++) {
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

/*
void Scales::getScales(float *pfreqs, int pnf) {
    for(int i=0;i<pnf;i++) {
        pfreqs[i]=scales[i];
    };
};

void Scales::getFrequencies(float *pfreqs, int pnf) {
    for(int i=0;i<pnf;i++) {
        pfreqs[i]=((float)fs)/scales[i];
    };
};
*/

void Scales::calculate_logscale_array(float base, float four_wavl, double fs, float f0, float f1, int fn) {

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

void Scales::calculate_linfreq_array(float four_wavl, double fs, float f0, float f1, int fn) {

    float nf0 = f0;
    float nf1 = f1;
    //If a signal has fs=100hz and you want to measure [0.1-50]Hz, you need scales 2 to 1000;

    //Cannot pass the nyquist frequency
    if (f1 > fs/2.0)
      throw std::runtime_error(
        "Max frequency cannot be higher than the Nyquist frequency: sampling frequency / 2"
      );

    float df = nf1-nf0;

    for(int i=0; i<fn; i++) {
        scales[fn-i-1] = (((float)fs)/(nf0 + (df/fn)*(float)i));
    }
}

/*
void Scales::calculate_linscale_array(float four_wavl, int fs, float f0, float f1, int fn) {

    float nf0 = f0;
    float nf1 = f1;
    //If a signal has fs=100hz and you want to measure [0.1-50]Hz, you need scales 2 to 1000;
    float s0 = fs/nf1;
    float s1 = fs/nf0;

    //Cannot pass the nyquist frequency
    if (f1 > fs/2.0)
      throw std::runtime_error(
        "Max frequency cannot be higher than the Nyquist frequency: sampling frequency / 2"
      );

    float ds = s1-s0;

    for(int i=0; i<fn; i++) {
        scales[i] = (s0 + (ds/fn)*i);
    }
}
*/

//==============================================================//
//================== FCWT =====================================//
//==============================================================//

void FCWT::daughter_wavelet_multiplication(fftwf_complex *input, fftwf_complex *output, float const *mother, float scale, int isize, bool imaginary, bool doublesided)
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
        int s4 = (isize>>2)-1;

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
                //U256f tmp = {_mm256_mul_ps(step4,_mm256_add_ps(qq,offset))};

                __m256 wav = _mm256_set_ps(
                                        mother[(int)tmp.a[7]]*(1-2*imaginary),
                                        mother[(int)tmp.a[6]],
                                        mother[(int)tmp.a[5]]*(1-2*imaginary),
                                        mother[(int)tmp.a[4]],
                                        mother[(int)tmp.a[3]]*(1-2*imaginary),
                                        mother[(int)tmp.a[2]],
                                        mother[(int)tmp.a[1]]*(1-2*imaginary),
                                        mother[(int)tmp.a[0]]);

                if(imaginary) {
                    __m256 tmp2 = _mm256_mul_ps(I8[q4],wav);
                    O8[q4] = _mm256_shuffle_ps(tmp2, tmp2, 177);
                } else {
                    O8[q4] = _mm256_mul_ps(I8[q4],wav);
                }
            }

            if(doublesided) {
                for(int q4=start; q4<end; q4++) {
                    float q = (float)(q4*4);

                    __m256 qq = _mm256_set1_ps(q);
                    U256f tmp = {_mm256_mul_ps(step4,_mm256_add_ps(qq,offset))};

                    __m256 wav = _mm256_set_ps(
                                            mother[(int)tmp.a[0]]*(1-2*imaginary),
                                            mother[(int)tmp.a[1]],
                                            mother[(int)tmp.a[2]]*(1-2*imaginary),
                                            mother[(int)tmp.a[3]],
                                            mother[(int)tmp.a[4]]*(1-2*imaginary),
                                            mother[(int)tmp.a[5]],
                                            mother[(int)tmp.a[6]]*(1-2*imaginary),
                                            mother[(int)tmp.a[7]]);

                    if(imaginary) {
                        __m256 tmp2 = _mm256_mul_ps(I8[s4-q4],wav);
                        O8[s4-q4] = _mm256_shuffle_ps(tmp2, tmp2, 177);
                    } else {
                        O8[s4-q4] = _mm256_mul_ps(I8[s4-q4],wav);
                    }
                }
            }
        }
    #else
        int athreads = min(threads,max(1,endpoint/16));
        int batchsize = (endpoint/athreads);
        float maximum = isizef-1;
        int s1 = isize-1;

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
                output[q1][1] = input[q1][1]*mother[(int)tmp]*(1-2*imaginary);
            }

            if(doublesided) {
                for(int q1=start; q1<end; q1++) {
                    float q = (float)q1;
                    float tmp = min(maximum,step*q);

                    output[s1-q1][0] = input[s1-q1][0]*mother[(int)tmp]*(1-2*imaginary);
                    output[s1-q1][1] = input[s1-q1][1]*mother[(int)tmp];
                }
            }
        }

    #endif

    return;
}

/*
void FCWT::create_FFT_optimization_plan(int maxsize, int flags) {

    int nt = find2power(maxsize);

    // if(nt <= 10) {
    //     std::cerr << "Maxsize is too small (<=1024)... please use a larger number" << std::endl;
    // }


    for(int i=11; i<=nt; i++) {
        int n = 1 << i;

        float *dat = (float*)malloc(sizeof(float)*n);
        fftwf_complex *O1 = fftwf_alloc_complex(n);
        fftwf_complex *out = fftwf_alloc_complex(n);

        #ifndef SINGLE_THREAD
            omp_set_num_threads(threads);
            // std::cout << "Threads:" << omp_get_max_threads() << std::endl;

            fftwf_init_threads();
            fftwf_plan_with_nthreads(omp_get_max_threads());
        #endif

        fftwf_plan p_for;
        fftwf_plan p_back;

        char file_for[50];
        // sprintf(file_for, "n%d_t%d.wis", n, threads);

        // std::cout << "Calculating optimal scheme for forward FFT with N:" << n << std::endl;
        p_for = fftwf_plan_dft_r2c_1d(n, dat, O1, flags);

        // std::cout << "Calculating optimal scheme for backward FFT with N:" << n << std::endl;
        p_back = fftwf_plan_dft_1d(n, O1, out, FFTW_BACKWARD, flags);

        fftwf_export_wisdom_to_filename(file_for);

        free(dat);
        fftwf_free(O1);
        fftwf_free(out);

        // std::cout << "Optimization schemes for N: " << n << " have been calculated. Next time you use fCWT it will automatically choose the right optimization scheme based on number of threads and signal length." << std::endl;
    }
}
void FCWT::create_FFT_optimization_plan(int maxsize, string flags) {
    int flag = 0;

    if(flags == "FFTW_MEASURE") {
        flag = FFTW_MEASURE;
    } else if(flags == "FFTW_PATIENT") {
        flag = FFTW_PATIENT;
    } else if(flags == "FFTW_EXHAUSTIVE") {
        flag = FFTW_EXHAUSTIVE;
    } else if(flags == "FFTW_ESTIMATE") {
        flag = FFTW_ESTIMATE;
    } else {
        // std::cerr << "Unknown flag: " << flags << std::endl;
        return;
    }

    create_FFT_optimization_plan(maxsize, flag);
}

void FCWT::load_FFT_optimization_plan() {
    const int nt = find2power(size);
    const int newsize = 1 << nt;

    if(use_optimalization_schemes) {
        if(newsize <= 1024) {
            // std::cout << "Inputsize is too small (N <= 1024) to use optimization." << std::endl;
            return;
        }

        // char file_for[50];
        // sprintf(file_for, "n%d_t%d.wis", newsize, threads);

        // if(!fftwf_import_wisdom_from_filename(file_for)) {
        //     std::cout << "WARNING: Optimization scheme '" << file_for << "' was not found, fallback to calculation without optimization." << std::endl;
        // }

    }
}
*/

//Convolve in time domain using a single wavelet
void FCWT::convolve(fftwf_plan p, fftwf_complex *Ihat, fftwf_complex *O1, fftwf_complex *lastscalemem, complex<float> *out, Wavelet *wav, int size, int newsize, float scale, bool lastscale) {

    if(lastscale) {
        // The inverse FFT writes newsize >= size values. For all but the last
        // scale they spill into the next row of out (which is overwritten
        // afterwards), for the last scale we need a separate buffer.
        fftbased(p, Ihat, O1, (float*)lastscalemem, wav->mother, newsize, scale, wav->imag_frequency, wav->doublesided);
        if(use_normalization) fft_normalize((complex<float>*)lastscalemem, newsize);
        memcpy(out, (complex<float>*)lastscalemem, sizeof(complex<float>)*size);
    } else {
        // if(!out) {
        //     std::cout << "OUT NOT A POINTER" << std::endl;
        // }
        fftbased(p, Ihat, O1, (float*)out, wav->mother, newsize, scale, wav->imag_frequency, wav->doublesided);
        if(use_normalization) fft_normalize(out, newsize);
    }
}

void FCWT::fftbased(fftwf_plan p, fftwf_complex *Ihat, fftwf_complex *O1, float *out, float* mother, int size, float scale, bool imaginary, bool doublesided) {

    // O1 is re-used for every scale, but daughter_wavelet_multiplication() only
    // writes the entries up to a scale-dependent endpoint. Clear it, so that
    // no values of the previous scale leak into this one.
    memset(O1, 0, sizeof(fftwf_complex)*size);

    //Perform daughter wavelet generation and multiplication with the Fourier transformed input signal
    daughter_wavelet_multiplication(Ihat,O1,mother,scale,size,imaginary,doublesided);

    // The plan p is created with FFTW_UNALIGNED, so we can write to out
    // directly. (Previously, out was shifted via std::align, which moved
    // the result by one sample for odd signal lengths.)
    fftwf_execute_dft(p,O1,(fftwf_complex*)out);
}

void FCWT::fft_normalize(complex<float>* out, int size) {

    for(int i=0; i<size; i++) {
        out[i] = out[i] / (float)size;
    }
}

void FCWT::cwt(float *pinput, int psize, complex<float>* poutput, Scales *scales, bool complexinput) {

    if(psize < 1 || psize > FCWT_MAX_SIZE) {
        throw std::runtime_error("Signal length must be between 1 and 2^30.");
    }

    size = psize;

    //Find nearest power of 2
    const int nt = find2power(size);
    const int newsize = 1 << nt;

    // Allocate all buffers up front: if an allocation fails (std::bad_alloc),
    // everything allocated so far is freed automatically, and no FFTW plan
    // has been created yet.
    AlignedBuffer Ihat_buf((size_t)newsize*sizeof(fftwf_complex));
    AlignedBuffer O1_buf((size_t)newsize*sizeof(fftwf_complex));
    AlignedBuffer lastscale_buf((size_t)newsize*sizeof(fftwf_complex));
    fftwf_complex *Ihat = Ihat_buf.complex_ptr();
    fftwf_complex *O1 = O1_buf.complex_ptr();

    // zero-padded copy of the input (complex input: two floats per value)
    std::vector<float> input((size_t)newsize * (complexinput ? 2 : 1), 0.0f);
    memcpy(input.data(), pinput, sizeof(float)*(size_t)size*(complexinput ? 2 : 1));

    //Generate mother wavelet function
    wavelet->generate(newsize);

    #ifndef SINGLE_THREAD
        // Note: we do not call omp_set_num_threads() here, as that would change
        // the OpenMP default for the whole R session. Our own parallel regions
        // set the number of threads explicitly, FFTW gets it here:
        fftwf_init_threads();
        fftwf_plan_with_nthreads(threads);
    #endif

    fftwf_plan pinv;
    fftwf_plan p;

    // //Perform forward FFT on input signal
    if(complexinput) {
        p = fftwf_plan_dft_1d(newsize, (fftwf_complex*)input.data(), Ihat, FFTW_FORWARD, FFTW_ESTIMATE);
    } else {
        p = fftwf_plan_dft_r2c_1d(newsize, input.data(), Ihat, FFTW_ESTIMATE);
    }

    fftwf_execute(p);
    fftwf_destroy_plan(p);

    // pinv is re-used via fftwf_execute_dft() on output pointers of varying
    // alignment (offsets into poutput, temporary buffers). FFTW requires the
    // new arrays to have the same alignment as the planning arrays unless the
    // plan is created with FFTW_UNALIGNED (otherwise: undefined behavior,
    // e.g. crashes with AVX/AVX-512 codelets).
    pinv = fftwf_plan_dft_1d(newsize, O1, (fftwf_complex*)poutput, FFTW_BACKWARD, FFTW_ESTIMATE | FFTW_UNALIGNED);

    for(int i=1; i<(newsize>>1); i++) {
        Ihat[newsize-i][0] = Ihat[i][0];
        Ihat[newsize-i][1] = -Ihat[i][1];
    }

    complex<float> *out = poutput;

    for(int i = 0; i < scales->nscales; i++) {
        //FFT-base convolution in the frequency domain
        convolve(pinv, Ihat, O1, lastscale_buf.complex_ptr(), out, wavelet, size, newsize, scales->scales[i], i==(scales->nscales-1));
        out = out + size;
    }

    // //Cleanup (buffers are freed automatically)
    fftwf_destroy_plan(pinv);
}


void FCWT::cwt(float *pinput, int psize, complex<float>* poutput, Scales *scales) {
    cwt(pinput,psize,poutput,scales,false);
}

void FCWT::cwt(complex<float> *pinput, int psize, complex<float>* poutput, Scales *scales) {
    cwt((float*)pinput,psize,poutput,scales,true);
}

void FCWT::cwt(float *pinput, int psize, Scales *scales, complex<float>* poutput, int pn1, int pn2) {
    assert((psize*scales->nscales) == (pn1*pn2));
    cwt(pinput,psize,poutput,scales);
}

void FCWT::cwt(complex<float> *pinput, int psize, Scales *scales, complex<float>* poutput, int pn1, int pn2) {
    assert((psize*scales->nscales) == (pn1*pn2));
    cwt(pinput,psize,poutput,scales);
}

