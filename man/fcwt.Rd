% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcwt.R
\name{fcwt}
\alias{fcwt}
\title{Fast continuous wavelet transform}
\usage{
fcwt(
  signal,
  sample_freq,
  n_freqs,
  freq_begin = 2 * sample_freq/length(signal),
  freq_end = sample_freq/2,
  freq_scale = c("linear", "log"),
  sigma = 1,
  remove_coi = TRUE,
  n_threads = 2L
)
}
\arguments{
\item{signal}{Real-valued time series. The time steps are assumed to be evenly spaced.}

\item{sample_freq}{Sampling rate of input time series. This number primarily establishes
a connection to physical units which is used in other frequency definitions
as well as the units of the output data. Expects either a value with frequency
units, generated with \code{u()}, or a pure number, in which case it is
interpreted in units of 'Hertz'.}

\item{n_freqs}{Number of frequency bins generated by the CWT. The frequencies
are linearly distributed. Computation time increases when raising the
number of frequency bins.}

\item{freq_begin, freq_end}{Optionally specifies the frequency range \verb{[freq_end, freq_begin]}. If not
specified the maximal meaningful frequency range, depending on the input signal,
is taken.
A frequency-valued number, generated with \code{u()}, or a pure number, that is
interpreted in units of 'Hertz'.}

\item{freq_scale}{Should the frequency scale be linear or logarithmic?
"linear"  / "log" for linear / logarithmic.}

\item{sigma}{Sets a dimensionless parameter modifying the wavelet spread which in the
time-domain is roughly given by \eqn{\Sigma_t \sim \sqrt{2} \frac{\sigma}{f}}.
Changing this parameter adjusts the time/frequency uncertainty balance.
Defaults to 1.
Larger (lower) value of sigma corresponds to a better (worse) frequency
resolution and a worse (better) time resolution.}

\item{remove_coi}{Boundary effects can result in nonphysical artifacts. If \code{remove_coi = TRUE},
those are effectively removed by setting corresponding values to \code{NA}.
We define the essential support of the
(Gaussian) wavelet to be four times its standard deviation,
\eqn{4 \Sigma_t = 2 * \sqrt{2} \frac{\sigma}{f}}, and so a wavelet touches
the boundary if the distance of the center of the wavelet to the boundary
is less then \eqn{4 \Sigma_t}. Values that fall into that range are removed
if \code{remove_coi = TRUE}.}

\item{n_threads}{Number of threads used by the computation, if supported by your platform.
Defaults to 2 threads (to accomodate CRAN requirements).}
}
\value{
The spectogram, a numeric real-valued matrix with dimensions
\code{dim = c(length(signal), n_freqs)}.
This matrix is wrapped into a S3-class \code{fcwtr_scalogram} so that plotting and
coercion functions can be used conveniently.
}
\description{
The core function of this package making use of the fCWT library. It processes
an input signal in form of a real valued numeric vector interpreted as an
evenly spaced time series and returns the absolute values of a spectogram,
i.e. a graph with a time and a frequency dimension.
}
\details{
The wavelet used in this calculation is the so called Morlet wavelet,
a sinusoidal wave modulated by a Gaussian whose spread is controlled by
the argument \code{sigma}.

See the original paper
Arts, L.P.A., van den Broek, E.L.
The fast continuous wavelet transformation (fCWT) for real-time, high-quality,
noise-resistant time–frequency analysis.
Nat Comput Sci 2, 47–58 (2022). \doi{10.1038/s43588-021-00183-z}
}
\examples{
ts_sin_440 <- sin((1:5000) * 2 * pi * 440 / 44100)

res <-
  fcwt(
    ts_sin_440,
    sample_freq = u(44.1, "kHz"),
    freq_begin = u(50, "Hz"),
    freq_end = u(1000, "Hz"),
    n_freqs = 10,
    sigma = 5
  )
}
