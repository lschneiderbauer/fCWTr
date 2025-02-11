% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sigma.R
\name{sigma_res}
\alias{sigma_res}
\title{Calculates Time/Frequency Resolution}
\usage{
sigma_res(sigma, freq)
}
\arguments{
\item{sigma}{The dimensionless positive parameter in \code{\link[=fcwt]{fcwt()}} controlling the wavelet spread.}

\item{freq}{The reference frequency where the frequency and time resolution are
calculated. Must be a quantity with frequency units (see \code{\link[=u]{u()}}).}
}
\value{
A named list with two elements: "time" and "freq", in time and frequency
units. They obey the property \eqn{time \times freq = \frac{8}{\pi}}.
}
\description{
For a given \code{sigma} and reference frequency \code{freq} this function calculates
the expected time and frequency resolution respectively.
}
\details{
Note that this resolution depends on the reference frequency, since the
FCWT scales the wavelet spread depending on the frequency in a natural way.
(in contrast to a simple fixed-windowed Fourier Transform).
}
\examples{
sigma_res(1, u(440, "Hz"))

}
\seealso{
Other sigma: 
\code{\link{sigma_freq_res_rel}()},
\code{\link{sigma_from_freq_res}()},
\code{\link{sigma_from_freq_res_rel}()},
\code{\link{sigma_from_time_res}()}
}
\concept{sigma}
