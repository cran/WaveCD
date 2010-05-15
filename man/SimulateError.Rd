\name{SimulateError}
\alias{SimulateError}
\title{ Simulate AR(p) error series }
\description{
This function finds an AR(P) error series with given length, coefficients and error standard deviation.   
}
\usage{
SimulateError(phi, SigE, n)
}

\arguments{
  \item{phi}{ Vector of AR(p) coefficients. }
  \item{SigE}{Standard deviation of errors in an AR(p) series.}
  \item{n}{ Length of series. }
}

\value{
Simulated series with AR(p) process.
}

\references{ 
M.S. Islam (2008). Periodicity, Change Detection and Prediction in Microarrays.
Ph.D. Thesis, University of Western Ontario.
}

\author{ M. Shahidul Islam}


\keyword{nonparametric}
