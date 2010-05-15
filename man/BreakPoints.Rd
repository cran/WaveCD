\name{BreakPoints}
\alias{BreakPoints}
\title{ Find break points in a given series}

\description{
This function applies maximum overlap discrete wavelet transform (MODWT) to find
break points in a series. We compare the wavelet coefficients obtained at highest order resolution with Wang's threshold to detect change points. Double application of the process distinguishes noise from a real jump.       
}

\usage{
BreakPoints(x)
}

\arguments{
  \item{x}{ a series }
}
\details{
Uses MODWT to detect breakpoints. See Islam (2008).
}

\value{
list of break points (end points not included)
}

\references{ 
M.S. Islam (2008). Periodicity, Change Detection and Prediction in Microarrays.
Ph.D. Thesis, University of Western Ontario.
}

\author{ M.Shahidul Islam}

\seealso{\code{\link{PlotBreakPoints}} }
\examples{
set.seed(123)
x<-c(rnorm(150), rnorm(250, 1), rnorm(200))
BreakPoints(x)
}

\keyword{nonparametric}
