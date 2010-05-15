\name{WaveCD-package}
\alias{WaveCD}
\docType{package}
\title{
Detects and plots change points in a series
}
\description{
It finds and plots change points in a series using wavelet method. We compare the wavelet coefficients obtained at highest order resolution with Wang's threshold to detect change points. Double implementation of MODWT confirms the presence of break points in the series.
}
\details{
\tabular{ll}{
Package: \tab WaveCD\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-05-15\cr
License: \tab GPL (>= 2)\cr
}
}

\author{
M. Shahidul Islam
Maintainer: <islam@stats.uwo.ca>
}


\references{ 
M.S. Islam (2008). Periodicity, Change Detection and Prediction in Microarrays.
Ph.D. Thesis, University of Western Ontario.
}

\keyword{ package }

\examples{
# Finding break points in the series
set.seed(123)
x<-c(rnorm(150, sd=0.1), rnorm(250, 0.8, sd=0.1), rnorm(150, sd=0.1))
BreakPoints(x)


# Graphical displa of break points 
phi<-0.8
SigE<- 0.12
ex.c1<- c(rep(0, 80), rep(-0.7,30), rep(0,40))+ SimulateError(phi, SigE, 150)
ex.c2<- c(rep(0,40), rep(0.7, 30), rep(0, 50))+ SimulateError(phi, SigE, 120)
ex.c<-c(ex.c1, ex.c2)
PlotBreakPoints(ex.c, c(rep(1, 150),rep(2, 120)))
}
