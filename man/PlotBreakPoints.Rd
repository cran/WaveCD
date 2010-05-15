\name{PlotBreakPoints}
\alias{PlotBreakPoints}
\title{ Plot the series and detected jump points}
\description{
A lattice style plot is used to show break points for each chromosome
}
\usage{
PlotBreakPoints(x, Chromosome, layout = c(length(unique(Chromosome)),1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ any series of observations }
  \item{Chromosome}{ chromosome corresponding to data in x }
  \item{layout}{ the layout of the lattice plot}
  \item{...}{ arguments that can be passed to plot command}
}
\details{
Uses MODWT method described in Islam (2008).
}
\value{
lattice plot
}

\references{ 
M.S. Islam (2008). Periodicity, Change Detection and Prediction in Microarrays.
Ph.D. Thesis, University of Western Ontario.
}

\author{ M. Shahidul Islam}

\seealso{ \code{\link{BreakPoints}} }

\examples{
# Example-1: Data simulated from AR(1) process with phi=0.8
phi<-0.8
SigE<- 0.12
ex.c1<- c(rep(0, 80), rep(-0.7,30), rep(0,40))+ SimulateError(phi, SigE, 150)
ex.c2<- c(rep(0,40), rep(0.7, 30), rep(0, 50))+ SimulateError(phi, SigE, 120)
ex.c<-c(ex.c1, ex.c2)
PlotBreakPoints(ex.c, c(rep(1, 150),rep(2, 120)))


# Example-2: seven jump points

fx<-function(x){ifelse(x<50,0,
	ifelse(x>=50 && x<60,1,
	  ifelse(x>=60 && x<92,0,
	    ifelse(x>=92 && x<106,-1, 
              ifelse(x>=106 && x<145,0, 
		ifelse(x>=145 && x<169,1,
		  ifelse(x>=169 && x<=180,2, 0)))))))+rnorm(1,0,0.1)}


set.seed(123)
y<-sapply(1:200,fx)
PlotBreakPoints(y, c(rep(1, length(y))), type="l", lwd=1.7)


#
# CGH Data
#
data(arrayCGH)
# Plotting first 12 chromosomes 
PlotBreakPoints(arrayCGH[1:765,1], arrayCGH[1:765,2], type="l",layout=c(3,4))
# Plotting rest of the chromosomes
PlotBreakPoints(arrayCGH[766:1040,1], arrayCGH[766:1040,2], type="l",layout=c(3,4))

}

\keyword{ nonparametric }
