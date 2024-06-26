% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardGompertz.R
\name{standardGompertz}
\alias{standardGompertz}
\title{Standard Gompertz Reliability Growth Function}
\usage{
standardGompertz(t, upperLimit, baseLine, growthRate)
}
\arguments{
\item{t}{The stage number (starting from 1, for stage2, t=1).}

\item{upperLimit}{The upper limit of reliability, where 0 < upperLimit <= 1.}

\item{baseLine}{The initial reliability at time=0, where 0 < baseLine < 1.}

\item{growthRate}{The growth pattern indicator, where 0 < growthRate < 1.
Smaller values of growthRate indicate rapid early reliability growth, while larger values indicate slow reliability growth.}
}
\value{
The reliability at the given stage based on the Standard Gompertz reliability growth model.
}
\description{
This function calculates the improved reliability at a given stage
using the Standard Gompertz reliability growth model.
}
\examples{
reliability_at_stage_5 <- standardGompertz(t = 5,
upperLimit = 0.99,
baseLine = 0.2,
growthRate = 0.5)

reliability_at_stage_5

}
