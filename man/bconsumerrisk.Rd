% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bcomsumerrisk.R
\name{bconsumerrisk}
\alias{bconsumerrisk}
\title{Consumer's Risk for Multi-stage Binomial RDT}
\usage{
bconsumerrisk(M, n, c, pi, R)
}
\arguments{
\item{M}{Monte Carlo simulation size}

\item{n}{sample size in test plan.}

\item{c}{Maximum allowable failures in test plan.}

\item{pi}{Failure probability.}

\item{R}{Lower level reliability requirement.}
}
\value{
Probability of consumer's risk
}
\description{
Define the consumer's risk function which gets the probability of passing the test when the lower level reliability requirement is not satisfied (for binomial RDT).
}
