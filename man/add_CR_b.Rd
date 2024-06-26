% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_consumer_risks.R
\name{add_CR_b}
\alias{add_CR_b}
\title{Add Consumer Risks (CR) to data frame given failure probabilities}
\usage{
add_CR_b(data, M, pi)
}
\arguments{
\item{data}{The data frame containing all possible test plans.}

\item{M}{Monte Carlo simulation size.}

\item{pi}{A matrix containing the failure probability samples for different
test plans (rows represent samples, columns represent test plans).}
}
\value{
The data frame with an additional column for Consumer Risk (CR) values.
}
\description{
This function calculates and adds the Consumer Risk (CR) values to the
             input data frame for a Binomial Randomized Diagnostic Trial (RDT).
}
