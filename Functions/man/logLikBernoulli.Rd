% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logLikBernoulli.R
\name{logLikBernoulli}
\alias{logLikBernoulli}
\title{Calculates the parameter p that maximizes the log-likelihood}
\usage{
logLikBernoulli(data)
}
\arguments{
\item{data}{A numeric vector containing binary outcomes (0s and 1s).}
}
\value{
The value of p that maximizes the log-likelihood.
}
\description{
This function takes a vector and calculates the parameter p that maximizes the log-likelihood
and it uses a grid-based search approach, with p in steps of 0.001.
}
\examples{
data = c(1, 0, 0, 0, 1, 1, 1)
logLikBernoulli(data)

}
