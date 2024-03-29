% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tau_and_initialise.R
\name{negloglik}
\alias{negloglik}
\title{Negative loglikelihood function}
\usage{
negloglik(params, goals_home, goals_visitor, team_home, team_visitor,
  structure)
}
\arguments{
\item{params}{list of paramters}

\item{goals_home}{likelihood parameter}

\item{goals_visitor}{likelihood parameter}

\item{team_home}{likelihood parameter}

\item{team_visitor}{likelihood parameter}

\item{structure}{correct structure format for parameters}
}
\description{
This is an internal function for \code{\link[amateuR]{estimate_params}}
}
\keyword{internal}
