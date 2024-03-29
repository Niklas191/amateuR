% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive_plots.R
\name{plot_test}
\alias{plot_test}
\title{plot_test}
\usage{
plot_test(game_data, team)
}
\arguments{
\item{game_data}{A league data vector, which is created with \code{\link[amateuR]{download_league_data}}.}

\item{team}{A team name as string.}
}
\description{
This is an internal function for \code{\link[amateuR]{plot_goal_difference}} and \code{\link[amateuR]{plot_points}}.
It performs a test if the team name is correct and if there are enough data points for a plot.
}
\keyword{internal}
