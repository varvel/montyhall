% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/montyhall.R
\name{determine_winner}
\alias{determine_winner}
\title{Determine whether the contestant wins or loses}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{final.pick}{A number between 1 and 3}

\item{game}{A length 3 character vector indicating the positions of
goats and the car.}
}
\value{
The function returns a character vector of either "WIN" or "LOSE"
}
\description{
\code{determine_winner()} evaluates whether the contestants wins or loses
}
\details{
The function determines whether the contestant's final pick is the
winning car door, or the losing goat door.
}
\examples{
  change_door()

}
