% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/montyhall.R
\name{play_n_games}
\alias{play_n_games}
\title{Play the Monty Hall game n times and display results}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{n=100}{A number to determine how many times to iterate the function}
}
\value{
The function returns a table of results and displays the proportion
of wins and losses for each strategy
}
\description{
\code{play_n_games()} plays a Monty Hall game n times displays the outcome
}
\details{
The function plays a 3-door Monty Hall game with 2 goats and 1 car.The
function loops for a specified number of times. The outcomes arr displayed
for the number of times the contestant wins or loses with each strategy,
either staying or switching
}
\examples{
  play_n_games(n=1000)

}
