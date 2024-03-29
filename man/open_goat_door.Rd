% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/montyhall.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Open the Goat Door}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{game}{game A length 3 character vector indicating the positions of
goats and the car}

\item{a.pick}{A number between 1 and 3}
}
\value{
The function returns a number between 1 and 3
}
\description{
\code{open_goat_door()} selects the Goat Door to be opened
}
\details{
If the contestant's initial door selection is the car, the function
opens either remaining goat door at random. If the contestant's initial
door selection is a goat, then the other goat door is opened.
}
\examples{
  open_goat_door()

}
