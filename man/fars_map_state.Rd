% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{function "fars_map_state"}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{A number of a US state (number with 2 digits)}

\item{year}{A year (number with 4 digits)}
}
\value{
none
}
\description{
The function plot a map of a US state with the coordinates of the accidents in a year
}
\details{
The function stopped with the message invalid STATE number: ", <state.num>)"
   if no record with the state number in the data frame

   The function generate the message "no accidents to plot" and returns the invisible value NULL
   if there is not any accident in the selected data
}
\examples{
stateno <- 1
yyyy <- 2015
fars_map_state( state.num=stateno, year=yyyy )

}
