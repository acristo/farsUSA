% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions_ACO.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Print a map of crash over year and state of U.S.}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{integer with the number of desired state, ranging 1:56}

\item{year}{An integert with the desired year}
}
\value{
This function returns a map
}
\description{
This function prints a map with the position of every crash occurred over the state 'state.num' and the year 'year'
}
\examples{
fars_map_state(1, 2013)

}
