\name{C14Atm_NH}
\alias{C14Atm_NH}
\docType{data}
\title{Post-bomb atmospheric 14C fraction}
\description{
    Atmospheric 14C concentrations for the post-bomb period expressed as Delta 14C in per mile. This dataset contains a combination of observations from locations in Europe and North America. It is representative for the Northern Hemisphere. 
}
\usage{data(C14Atm_NH)}
\format{
  A data frame with 111 observations on the following 2 variables.
  \describe{
    \item{\code{YEAR}}{a numeric vector with year of measurement.}
    \item{\code{Atmosphere}}{a numeric vector with the Delta 14 value of atmospheric CO2 in per mil.}
  }
}
\examples{
plot(C14Atm_NH,type="l")
}
\keyword{datasets}
