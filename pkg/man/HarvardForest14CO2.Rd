\name{HarvardForest14CO2}
\alias{HarvardForest14CO2}
\docType{data}
\title{
 Delta14C in soil CO2 efflux from Harvard Forest
}
\description{
Measurements of Delta14C in soil CO2 efflux conducted at Harvard Forest, USA, between 1996 and 2010.
}
\usage{data(HarvardForest14CO2)}
\format{
  A data frame with the following 3 variables.
  \describe{
    \item{\code{Year}}{A numeric vector with the date of measurement in years}
    \item{\code{D14C}}{A numeric vector with the value of the Delta 14C value measured in CO2 efflux in per mil}
    \item{\code{Site}}{A factor indicating the site where measurements were made. NWN: Northwest Near, Drydown: Rainfall exclusion experiment.}
  }
}
\details{Samples for isotopic measurements of soil CO2 efflux were collected from chambers that enclosed an air headspace in contact with the soil surface in the absence of vegetation using a closed dynamic chamber system to collect accumulated CO2 in stainless steel traps with a molecular sieve inside. See Sierra et al. (2012) for additional details.}
\references{Sierra, C. A., Trumbore, S. E., Davidson, E. A., Frey, S. D., Savage, K. E., and Hopkins, F. M. 2012. Predicting decadal trends and transient responses of radiocarbon storage and fluxes in a temperate forest soil, Biogeosciences, 9, 3013-3028, doi:10.5194/bg-9-3013-2012}
\examples{
plot(HarvardForest14CO2[,1:2])
}
\keyword{datasets}
