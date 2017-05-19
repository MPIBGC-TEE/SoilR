\name{eCO2}
\alias{eCO2}
\docType{data}
\title{
 Soil CO2 efflux from an incubation experiment
}
\description{
A dataset with soil CO2 efflux measurements from a laboratory incubation at controlled temperature and moisture conditions.
}
\usage{data(eCO2)}
\format{
  A data frame with the following 3 variables.
  \describe{
    \item{\code{Days}}{A numeric vector with the day of measurement after the experiment started.}
    \item{\code{eCO2mean}}{A numeric vector with the release flux of CO2. Units in ug C g-1 soil day-1.}
    \item{\code{eCO2sd}}{A numeric vector with the standard deviation of the release flux of CO2-C. Units in ug C g-1 soil day-1.}
  }
}
\details{
A laboratory incubation experiment was performed in March 2014 for a period of 35 days under controlled conditions of temperature (15 degrees Celcius), moisture (30 percent soil water content), and oxygen levels (20 percent). Soil CO2 measurements were taken using an automated system for gas sampling connected to an infrared gas analyzer. The soil was sampled at a boreal forest site (Caribou Poker Research Watershed, Alaska, USA). This dataset presents the mean and standard deviation of 4 replicates.
}
\examples{
head(eCO2)

plot(eCO2[,1:2],type="o",ylim=c(0,50),ylab="CO2 efflux (ug C g-1 soil day-1)")
arrows(eCO2[,1],eCO2[,2]-eCO2[,3],eCO2[,1],eCO2[,2]+eCO2[,3], angle=90,length=0.3,code=3)

}
\keyword{datasets}
