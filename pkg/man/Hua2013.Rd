\name{Hua2013}
\alias{Hua2013}
\docType{data}
\title{
Atmospheric radiocarbon for the period 1950-2010 from Hua et al. (2013)
}
\description{
Atmospheric radiocarbon for the period 1950-2010 reported by Hua et al. (2013) for 5 atmospheric zones.
}
\usage{data(Hua2013)}
\format{
  A \link{list} containing 5 data frames, each representing an atmospheric zone. The zones are: 
  NHZone1: northern hemisphere zone 1, NHZone2: northern hemisphere zone 2,
  NHZone3: northern hemisphere zone 3, SHZone12: southern hemisphere zones 1 and 2, 
  SHZone3: southern hemisphere zone 3. Each data frame contains a variable number of observations 
  on the following 5 variables.
  \describe{
    \item{\code{Year.AD}}{Year AD}
    \item{\code{mean.Delta14C}}{mean value of atmospheric radiocarbon reported as Delta14C}
    \item{\code{sd.Delta14C}}{standard deviation of atmospheric radiocarbon reported as Delta14C}
    \item{\code{mean.F14C}}{mean value of atmospheric radiocarbon reported as fraction modern F14C}
    \item{\code{sd.F14}}{standard deviation of atmospheric radiocarbon reported as fraction modern F14C}
  }
}
\details{
This dataset corresponds to Table S3 from Hua et al. (2013). For additional details see the original publication.
}
\source{
\url{https://journals.uair.arizona.edu/index.php/radiocarbon/article/view/16177}
}
\references{
Hua Q., M. Barbetti, A. Z. Rakowski. 2013. Atmospheric radiocarbon for the period 1950-2010. Radiocarbon 55(4):2059-2072.
}
\examples{

plot(Hua2013$NHZone1$Year.AD, Hua2013$NHZone1$mean.Delta14C, 
     type="l",xlab="Year AD",ylab=expression(paste(Delta^14,"C (\u2030)")))
lines(Hua2013$NHZone2$Year.AD,Hua2013$NHZone2$mean.Delta14C,col=2)
lines(Hua2013$NHZone3$Year.AD,Hua2013$NHZone3$mean.Delta14C,col=3)
lines(Hua2013$SHZone12$Year.AD,Hua2013$SHZone12$mean.Delta14C,col=4)
lines(Hua2013$SHZone3$Year.AD,Hua2013$SHZone3$mean.Delta14C,col=5)
legend(
	"topright",
	c(
		"Norther hemisphere zone 1",
		"Norther hemisphere zone 2",
		"Norther hemisphere zone 3",
                "Southern hemisphere zones 1 and 2",
		"Southern Hemispher zone 3"
	),
	lty=1,
	col=1:5,
	bty="n"
)

}
\keyword{datasets}
