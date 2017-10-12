\name{IntCal13}
\alias{IntCal13}
\docType{data}
\title{
Atmospheric radiocarbon for the 0-50,000 yr BP period
}
\description{
Atmospheric radiocarbon calibration curve for the period 0 to 50,000 yr BP. This is the most recent update to the internationally agreed calibration curve and superseds \code{\link{IntCal09}}.
}
\usage{data(IntCal13)}
\format{
  A data frame with 5140 observations on the following 5 variables.
  \describe{
    \item{\code{CAL.BP}}{Calibrated age in years Before Present (BP).}
    \item{\code{C14.age}}{C14 age in years BP.}
    \item{\code{Error}}{Error estimate for \code{C14.age}.}
    \item{\code{Delta.14C}}{Delta.14C value in per mil.}
    \item{\code{Sigma}}{Standard deviation of \code{Delta.14C} in per mil.}
  }
}
\details{
\code{Deltal.14C} is age-corrected as per Stuiver and Polach (1977). All details about the derivation of this dataset are provided in Reimer et al. (2013).
}

\source{
\url{
  http://www.radiocarbon.org/IntCal13%20files/intcal13.14c
  }
}
\references{
Reimer PJ, Bard E, Bayliss A, Beck JW, Blackwell PG, Bronk Ramsey C, Buck CE,
Cheng H, Edwards RL, Friedrich M, Grootes PM, Guilderson TP, Haflidason H, 
Hajdas I, Hatte C, Heaton TJ, Hogg AG, Hughen KA, Kaiser KF, Kromer B, 
Manning SW, Niu M, Reimer RW, Richards DA, Scott EM, Southon JR, Turney CSM,
van der Plicht J. 2013. IntCal13 and MARINE13 radiocarbon age calibration curves 0-50000 years calBP. Radiocarbon 55(4): 1869-1887. DOI: 10.2458/azu_js_rc.55.16947


M. Stuiver and H. A. Polach. 1977. Rerporting of C-14 data. Radiocarbon, 19(3):355 - 363.
}
\examples{


     plot(IntCal13$CAL.BP,IntCal13$C14.age-IntCal13$Error,type="l",col=2,
          xlab="cal BP",ylab="14C BP")
     lines(IntCal13$CAL.BP,IntCal13$C14.age+IntCal13$Error,col=2)

     plot(IntCal13$CAL.BP,IntCal13$Delta.14C+IntCal13$Sigma,type="l",col=2,
          xlab="cal BP",ylab="Delta14C")
     lines(IntCal13$CAL.BP,IntCal13$Delta.14C-IntCal13$Sigma,col=2)

}
\keyword{datasets}
