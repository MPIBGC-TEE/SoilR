\name{fW.RothC}
\alias{fW.RothC}
\usage{fW.RothC(P,
E,
S.Thick=23,
pClay=23.4,
pE=0.75,
bare=FALSE)}
\arguments{
\item{P}{A vector with monthly precipitation (mm).}
\item{E}{A vector with same length with open pan evaporation or evapotranspiration (mm).}
\item{S.Thick}{Soil thickness in cm. Default for Rothamsted is 23 cm.}
\item{pClay}{Percent clay.}
\item{pE}{Evaporation coefficient. If open pan evaporation is used pE=0.75. If Potential evaporation is used, pE=1.0.}
\item{bare}{Logical. Under bare soil conditions, bare=TRUE. Dafault is set under vegetated soil.}
}
\description{Calculates the effects of moisture (precipitation and pan evaporation) on decomposition rates according to the RothC model.}
\references{Coleman, K., and D. S. Jenkinson (1999), RothC-26.3 A model for the turnover of carbon in soil: 
model description and windows user guide (modified 2008), 47 pp, IACR Rothamsted, Harpenden.}
\title{Effects of moisture on decomposition rates according to the RothC model}
\value{A data.frame with accumulated top soil moisture deficit 
(Acc.TSMD) and the rate modifying factor b. }
