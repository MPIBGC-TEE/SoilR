\description{no Description}
\name{getDecompOp}
\alias{getDecompOp}
\usage{getDecompOp(object)}
\title{getDecompOp S4 generic}
\examples{# suppose you have somehow created a model
# e.g. by one of the predefined functions
years=seq(1901,2010,by=0.5)

Ex=GaudinskiModel14(
t=years,
ks=c(kr=1/3, koi=1/1.5, koeal=1/4, koeah=1/80, kA1=1/3, kA2=1/75, kM=1/110),
inputFc=C14Atm_NH
)
op <- getDecompOp(Ex)


# Now you can get properties of the decompostion operator e.g.

func <- getFunctionDefinition(op)

#func is a x valued funtion of time
# In this example it is even constant, which is boring.
# you can evaluate it for different times
func(0)
func(5)}
\arguments{
\item{object}{see the method arguments for details}
}
\section{Methods}{	\code{\link{getDecompOp,Model-method}}\cr
	\code{\link{getDecompOp,NlModel-method}}\cr}
