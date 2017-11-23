\description{All models need to specify the influx of material to the pools.
This parameter will be represented as an object of one of the subclasses of this class.
The most general form of influx supported up to now is a  vector valued function of time represented by
\code{\link{BoundInFlux-class}}. In the most simple case it is constant and represented by an object of class 
\code{\link{ConstInFlux-class}}. Such an object can for instance be created from a numeric vector.}
\name{InFlux-class}
\alias{InFlux-class}
\docType{class}
\section{Methods}{Exported methods directly defined for class InFlux:

  \describe{
    \item{GeneralInFlux}{\code{signature(object = "InFlux")}: ... } \code{\link{GeneralInFlux,InFlux-method}}
	 }}
\title{InFlux S4 class}
\section{Subclasses}{\describe{
	\code{\linkS4class{BoundInFlux}}\cr
	\code{\linkS4class{ConstInFlux}}\cr
	\code{\linkS4class{UnBoundInFlux}}\cr
}}
\section{Constructors}{The class is abstract ( \code{contains "VIRTUAL"}).
           It can therefore not be instanciated directly.
           Look at non virtual subclasses and their constructors!

There is also an \href{https://en.wikipedia.org/wiki/Abstract_factory_pattern}{abstract factory} that produces instances of different subclasses depending on the input:

	\code{\link{GeneralInFlux}}\cr}
