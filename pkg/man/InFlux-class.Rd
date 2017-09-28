\title{InFlux S4 class}
\description{no Description}
\name{InFlux-class}
\alias{InFlux-class}
\docType{class}
\section{Methods}{  \describe{
    \item{GeneralInFlux}{\code{signature(object = "InFlux")}: ... } \code{\link{GeneralInFlux,InFlux-method}}
	 }}
\section{Subclasses}{\describe{
	\code{\link{BoundInFlux-class}}\cr
	\code{\link{ConstInFlux-class}}\cr
}}
\section{Constructors}{The class is abstract ( \code{contains "VIRTUAL"}).
           It can therefore not be instanciated directly.
           Look at non virtual subclasses and their constructors!

There is also an \href{https://en.wikipedia.org/wiki/Abstract_factory_pattern}{abstract factory} that produces instances of different subclasses depending on the input:

	\code{\link{GeneralInFlux}}\cr}
