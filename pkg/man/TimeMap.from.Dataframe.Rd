\name{TimeMap.from.Dataframe}
\alias{TimeMap.from.Dataframe}
\usage{TimeMap.from.Dataframe(dframe,
lag=0,
interpolation=splinefun)}
\arguments{
\item{dframe}{A data frame containing exactly two columns:
the first one is interpreted as time}
\item{lag}{a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
\item{interpolation}{A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.}
}
\title{TimeMap from Dataframe}
\description{This function is a deprecated constructor of the class TimeMap.}
\value{An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag
this serves as a saveguard for Model which thus can check that all involved functions of time are actually defined for the times of interest  }
