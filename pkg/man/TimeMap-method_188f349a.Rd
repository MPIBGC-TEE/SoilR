\docType{methods}
\name{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}
\title{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method 
 pass through constructor}
\alias{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}
\usage{\S4method{TimeMap}{TimeMap,ANY,ANY,ANY,ANY}(map= )}
\arguments{
\item{map}{: of class TimeMap, the object that will be returned unchanged}
}
\description{The function just returns its argument.
So any function that has to convert one of its argument can just call TimeMap on it 
even if the argument is allready one.}
