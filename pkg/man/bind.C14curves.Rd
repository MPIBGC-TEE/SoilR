\name{bind.C14curves}
\alias{bind.C14curves}
\usage{bind.C14curves(prebomb,
postbomb,
time.scale)}
\arguments{
\item{prebomb}{A pre-bomb radiocarbon dataset. They could be either \code{\link{IntCal09}} or \code{\link{IntCal13}}.}
\item{postbomb}{A post-bomb radiocarbon dataset. They could be any of the datasets in \code{\link{Hua2013}}.}
\item{time.scale}{A character indicating whether to report the results in years before present \code{BP} or anno domini \code{AD}.}
}
\description{This function takes a pre- and a post-bomb curve, binds them together, and reports the results back either in years BP or AD.}
\title{Binding of pre- and post-bomb Delta14C curves}
\value{A \code{data.frame} with 3 columns: years in AD or BP, the atmospheric Delta14C value, the standard deviation of the Delta14C value.}
