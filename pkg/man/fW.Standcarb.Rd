\name{fW.Standcarb}
\alias{fW.Standcarb}
\usage{fW.Standcarb(Moist,
MatricShape=5,
MatricLag=0,
MoistMin=30,
MoistMax=350,
DiffuseShape=15,
DiffuseLag=4)}
\arguments{
\item{Moist}{A scalar or vector containing values of moisture content of a litter or soil pool (\%).}
\item{MatricShape}{A scalar that determines when matric limit is reduced to the point that decay can begin to occur.}
\item{MatricLag}{A scalar used to offset the curve to the left or right.}
\item{MoistMin}{A scalar determining the minimum moisture content.}
\item{MoistMax}{A scalar determining the maximum moisture content without diffusion limitations.}
\item{DiffuseShape}{A scalar that determines the range of moisture contents where diffusion is not limiting.}
\item{DiffuseLag}{A scalar used to shift the point when moisture begins to limit diffusion.}
}
\title{Effects of moisture on decomposition rates according to the StandCarb model}
\description{Calculates the effects of moisture on decomposition rates according to the StandCarb model.}
\references{Harmon, M. E., and J. B. Domingo (2001), A users guide to STANDCARB version 2.0: 
A model to simulate carbon stores in forest stands. Oregon State University, Corvallis.}
\value{A data frame with limitation due to water potential (MatricLimit), limitation due to oxygen diffusion (DiffuseLimit), and the overall limitation of moisture on decomposition rates (MoistDecayIndex).}
