\docType{package}
\name{SoilR-package}
\alias{SoilR}
\alias{SoilR-package}
\title{SOILR}
\description{
The package allows you to play around with different Soil models.
}
\details{
  The typical workflow consists of the following steps:
  \itemize{
    \item{Create a model}
    \item{Inspect it}
  }
  The simplest way of creating a model is to use one of the top level functions for predefined models: \code{\link{predefinedModels}}. \cr
  The objects returned by these functions can be of different type, usually either \code{Model} or \code{Model14}.
  To inspect the behaviour of a model object these classes provide several methods to be found in their respective descriptions. ( \code{\link{Model-class}} or \code{\link{Model_14-class}} )\cr
  If none of the predefined models fits you can assemble your own model. The functions that create it are the constructors of the classes \code{Model} or \code{Model14}. By convention they have the same name as the class and are desribed here: \cr
  \code{\link{Model}},\cr
  \code{\link{Model_14}}.
}
