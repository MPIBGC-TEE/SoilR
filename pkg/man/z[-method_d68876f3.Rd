\docType{methods}
\name{[,Model,character,missing,missing-method}
\title{[,Model,character,missing,missing-method 
 overload the [ ] operator for models and character vector}
\alias{[,Model,character,missing,missing-method}
\usage{\S4method{[}{Model,character,missing,missing}(x= ,
i= )}
\arguments{
\item{x}{: of class Model}
\item{i}{: of class character}
}
\description{This method is experimental and might change. User code should at the moment not rely on it. It overloads the [] operator for Model objects and acts as an alternative
to the get methods
so m["times"] is equivalent to getTimes(m), m["C"] to getC(m) and so on.}
