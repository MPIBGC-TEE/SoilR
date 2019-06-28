

#' AbsoluteFractionModern_from_Delta14C,numeric-method Converts from Delta14C
#' to Absolute Fraction Modern
#' 
#' Converts a number or vector containing Delta14C values to the appropriate
#' Absolute Fraction Modern values. Have a look at the methods for details.
#' 
#' 
#' @name AbsoluteFractionModern_from_Delta14C,numeric-method
#' @docType methods
#' @param delta14C : of class numeric, A numeric object containing the values
#' in Delta14C format
NULL





#' AbsoluteFractionModern_from_Delta14C,matrix-method Converts from Delta14C to
#' Absolute Fraction Modern
#' 
#' This method produces a matrix of Delta14C values from a Matrix or number of
#' Absolute Fraction Modern
#' 
#' 
#' @name AbsoluteFractionModern_from_Delta14C,matrix-method
#' @docType methods
#' @param delta14C : of class matrix, An object of class matrix containing the
#' values in Delta14C format
NULL





#' AbsoluteFractionModern_from_Delta14C S4 generic
#' 
#' no Description
#' 
#' 
#' @param delta14C see the method arguments for details
#' @section Methods:
#' \code{\link{AbsoluteFractionModern_from_Delta14C,matrix-method}}\cr
#' \code{\link{AbsoluteFractionModern_from_Delta14C,numeric-method}}\cr
NULL





#' add_plot,TimeMap-method overview plot
#' 
#' The method adds a simple overview plot of a the scalar, vector, matrix or
#' arrayvalued function plotting all time dependent component.
#' 
#' 
#' @name add_plot,TimeMap-method
#' @docType methods
#' @param x : of class TimeMap, the TimeMap Object to be plotted
#' @param ... : other plot parameters not implemented yet
NULL





#' add_plot S4 generic
#' 
#' no Description
#' 
#' 
#' @param x see the method arguments for details
#' @param ... see the method arguments for details
#' @section Methods: \code{\link{add_plot,TimeMap-method}}\cr
NULL





#' as.character,TimeMap-method convert TimeMap Objects to something printable.
#' 
#' This method is needed to print a TimeMap object.
#' 
#' 
#' @name as.character,TimeMap-method
#' @docType methods
#' @param x : of class TimeMap, An Object of class time map
#' @param ... : will be ignored
NULL





#' creates a character representation of the object in question
#' 
#' This function computes the carbon content of the pools as function of time
#' 
#' 
#' @param object The object to be printed.
#' @author Carlos A. Sierra <csierra@@bgc-jena.mpg.de>, Markus Mueller
#' <mamueller@@bgc-jena.mpg.de>
NULL





#' Objects containing the atmospheric 14C fraction and the format it is
#' provided in.
#' 
#' no Description
#' 
#' 
#' @name BoundFc-class
#' @docType class
#' @section Methods: No exported methods directly defined for class BoundFc:
#' 
#' Methods inherited from superclasses:
#' 
#' from class TimeMap:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ...
#' } \code{\link{GeneralDecompOp,TimeMap-method}}
#' \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{GeneralInFlux,TimeMap-method}}
#' \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime =
#' "ANY", times = "ANY", data = "ANY")}: ... }
#' \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
#' \item{add_plot}{\code{signature(x = "TimeMap")}: ... }
#' \code{\link{add_plot,TimeMap-method}} \item{as.character}{\code{signature(x
#' = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
#' \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getFunctionDefinition,TimeMap-method}}
#' \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getTimeRange,TimeMap-method}} }
NULL





#' BoundFc,character-method A constructor
#' 
#' The method constructs an object from a format string and other parameters
#' which are passed on to \code{\link{TimeMap}}
#' 
#' 
#' @name BoundFc,character-method
#' @docType methods
#' @param format : of class character, a string that specifies the format used
#' to represent the atmospheric fraction. Possible values are "Delta14C" which
#' is the default or "afn" the Absolute Fraction Normal representation
#' @param ... : passed on to TimeMap
NULL





#' BoundFc,missing-method A constructor
#' 
#' The method constructs an object from a list which must contain a
#' 
#' 
#' @name BoundFc,missing-method
#' @docType methods
#' @param format : of class missing, If this method is called the format
#' argument was not present.
#' @param ... : passed on to TimeMap
NULL





#' BoundFc S4 generic
#' 
#' no Description
#' 
#' 
#' @param format see the method arguments for details
#' @param ... see the method arguments for details
#' @section Methods: \code{\link{BoundFc,character-method}}\cr
#' \code{\link{BoundFc,missing-method}}\cr
NULL





#' BoundInFlux S4 class
#' 
#' defines a time dependent inputrate as function of time and including the
#' domain where the function is well defined. This can be used to avoid
#' interpolations out of range when mixing different time dependent data sets
#' 
#' 
#' @name BoundInFlux-class
#' @docType class
#' @section Methods: No exported methods directly defined for class
#' BoundInFlux:
#' 
#' Methods inherited from superclasses:
#' 
#' from class InFlux:
#' 
#' \describe{ \item{GeneralInFlux}{\code{signature(object = "InFlux")}: ... }
#' \code{\link{GeneralInFlux,InFlux-method}} } from class TimeMap:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ...
#' } \code{\link{GeneralDecompOp,TimeMap-method}}
#' \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{GeneralInFlux,TimeMap-method}}
#' \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime =
#' "ANY", times = "ANY", data = "ANY")}: ... }
#' \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
#' \item{add_plot}{\code{signature(x = "TimeMap")}: ... }
#' \code{\link{add_plot,TimeMap-method}} \item{as.character}{\code{signature(x
#' = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
#' \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getFunctionDefinition,TimeMap-method}}
#' \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getTimeRange,TimeMap-method}} }
NULL





#' a decomposition operator described by a matrix valued function of time
#' 
#' no Description
#' 
#' 
#' @name BoundLinDecompOp-class
#' @docType class
#' @section Methods: No exported methods directly defined for class
#' BoundLinDecompOp:
#' 
#' Methods inherited from superclasses:
#' 
#' from class DecompOp:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "DecompOp")}: ...
#' } \code{\link{GeneralDecompOp,DecompOp-method}} } from class TimeMap:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ...
#' } \code{\link{GeneralDecompOp,TimeMap-method}}
#' \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{GeneralInFlux,TimeMap-method}}
#' \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime =
#' "ANY", times = "ANY", data = "ANY")}: ... }
#' \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
#' \item{add_plot}{\code{signature(x = "TimeMap")}: ... }
#' \code{\link{add_plot,TimeMap-method}} \item{as.character}{\code{signature(x
#' = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
#' \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getFunctionDefinition,TimeMap-method}}
#' \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getTimeRange,TimeMap-method}} }
NULL





#' BoundLinDecompOp,ANY-method a constructor
#' 
#' Creates a BoundLinDecompOp Object.
#' 
#' 
#' @name BoundLinDecompOp,ANY-method
#' @docType methods
#' @param map : of class ANY, passed on to TimeMap
#' @param ... : passed on to TimeMap
NULL





#' BoundLinDecompOp,UnBoundLinDecompOp-method convert a UnBoundLinDecompOp to a
#' BoundLinDecompOp
#' 
#' The method creates a BoundLinDecompOp consisting of a constant time
#' dependent function and the limits of its domain (starttime and endtime) set
#' to -Inf and Inf respectively
#' 
#' 
#' @name BoundLinDecompOp,UnBoundLinDecompOp-method
#' @docType methods
#' @param map : of class UnBoundLinDecompOp
#' @param starttime : the left hand boundary of the valid time interval
#' @param endtime : the right hand boundary of the valid time interval
NULL





#' BoundLinDecompOp S4 generic
#' 
#' no Description
#' 
#' 
#' @param map see the method arguments for details
#' @param ... see the method arguments for details
#' @section Methods: \code{\link{BoundLinDecompOp,ANY-method}}\cr
#' \code{\link{BoundLinDecompOp,UnBoundLinDecompOp-method}}\cr
NULL





#' Post-bomb atmospheric 14C fraction
#' 
#' Atmospheric 14C concentrations for the post-bomb period expressed as Delta
#' 14C in per mile. This dataset contains a combination of observations from
#' locations in Europe and North America. It is representative for the Northern
#' Hemisphere.
#' 
#' 
#' @name C14Atm_NH
#' @docType data
#' @format A data frame with 111 observations on the following 2 variables.
#' \describe{ \item{list("YEAR")}{a numeric vector with year of measurement.}
#' \item{list("Atmosphere")}{a numeric vector with the Delta 14 value of
#' atmospheric CO2 in per mil.} }
#' @keywords datasets
#' @examples
#' 
#' plot(C14Atm_NH,type="l")
#' 
NULL





#' Atmospheric 14C fraction
#' 
#' Atmospheric 14C fraction in units of Delta14C for the bomb period in the
#' northern hemisphere.
#' 
#' 
#' @name C14Atm
#' @docType data
#' @format A data frame with 108 observations on the following 2 variables.
#' \describe{ \item{list("V1")}{a numeric vector} \item{list("V2")}{a numeric
#' vector} }
#' @note This function will be deprecated soon. Please use \link{C14Atm_NH} or
#' \link{Hua2013} instead.
#' @keywords datasets
#' @examples
#' 
#' #Notice that C14Atm is a shorter version of C14Atm_NH
#' require("SoilR")
#' data("C14Atm_NH")
#' plot(C14Atm_NH,type="l")
#' lines(C14Atm,col=2)
#' 
NULL





#' ConstFc S4 class
#' 
#' no Description
#' 
#' 
#' @name ConstFc-class
#' @docType class
#' @section Methods: No exported methods directly defined for class ConstFc:
NULL





#' ConstInFlux S4 class
#' 
#' defines a constant inputrate
#' 
#' 
#' @name ConstInFlux-class
#' @docType class
#' @section Methods: Exported methods directly defined for class ConstInFlux:
#' 
#' \describe{ \item{getFunctionDefinition}{\code{signature(object =
#' "ConstInFlux")}: ... }
#' \code{\link{getFunctionDefinition,ConstInFlux-method}}
#' \item{getTimeRange}{\code{signature(object = "ConstInFlux")}: ... }
#' \code{\link{getTimeRange,ConstInFlux-method}} }
#' 
#' Methods inherited from superclasses:
#' 
#' from class InFlux:
#' 
#' \describe{ \item{GeneralInFlux}{\code{signature(object = "InFlux")}: ... }
#' \code{\link{GeneralInFlux,InFlux-method}} }
NULL





#' ConstInFlux,numeric-method constructor
#' 
#' the method converts a vector of constant input rates into an object of Class
#' \code{\link{ConstInFlux}}.
#' 
#' 
#' @name ConstInFlux,numeric-method
#' @docType methods
#' @param map : of class numeric
NULL





#' ConstInFlux S4 generic
#' 
#' no Description
#' 
#' 
#' @param map see the method arguments for details
#' @section Methods: \code{\link{ConstInFlux,numeric-method}}\cr
NULL





#' constant decomposition operator
#' 
#' no Description
#' 
#' 
#' @name ConstLinDecompOp-class
#' @docType class
#' @section Methods: Exported methods directly defined for class
#' ConstLinDecompOp:
#' 
#' \describe{ \item{getFunctionDefinition}{\code{signature(object =
#' "ConstLinDecompOp")}: ... }
#' \code{\link{getFunctionDefinition,ConstLinDecompOp-method}}
#' \item{getMeanTransitTime}{\code{signature(object = "ConstLinDecompOp")}: ...
#' } \code{\link{getMeanTransitTime,ConstLinDecompOp-method}}
#' \item{getTimeRange}{\code{signature(object = "ConstLinDecompOp")}: ... }
#' \code{\link{getTimeRange,ConstLinDecompOp-method}}
#' \item{getTransitTimeDistributionDensity}{\code{signature(object =
#' "ConstLinDecompOp")}: ... }
#' \code{\link{getTransitTimeDistributionDensity,ConstLinDecompOp-method}} }
#' 
#' Methods inherited from superclasses:
#' 
#' from class DecompOp:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "DecompOp")}: ...
#' } \code{\link{GeneralDecompOp,DecompOp-method}} }
NULL





#' ConstLinDecompOp,matrix-method construct from matric
#' 
#' This method creates a ConstLinDecompOp from a matrix The operator is assumed
#' to act on the vector of carbon stocks by multiplication of the (time
#' invariant) matrix from the left.
#' 
#' 
#' @name ConstLinDecompOp,matrix-method
#' @docType methods
#' @param mat : of class matrix
NULL





#' ConstLinDecompOp S4 generic
#' 
#' no Description
#' 
#' 
#' @param mat see the method arguments for details
#' @section Methods: \code{\link{ConstLinDecompOp,matrix-method}}\cr
NULL





#' decomposition operator
#' 
#' The decomposition operator is a necessary ingredient of any Model. Very
#' generally it describes the fluxes between the pools and to the exterior as
#' functions of time and the pool contents.  SoilR arranges different
#' decomposition operators into different classes, to determine which
#' computations can be performed with them (which methods can be called on
#' them). The simplest and least general decomposition operator is
#' \code{\link{ConstLinDecompOp-class}} which can be created from a constant
#' reservoir matrix.  Since it is the least general (most specific) the
#' additional information can be used to compute more and more specific results
#' (more methods) not available for the more abstract sub classes (for instance
#' \code{\link{BoundLinDecompOp-class}} that are necessary to model the more
#' general situations where the decomposition and transfer rates are functions
#' of time. The different decomposition operators are created by class-specific
#' functions, called constructors. (see the constructors section of this help
#' page)
#' 
#' 
#' @name DecompOp-class
#' @docType class
#' @section Methods: Exported methods directly defined for class DecompOp:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "DecompOp")}: ...
#' } \code{\link{GeneralDecompOp,DecompOp-method}} }
NULL





#' deprecated decomposition operator class
#' 
#' no Description
#' 
#' 
#' @name DecompositionOperator-class
#' @docType class
#' @section Methods: Exported methods directly defined for class
#' DecompositionOperator:
#' 
#' \describe{ \item{getFunctionDefinition}{\code{signature(object =
#' "DecompositionOperator")}: ... }
#' \code{\link{getFunctionDefinition,DecompositionOperator-method}}
#' \item{getTimeRange}{\code{signature(object = "DecompositionOperator")}: ...
#' } \code{\link{getTimeRange,DecompositionOperator-method}} }
#' 
#' Methods inherited from superclasses:
#' 
#' from class DecompOp:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "DecompOp")}: ...
#' } \code{\link{GeneralDecompOp,DecompOp-method}} }
NULL





#' Delta14C_from_AbsoluteFractionModern,numeric-method Converts to Delta14C
#' format
#' 
#' This method produces Delta14C values from Absolute Fraction Modern Have a
#' look at the methods for details.
#' 
#' 
#' @name Delta14C_from_AbsoluteFractionModern,numeric-method
#' @docType methods
#' @param AbsoluteFractionModern : of class numeric, A numeric object
#' containing the values in Absolute Fraction Modern format
NULL





#' Delta14C_from_AbsoluteFractionModern,matrix-method Converts Absolute
#' Fraction Modern values to Delta14C
#' 
#' This method produces a matrix of Delta14C values from a matrix of values in
#' Absolute Fraction Modern.
#' 
#' 
#' @name Delta14C_from_AbsoluteFractionModern,matrix-method
#' @docType methods
#' @param AbsoluteFractionModern : of class matrix, An object of class matrix
#' containing the values in Absolute Fraction Modern format
NULL





#' Delta14C_from_AbsoluteFractionModern S4 generic
#' 
#' no Description
#' 
#' 
#' @param AbsoluteFractionModern A numeric object
#' @section Methods:
#' \code{\link{Delta14C_from_AbsoluteFractionModern,matrix-method}}\cr
#' \code{\link{Delta14C_from_AbsoluteFractionModern,numeric-method}}\cr
NULL





#' Soil CO2 efflux from an incubation experiment
#' 
#' A dataset with soil CO2 efflux measurements from a laboratory incubation at
#' controlled temperature and moisture conditions.
#' 
#' A laboratory incubation experiment was performed in March 2014 for a period
#' of 35 days under controlled conditions of temperature (15 degrees Celsius),
#' moisture (30 percent soil water content), and oxygen levels (20 percent).
#' Soil CO2 measurements were taken using an automated system for gas sampling
#' connected to an infrared gas analyzer. The soil was sampled at a boreal
#' forest site (Caribou Poker Research Watershed, Alaska, USA). This dataset
#' presents the mean and standard deviation of 4 replicates.
#' 
#' @name eCO2
#' @docType data
#' @format A data frame with the following 3 variables.  \describe{
#' \item{list("Days")}{A numeric vector with the day of measurement after the
#' experiment started.} \item{list("eCO2mean")}{A numeric vector with the
#' release flux of CO2. Units in ug C g-1 soil day-1.} \item{list("eCO2sd")}{A
#' numeric vector with the standard deviation of the release flux of CO2-C.
#' Units in ug C g-1 soil day-1.} }
#' @keywords datasets
#' @examples
#' 
#' head(eCO2)
#' 
#' plot(eCO2[,1:2],type="o",ylim=c(0,50),ylab="CO2 efflux (ug C g-1 soil day-1)")
#' arrows(eCO2[,1],eCO2[,2]-eCO2[,3],eCO2[,1],eCO2[,2]+eCO2[,3], angle=90,length=0.3,code=3)
#' 
#' 
NULL





#' Fc S4 class
#' 
#' The \eqn{^{14}C}{14C} fraction is a necessary ingredient of any
#' \code{\linkS4class{Model_14}} object. In the most general case it is a real
#' valued function of time, accompanied by a string describing the unit or
#' format (i.e. "Delta14C" or "afn" for Absolute Fraction Modern) . In the most
#' simple case it is constant real number plus format.
#' 
#' 
#' @name Fc-class
#' @docType class
#' @section Methods: No exported methods directly defined for class Fc:
NULL





#' GeneralDecompOp,list-method creates a BoundLinDecompOp from a nested list of
#' a times vector and a list of matrices (a matrix for each time step)
#' 
#' The resulting operator is creted by a call to the constructor of class
#' BoundLinDecompOp
#' 
#' 
#' @name GeneralDecompOp,list-method
#' @docType methods
#' @param object : of class list
NULL





#' GeneralDecompOp,TimeMap-method creates a BoundLinDecompOp from a TimeMap
#' object
#' 
#' The resulting operator is creted by a call to the constructor of class
#' BoundLinDecompOp
#' 
#' 
#' @name GeneralDecompOp,TimeMap-method
#' @docType methods
#' @param object : of class TimeMap
NULL





#' GeneralDecompOp,function-method creates a UnBoundLinDecompOp from a matrix
#' valued function
#' 
#' The resulting operator is created by a call to the constructor of class
#' UnBoundLinDecompOp
#' 
#' 
#' @name GeneralDecompOp,function-method
#' @docType methods
#' @param object : of class function
NULL





#' GeneralDecompOp,DecompOp-method pass through factory
#' 
#' This method handles the case that no actual construction is necessary since
#' the argument is already of a subclass of DecompOp. See the \code{subclasses}
#' section of \code{\link{DecompOp-class}}
#' 
#' 
#' @name GeneralDecompOp,DecompOp-method
#' @docType methods
#' @param object : of class DecompOp
NULL





#' GeneralDecompOp,matrix-method creates a ConstanDecompOp from a matrix
#' 
#' The resulting operator is creted by a call to the constructor of class
#' ConstLinDecompOp
#' 
#' 
#' @name GeneralDecompOp,matrix-method
#' @docType methods
#' @param object : of class matrix
NULL





#' GeneralDecompOp S4 generic
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{GeneralDecompOp,DecompOp-method}}\cr
#' \code{\link{GeneralDecompOp,TimeMap-method}}\cr
#' \code{\link{GeneralDecompOp,function-method}}\cr
#' \code{\link{GeneralDecompOp,list-method}}\cr
#' \code{\link{GeneralDecompOp,matrix-method}}\cr
NULL





#' GeneralInFlux,InFlux-method pass through conversion
#' 
#' This method handles the case that no actual conversion is necessary since
#' the argument is already of a subclass of \link{InFlux-class}
#' 
#' 
#' @name GeneralInFlux,InFlux-method
#' @docType methods
#' @param object : of class InFlux
NULL





#' GeneralInFlux,list-method creates a BoundInFlux from a nested list of a
#' times vector and a list of vectors (a vector for each time step)
#' 
#' The resulting object is created by a call to the constructor of class
#' BoundInFlux
#' 
#' 
#' @name GeneralInFlux,list-method
#' @docType methods
#' @param object : of class list
NULL





#' GeneralInFlux,numeric-method conversion of a vector to an object of class
#' \code{\link{ConstInFlux}}
#' 
#' This method enables the model creating functions to handle constant input
#' streams
#' 
#' 
#' @name GeneralInFlux,numeric-method
#' @docType methods
#' @param object : of class numeric
NULL





#' GeneralInFlux,TimeMap-method create a BoundInFlux from a TimeMap object
#' 
#' The method is used to ensure compatibility TimeMap class. The resulting
#' BoundInFlux is created by a call to the constructor BoundInFlux(object) of
#' that class.
#' 
#' 
#' @name GeneralInFlux,TimeMap-method
#' @docType methods
#' @param object : of class TimeMap
NULL





#' GeneralInFlux,function-method creates a UnBoundInFlux from a vector valued
#' function
#' 
#' The resulting operator is created by a call to the constructor of class
#' UnBoundInFlux. You should only use this if the domain of your function is
#' the complete time axis (-Inf,+Inf). If your function has a finite domain
#' create an object of class \code{\link{BoundInFlux-class}} ### by calling
#' \code{\link{BoundInFlux}}. This will activeate checks on that avoid
#' unintended extrapolation.
#' 
#' 
#' @name GeneralInFlux,function-method
#' @docType methods
#' @param object : of class function
NULL





#' GeneralInFlux S4 generic
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{GeneralInFlux,InFlux-method}}\cr
#' \code{\link{GeneralInFlux,TimeMap-method}}\cr
#' \code{\link{GeneralInFlux,function-method}}\cr
#' \code{\link{GeneralInFlux,list-method}}\cr
#' \code{\link{GeneralInFlux,numeric-method}}\cr
NULL





#' getAccumulatedRelease,Model-method time integrals of release fluxes per pool
#' 
#' The method integrates the release flux of every pool for all times in the
#' interval specified by the model definition.
#' 
#' 
#' @name getAccumulatedRelease,Model-method
#' @docType methods
#' @param object : of class Model
NULL





#' getAccumulatedRelease S4 generic
#' 
#' no Description
#' 
#' 
#' @param object A Model object (e.g. of class Model or Model14) Have a look at
#' the methods for details.
#' @return A n x m matrix of cummulative release fluxes with m columns
#' representing the number of pools, and n rows representing the times as
#' specified by the argument \code{t} in \code{\link{GeneralModel}} or other
#' model creating functions.
#' @section Methods: \code{\link{getAccumulatedRelease,Model-method}}\cr
NULL





#' getC,Model-method
#' 
#' This function computes the value for C for each time and pool.
#' 
#' This function takes a Model object, which represents a system of ODEs of the
#' form \deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t)
#' \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} and solves the system for
#' \eqn{\mathbf{C}(t)}{C(t)}. The numerical solver used can be specified in the
#' constructor of the Model class e.g. \code{\link{Model}},
#' \code{\link{GeneralModel}}.
#' 
#' @name getC,Model-method
#' @docType methods
#' @param object : of class Model
#' @return A matrix with m columns representing the number of pools, and n rows
#' representing the times as specified by the argument \code{t} in
#' \code{\link{Model}},\code{\link{GeneralModel}} or another model creating
#' function.
#' @seealso See examples in \code{\link{GeneralModel}},
#' \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}},
#' \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.
NULL





#' getC,NlModel-method compute the solution for the content of the pools
#' 
#' This function computes the value for C for each time and pool.
#' 
#' 
#' @name getC,NlModel-method
#' @docType methods
#' @param object : of class NlModel, the model
#' @param as.closures : a flag that in TRUE will return an approximating
#' function instead of the values
NULL





#' getC S4 generic
#' 
#' no Description
#' 
#' 
#' @param object some model object, the actual class depends on the method
#' used.
#' @param as.closures if set to TRUE instead of a matrix a list of functions
#' will be returned.
#' @return A matrix with m columns representing the number of pools, and n rows
#' representing the times as specified by the argument \code{t} in
#' \code{\link{GeneralModel}} or another model creating function.
#' @section Methods: \code{\link{getC,Model-method}}\cr
#' \code{\link{getC,NlModel-method}}\cr
NULL





#' getC14,Model_14-method
#' 
#' This function computes the value for \eqn{^{14}C}{14C} (mass or
#' concentration ) as function of time
#' 
#' 
#' @name getC14,Model_14-method
#' @docType methods
#' @param object : of class Model_14
NULL





#' This function
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getC14,Model_14-method}}\cr
NULL





#' getDecompOp,Model-method
#' 
#' Extracts the Operator from a model object
#' 
#' 
#' @name getDecompOp,Model-method
#' @docType methods
#' @param object : of class Model
NULL





#' getDecompOp,NlModel-method
#' 
#' This method returns the Decomposition Operator of the Model
#' 
#' 
#' @name getDecompOp,NlModel-method
#' @docType methods
#' @param object : of class NlModel
NULL





#' getDecompOp S4 generic
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getDecompOp,Model-method}}\cr
#' \code{\link{getDecompOp,NlModel-method}}\cr
#' @examples
#' # suppose you have somehow created a model
#' # e.g. by one of the predefined functions
#' years=seq(1901,2010,by=0.5)
#' 
#' Ex=GaudinskiModel14(
#' t=years,
#' ks=c(kr=1/3, koi=1/1.5, koeal=1/4, koeah=1/80, kA1=1/3, kA2=1/75, kM=1/110),
#' inputFc=C14Atm_NH
#' )
#' op <- getDecompOp(Ex)
#' 
#' 
#' # Now you can get properties of the decompostion operator e.g.
#' 
#' func <- getFunctionDefinition(op)
#' 
#' #func is a x valued funtion of time
#' # In this example it is even constant, which is boring.
#' # you can evaluate it for different times
#' func(0)
#' func(5)
NULL





#' getF14,Model_14-method radiocarbon
#' 
#' Calculates the radiocarbon fraction for each pool at each time step.
#' 
#' 
#' @name getF14,Model_14-method
#' @docType methods
#' @param object : of class Model_14, The model
NULL





#' Computes the \eqn{\frac{^{14}C}{C}}{14C/C} ratio
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getF14,Model_14-method}}\cr
NULL





#' getF14C,Model_14-method read access to the models F14C variable
#' 
#' The model was created with a F14C object to describe the atmospheric 14C
#' content. This method serves to investigate those settings from the model.
#' 
#' 
#' @name getF14C,Model_14-method
#' @docType methods
#' @param object : of class Model_14
NULL





#' This function
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getF14C,Model_14-method}}\cr
NULL





#' getF14R,Model_14-method average radiocarbon fraction weighted by
#' carbonrelease
#' 
#' Calculates the average radiocarbon fraction weighted by the amount of carbon
#' release at each time step.
#' \eqn{\overline{F_R}=\frac{\sum_{i=1}^{n}{^{14}R_i}}{\sum_{i=1}^{n}{R_i}}}{(14R_1(t)+...+14R_n(t))
#' )/(R_1(t)+...R_n(t)))} Where \eqn{^{14}R_i(t)}{14R_i(t)} is the time
#' dependent release of \eqn{^{14}C}{14C} of pool \eqn{i} and
#' \eqn{R_i(t)}{R_i(t)} the release of all carbon isotops of pool \eqn{i}.
#' Since the result is always in Absolute Fraction Modern format wie have to
#' convert it to Delta14C
#' 
#' 
#' @name getF14R,Model_14-method
#' @docType methods
#' @param object : of class Model_14, an object
NULL





#' This function
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getF14R,Model_14-method}}\cr
NULL





#' getFunctionDefinition,DecompositionOperator-method
#' 
#' extract the function definition (the R-function)
#' 
#' 
#' @name getFunctionDefinition,DecompositionOperator-method
#' @docType methods
#' @param object : of class DecompositionOperator
NULL





#' getFunctionDefinition,TransportDecompositionOperator-method
#' 
#' extract the function definition (the R-function) from the object
#' 
#' 
#' @name getFunctionDefinition,TransportDecompositionOperator-method
#' @docType methods
#' @param object : of class TransportDecompositionOperator
NULL





#' getFunctionDefinition,UnBoundInFlux-method creates a constant timedependent
#' function and returns it
#' 
#' The method creates a timedependent function from the existing matrix
#' describing the operator
#' 
#' 
#' @name getFunctionDefinition,UnBoundInFlux-method
#' @docType methods
#' @param object : of class UnBoundInFlux
NULL





#' getFunctionDefinition,TimeMap-method
#' 
#' extract the function definition (the R-function)
#' 
#' 
#' @name getFunctionDefinition,TimeMap-method
#' @docType methods
#' @param object : of class TimeMap
NULL





#' getFunctionDefinition,ConstInFlux-method
#' 
#' create the (constant) function of time that is required by the models
#' 
#' 
#' @name getFunctionDefinition,ConstInFlux-method
#' @docType methods
#' @param object : of class ConstInFlux
#' @return A constant function of time that is used by the Models to represent
#' the input fluxes.
NULL





#' getFunctionDefinition,UnBoundLinDecompOp-method creates a constant
#' timedependent function and returns it
#' 
#' The method creates a timedependent function from the existing matrix
#' describing the operator
#' 
#' 
#' @name getFunctionDefinition,UnBoundLinDecompOp-method
#' @docType methods
#' @param object : of class UnBoundLinDecompOp
NULL





#' getFunctionDefinition,ConstLinDecompOp-method creates a constant
#' timedependent function and returns it
#' 
#' The method creates a timedependent function from the existing matrix
#' describing the operator
#' 
#' 
#' @name getFunctionDefinition,ConstLinDecompOp-method
#' @docType methods
#' @param object : of class ConstLinDecompOp
NULL





#' getFunctionDefinition S4 generic
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getFunctionDefinition,ConstInFlux-method}}\cr
#' \code{\link{getFunctionDefinition,ConstLinDecompOp-method}}\cr
#' \code{\link{getFunctionDefinition,DecompositionOperator-method}}\cr
#' \code{\link{getFunctionDefinition,TimeMap-method}}\cr
#' \code{\link{getFunctionDefinition,TransportDecompositionOperator-method}}\cr
#' \code{\link{getFunctionDefinition,UnBoundInFlux-method}}\cr
#' \code{\link{getFunctionDefinition,UnBoundLinDecompOp-method}}\cr
NULL





#' getMeanTransitTime,ConstLinDecompOp-method compute the mean transit time
#' 
#' This method computes the mean transit time for the linear time invariant
#' system that can be constructed from the given operator and input
#' distribution.
#' 
#' It relies on the mehtod \code{getTransitTimeDistributionDensity} using the
#' same arguments.
#' 
#' To compute the mean transit time for the distribution we have to compute the
#' integral \deqn{ \bar{T} = \int_0^\infty T \cdot S_r\left(
#' \frac{\vec{I}}{I},0,T\right)\; dT } for the numerically computed density. To
#' avoid issues with numerical integration we dont use \eqn{\infty}{\infty} as
#' upper limit but cut off the integragion interval prematurely. For this
#' purpose we calculate a maximum response time of the system as \cite{Lasaga}
#' \deqn{ \tau_{cycle} = \frac{1}{|\min(\lambda_i)|} } where
#' \eqn{\lambda_i}{\lambda_i} are non-zero eigenvalues of the matrix \eqn{{\bf
#' A}}{{\bf A}}.
#' 
#' @name getMeanTransitTime,ConstLinDecompOp-method
#' @docType methods
#' @param object : of class ConstLinDecompOp
#' @param inputDistribution : distribution of the inputs
#' @references Lasaga, A.: The kinetic treatment of geochemical cycles,
#' Geochimica et Cosmochimica Acta, 44, 815 -- 828,
#' doi10.1016/0016-7037(80)90263-X, 1980.
NULL





#' This function
#' 
#' no Description
#' 
#' 
#' @param object a DecompOp Object.
#' @param inputDistribution a vector of length equal to the number of pools.
#' The entries are weights, which must sum to 1.
#' @section Methods:
#' \code{\link{getMeanTransitTime,ConstLinDecompOp-method}}\cr
#' @references Manzoni, S., G.G. Katul, and A. Porporato. 2009. Analysis of
#' soil carbon transit times and age distributions using network theories.
#' Journal of Geophysical Research-Biogeosciences 114, DOI:
#' 10.1029/2009JG001070.
#' 
#' Thompson, M.~V. and Randerson, J.~T.: Impulse response functions of
#' terrestrial carbon cycle models: method and application, Global Change
#' Biology, 5, 371--394, 10.1046/j.1365-2486.1999.00235.x, 1999.
#' 
#' Bolin, B. and Rodhe, H.: A note on the concepts of age distribution and
#' transit time in natural reservoirs, Tellus, 25, 58--62, 1973.
#' 
#' Eriksson, E.: Compartment Models and Reservoir Theory, Annual Review of
#' Ecology and Systematics, 2, 67--84, 1971.
NULL





#' getReleaseFlux,Model-method get the release rate for all pools
#' 
#' The method computes the release of carbon per time for all points in time
#' specified in the Model objects time slot.
#' 
#' This function takes a Model object, which represents a system of ODEs
#' \deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t)
#' \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} solves the system for
#' \eqn{\mathbf{C}(t)}{C(t)}, calculates the release coefficients
#' \eqn{\mathbf{R}(t)}{R(t)}, and computes the release flux as
#' \eqn{\mathbf{R}(t) \mathbf{C}(t)}{R(t) C(t)}. The numerical solver used can
#' be specified in the model creating functions like e.g. \code{\link{Model}}.
#' 
#' @name getReleaseFlux,Model-method
#' @docType methods
#' @param object : of class Model, an object of class Model created by a call
#' to a constructor e.g. \code{\link{Model}}, \code{\link{GeneralModel}}or
#' other model creating functions.
#' @return A n x m matrix of release fluxes with m columns representing the
#' number of pools, and n rows representing the values for each time step as
#' specified by the argument \code{t} in \code{\link{Model}},
#' \code{\link{GeneralModel}} or another model creating function.
NULL





#' getReleaseFlux,NlModel-method get the release rate for all pools
#' 
#' The method computes the release of carbon per time for all points in time
#' specified in the objects time slot.
#' 
#' 
#' @name getReleaseFlux,NlModel-method
#' @docType methods
#' @param object : of class NlModel, an object of class NlModel
NULL





#' This function
#' 
#' no Description
#' 
#' 
#' @param object An model object (the actual class depends on the method e.g.
#' Model or Model14
#' @return A matrix. Every column represents a pool and every row a point in
#' time
#' @section Methods: \code{\link{getReleaseFlux,Model-method}}\cr
#' \code{\link{getReleaseFlux,NlModel-method}}\cr
NULL





#' getReleaseFlux14,Model_14-method 14C respiration rate for all pools
#' 
#' The function computes the \eqn{^{14}C}{14C} release flux ( mass per time )
#' for all pools. Note that the respiration coefficients for \eqn{^{14}C}{14C}
#' do not change in comparison to the total C case. The fraction of
#' \eqn{^{14}C}{14C} lost by respiration is not greater for \eqn{^{14}C}{14C}
#' although the decay is faster due to the contribution of radioactivity.
#' 
#' 
#' @name getReleaseFlux14,Model_14-method
#' @docType methods
#' @param object : of class Model_14, the model.
NULL





#' This function
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getReleaseFlux14,Model_14-method}}\cr
NULL





#' getTimeRange,DecompositionOperator-method ask for the boundaries of the
#' underlying time interval
#' 
#' The method returns the time range of the given object It is ( probably
#' mostly ) used internally to make sure that time dependent functions
#' retrieved from data are not used outside the interval where they are valid.
#' 
#' 
#' @name getTimeRange,DecompositionOperator-method
#' @docType methods
#' @param object : of class DecompositionOperator
NULL





#' getTimeRange,UnBoundInFlux-method return an (infinite) time range since the
#' function is assumed to be valid for all times
#' 
#' some functions dealing with DecompOps in general rely on this so we have to
#' implement it even though the timerange is always the same: (-inf,inf)
#' 
#' 
#' @name getTimeRange,UnBoundInFlux-method
#' @docType methods
#' @param object : of class UnBoundInFlux
NULL





#' getTimeRange,TimeMap-method ask for the boundaries of the underlying time
#' interval
#' 
#' The method returns the time range of the given object It is probably mostly
#' used internally to make sure that time dependent functions retrieved from
#' data are not used outside the interval where they are valid.
#' 
#' 
#' @name getTimeRange,TimeMap-method
#' @docType methods
#' @param object : of class TimeMap, An object of class TimeMap or one that
#' inherits from TimeMap
NULL





#' getTimeRange,ConstInFlux-method time domain of the function
#' 
#' The method returns a vector containing the start and end time where the
#' intepolation is valid. Since the class \code{\link{ConstInFlux}} represents
#' an input stream constant in time it will return -infinity,+infinity
#' 
#' 
#' @name getTimeRange,ConstInFlux-method
#' @docType methods
#' @param object : of class ConstInFlux
NULL





#' getTimeRange,UnBoundLinDecompOp-method return an (infinite) time range since
#' the function is assumed to be valid for all times
#' 
#' some functions dealing with DecompOps in general rely on this so we have to
#' implement it even though the timerange is always the same: (-inf,inf)
#' 
#' 
#' @name getTimeRange,UnBoundLinDecompOp-method
#' @docType methods
#' @param object : of class UnBoundLinDecompOp
NULL





#' getTimeRange,ConstLinDecompOp-method return an (infinite) time range since
#' the operator is constant
#' 
#' some functions dealing with DecompOps in general rely on this so we have to
#' implement it even though the timerange is always the same: (-inf,inf)
#' 
#' 
#' @name getTimeRange,ConstLinDecompOp-method
#' @docType methods
#' @param object : of class ConstLinDecompOp
NULL





#' getTimeRange S4 generic
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getTimeRange,ConstInFlux-method}}\cr
#' \code{\link{getTimeRange,ConstLinDecompOp-method}}\cr
#' \code{\link{getTimeRange,DecompositionOperator-method}}\cr
#' \code{\link{getTimeRange,TimeMap-method}}\cr
#' \code{\link{getTimeRange,UnBoundInFlux-method}}\cr
#' \code{\link{getTimeRange,UnBoundLinDecompOp-method}}\cr
NULL





#' getTimes,Model-method
#' 
#' This functions extracts the times argument from an argument of class Model
#' 
#' 
#' @name getTimes,Model-method
#' @docType methods
#' @param object : of class Model
NULL





#' getTimes,NlModel-method
#' 
#' This functions extracts the times argument from an object of class NlModel
#' 
#' 
#' @name getTimes,NlModel-method
#' @docType methods
#' @param object : of class NlModel
NULL





#' getTimes S4 generic
#' 
#' no Description
#' 
#' 
#' @param object see the method arguments for details
#' @section Methods: \code{\link{getTimes,Model-method}}\cr
#' \code{\link{getTimes,NlModel-method}}\cr
NULL





#' getTransitTimeDistributionDensity,ConstLinDecompOp-method compute the
#' TransitTimeDistributionDensity
#' 
#' This mehtod computes the probability density of the transit time of the
#' linear time invariant system that can be constructed from the given operator
#' and input distribution.
#' 
#' In a forthcoming paper \cite{SoilRv1.2} we derive the algorithm used in this
#' implementation under the assumption of steady conditions having prevailed
#' infinitely. We arrive at a formulation well known from the literature about
#' time invariant linlear systems, cited e.g. in \cite{ManzoniJGR}.\cr The
#' somehow amazing result is that the weight of the transit time density
#' \eqn{\psi(T)}{\psi(T)} for a \emph{transit time} \eqn{T}{T} for the steady
#' state system is identical to the output \eqn{O(T)}{O(T)} observed at time
#' \eqn{T}{T} of a \emph{different} system which started with a normalized
#' impulsive input \eqn{\frac{\vec{I}}{I}}{\frac{\vec{I}}{I}} at time
#' \eqn{T=0}{T=0}, where \eqn{I=\sum_{k=1}^m i_k} is the cumulative input flux
#' to all pools. \cr This fact simpliefies the computation considerably.
#' Translated into the language of an ode solver an impulsive input becomes a
#' start vector \eqn{\frac{\vec{I}}{I}}{\frac{\vec{I}}{I}} at time
#' \eqn{T=0}{T=0} and \eqn{O(T)}{O(T)} the respiration related to the solution
#' of the initial value problem observed at time \eqn{T}{T}.  \deqn{
#' }{}\deqn{\psi(T)=S_r \left( \frac{\vec{I}}{I},0,T\right) }{} Note that from
#' the perspective of the ode solver \eqn{S_r}{S_r} depends on the
#' decomposition operator and the distribution of the input among the pools
#' only. It is therefor possible to implement the transit time distribution as
#' a function of the decomposition operator and the fixed input flux
#' distribution. To insure steady state conditions the decomposition operator
#' is not allowed to be a true function of time. We therefor implement the
#' method only for the subclass \code{ConstLinDecompOp} \cr Remark:\cr The
#' decision to implement this method for \code{transitTimeDensity} especially
#' for objects of class \code{ConstLinDecompOp} reflects the fact that an
#' arbitrary model in SoilR is by no means bound to end up in steady state. To
#' insure this we would have to ignore the input part of a user created model
#' which would be confusing.  \cr Remark:\cr In future versions of SoilR it
#' will be possible to compute a dynamic, time dependent transit time
#' distribution for objects of class \code{ Model} with a time argument
#' specifying for which time the distribution is sought.  The steady state
#' results computed here could than be reproduced with the user responsible for
#' providing a model actually reaching it.
#' 
#' @name getTransitTimeDistributionDensity,ConstLinDecompOp-method
#' @docType methods
#' @param object : of class ConstLinDecompOp
#' @param inputDistribution : distribution of the inputs
#' @param times : the points in time where the solution is sought
#' @references Manzoni, S., Katul, G.~G., and Porporato, A.: Analysis of soil
#' carbon transit times and age distributions using network theories, J.
#' Geophys. Res., 114,
NULL





#' This function
#' 
#' no Description
#' 
#' 
#' @param object a protoDecompOp Object
#' @param inputDistribution a vector of length equal to the number of pools.
#' The entries are weights. That means that their sume must be equal to one!
#' @param times the times for which the distribution density is sought
#' @section Methods:
#' \code{\link{getTransitTimeDistributionDensity,ConstLinDecompOp-method}}\cr
#' @references Manzoni, S., G.G. Katul, and A. Porporato. 2009. Analysis of
#' soil carbon transit times and age distributions using network theories.
#' Journal of Geophysical Research-Biogeosciences 114, DOI:
#' 10.1029/2009JG001070.
NULL





#' Delta14C in soil CO2 efflux from Harvard Forest
#' 
#' Measurements of Delta14C in soil CO2 efflux conducted at Harvard Forest,
#' USA, between 1996 and 2010.
#' 
#' Samples for isotopic measurements of soil CO2 efflux were collected from
#' chambers that enclosed an air headspace in contact with the soil surface in
#' the absence of vegetation using a closed dynamic chamber system to collect
#' accumulated CO2 in stainless steel traps with a molecular sieve inside. See
#' Sierra et al. (2012) for additional details.
#' 
#' @name HarvardForest14CO2
#' @docType data
#' @format A data frame with the following 3 variables.  \describe{
#' \item{list("Year")}{A numeric vector with the date of measurement in years}
#' \item{list("D14C")}{A numeric vector with the value of the Delta 14C value
#' measured in CO2 efflux in per mil} \item{list("Site")}{A factor indicating
#' the site where measurements were made. NWN: Northwest Near, Drydown:
#' Rainfall exclusion experiment.} }
#' @references Sierra, C. A., Trumbore, S. E., Davidson, E. A., Frey, S. D.,
#' Savage, K. E., and Hopkins, F. M. 2012. Predicting decadal trends and
#' transient responses of radiocarbon storage and fluxes in a temperate forest
#' soil, Biogeosciences, 9, 3013-3028, doi:10.5194/bg-9-3013-2012
#' @keywords datasets
#' @examples
#' 
#' plot(HarvardForest14CO2[,1:2])
#' 
NULL





#' Atmospheric radiocarbon for the period 1950-2010 from Hua et al. (2013)
#' 
#' Atmospheric radiocarbon for the period 1950-2010 reported by Hua et al.
#' (2013) for 5 atmospheric zones.
#' 
#' This dataset corresponds to Table S3 from Hua et al. (2013). For additional
#' details see the original publication.
#' 
#' @name Hua2013
#' @docType data
#' @format A \link{list} containing 5 data frames, each representing an
#' atmospheric zone. The zones are: NHZone1: northern hemisphere zone 1,
#' NHZone2: northern hemisphere zone 2, NHZone3: northern hemisphere zone 3,
#' SHZone12: southern hemisphere zones 1 and 2, SHZone3: southern hemisphere
#' zone 3. Each data frame contains a variable number of observations on the
#' following 5 variables.  \describe{ \item{list("Year.AD")}{Year AD}
#' \item{list("mean.Delta14C")}{mean value of atmospheric radiocarbon reported
#' as Delta14C} \item{list("sd.Delta14C")}{standard deviation of atmospheric
#' radiocarbon reported as Delta14C} \item{list("mean.F14C")}{mean value of
#' atmospheric radiocarbon reported as fraction modern F14C}
#' \item{list("sd.F14")}{standard deviation of atmospheric radiocarbon reported
#' as fraction modern F14C} }
#' @references Hua Q., M. Barbetti, A. Z. Rakowski. 2013. Atmospheric
#' radiocarbon for the period 1950-2010. Radiocarbon 55(4):2059-2072.
#' @source
#' \url{https://journals.uair.arizona.edu/index.php/radiocarbon/article/view/16177}
#' @keywords datasets
#' @examples
#' 
#' 
#' plot(Hua2013$NHZone1$Year.AD, Hua2013$NHZone1$mean.Delta14C, 
#'      type="l",xlab="Year AD",ylab=expression(paste(Delta^14,"C (\u2030)")))
#' lines(Hua2013$NHZone2$Year.AD,Hua2013$NHZone2$mean.Delta14C,col=2)
#' lines(Hua2013$NHZone3$Year.AD,Hua2013$NHZone3$mean.Delta14C,col=3)
#' lines(Hua2013$SHZone12$Year.AD,Hua2013$SHZone12$mean.Delta14C,col=4)
#' lines(Hua2013$SHZone3$Year.AD,Hua2013$SHZone3$mean.Delta14C,col=5)
#' legend(
#' 	"topright",
#' 	c(
#' 		"Norther hemisphere zone 1",
#' 		"Norther hemisphere zone 2",
#' 		"Norther hemisphere zone 3",
#'                 "Southern hemisphere zones 1 and 2",
#' 		"Southern Hemispher zone 3"
#' 	),
#' 	lty=1,
#' 	col=1:5,
#' 	bty="n"
#' )
#' 
#' 
NULL





#' InFlux S4 class
#' 
#' All models need to specify the influx of material to the pools. This
#' parameter will be represented as an object of one of the subclasses of this
#' class. The most general form of influx supported up to now is a vector
#' valued function of time represented by \code{\link{BoundInFlux-class}}. In
#' the most simple case it is constant and represented by an object of class
#' \code{\link{ConstInFlux-class}}. Such an object can for instance be created
#' from a numeric vector.
#' 
#' 
#' @name InFlux-class
#' @docType class
#' @section Methods: Exported methods directly defined for class InFlux:
#' 
#' \describe{ \item{GeneralInFlux}{\code{signature(object = "InFlux")}: ... }
#' \code{\link{GeneralInFlux,InFlux-method}} }
NULL





#' Northern Hemisphere atmospheric radiocarbon for the pre-bomb period
#' 
#' Northern Hemisphere atmospheric radiocarbon calibration curve for the period
#' 0 to 50,000 yr BP.
#' 
#' \code{Deltal.14C} is age-corrected as per Stuiver and Polach (1977). All
#' details about the derivation of this dataset are provided in Reimer et al.
#' (2009).
#' 
#' @name IntCal09
#' @docType data
#' @format A data frame with 3522 observations on the following 5 variables.
#' \describe{ \item{list("CAL.BP")}{Calibrated age in years Before Present
#' (BP).} \item{list("C14.age")}{C14 age in years BP.}
#' \item{list("Error")}{Error estimate for \code{C14.age}.}
#' \item{list("Delta.14C")}{Delta.14C value in per mil.}
#' \item{list("Sigma")}{Standard deviation of \code{Delta.14C} in per mil.} }
#' @references P. Reimer, M.Baillie, E. Bard, A. Bayliss, J. Beck, P.
#' Blackwell, C. Ramsey, C. Buck, G. Burr, R. Edwards, et al. 2009. IntCal09
#' and Marine09 radiocarbon age calibration curves, 0 - 50,000 years cal bp.
#' Radiocarbon, 51(4):1111 - 1150.
#' 
#' M. Stuiver and H. A. Polach. 1977. Rerporting of C-14 data. Radiocarbon,
#' 19(3):355 - 363.
#' @source \url{ http://www.radiocarbon.org/IntCal09%20files/intcal09.14c }
#' @keywords datasets
#' @examples
#' 
#' 
#' par(mfrow=c(2,1))
#' plot(IntCal09$CAL.BP, IntCal09$C14.age, type="l")
#' polygon(x=c(IntCal09$CAL.BP,rev(IntCal09$CAL.BP)),
#' 	y=c(IntCal09$C14.age+IntCal09$Error,rev(IntCal09$C14.age-IntCal09$Error)),
#' 	col="gray",border=NA)
#' lines(IntCal09$CAL.BP,IntCal09$C14.age)
#' 
#' plot(IntCal09$CAL.BP,IntCal09$Delta.14C,type="l")
#' polygon(x=c(IntCal09$CAL.BP,rev(IntCal09$CAL.BP)),
#' 	y=c(IntCal09$Delta.14C+IntCal09$Sigma,rev(IntCal09$Delta.14C-IntCal09$Sigma)),
#' 	col="gray",border=NA)
#' lines(IntCal09$CAL.BP,IntCal09$Delta.14C)
#' par(mfrow=c(1,1))
#' 
NULL





#' Atmospheric radiocarbon for the 0-50,000 yr BP period
#' 
#' Atmospheric radiocarbon calibration curve for the period 0 to 50,000 yr BP.
#' This is the most recent update to the internationally agreed calibration
#' curve and superseds \code{\link{IntCal09}}.
#' 
#' \code{Deltal.14C} is age-corrected as per Stuiver and Polach (1977). All
#' details about the derivation of this dataset are provided in Reimer et al.
#' (2013).
#' 
#' @name IntCal13
#' @docType data
#' @format A data frame with 5140 observations on the following 5 variables.
#' \describe{ \item{list("CAL.BP")}{Calibrated age in years Before Present
#' (BP).} \item{list("C14.age")}{C14 age in years BP.}
#' \item{list("Error")}{Error estimate for \code{C14.age}.}
#' \item{list("Delta.14C")}{Delta.14C value in per mil.}
#' \item{list("Sigma")}{Standard deviation of \code{Delta.14C} in per mil.} }
#' @references Reimer PJ, Bard E, Bayliss A, Beck JW, Blackwell PG, Bronk
#' Ramsey C, Buck CE, Cheng H, Edwards RL, Friedrich M, Grootes PM, Guilderson
#' TP, Haflidason H, Hajdas I, Hatte C, Heaton TJ, Hogg AG, Hughen KA, Kaiser
#' KF, Kromer B, Manning SW, Niu M, Reimer RW, Richards DA, Scott EM, Southon
#' JR, Turney CSM, van der Plicht J. 2013. IntCal13 and MARINE13 radiocarbon
#' age calibration curves 0-50000 years calBP. Radiocarbon 55(4): 1869-1887.
#' DOI: 10.2458/azu_js_rc.55.16947
#' 
#' M. Stuiver and H. A. Polach. 1977. Rerporting of C-14 data. Radiocarbon,
#' 19(3):355 - 363.
#' @source \url{ http://www.radiocarbon.org/IntCal13%20files/intcal13.14c }
#' @keywords datasets
#' @examples
#' 
#' 
#' 
#'      plot(IntCal13$CAL.BP,IntCal13$C14.age-IntCal13$Error,type="l",col=2,
#'           xlab="cal BP",ylab="14C BP")
#'      lines(IntCal13$CAL.BP,IntCal13$C14.age+IntCal13$Error,col=2)
#' 
#'      plot(IntCal13$CAL.BP,IntCal13$Delta.14C+IntCal13$Sigma,type="l",col=2,
#'           xlab="cal BP",ylab="Delta14C")
#'      lines(IntCal13$CAL.BP,IntCal13$Delta.14C-IntCal13$Sigma,col=2)
#' 
#' 
NULL





#' Model_14
#' 
#' This class extends \code{\linkS4class{Model}}, to represent
#' \eqn{^{14}C}{14C} decay.  \enumerate{ \item \itemize{ \item As
#' \code{\linkS4class{Model}} it contains all the components that are needed to
#' solve the initial value problem for the pool contents
#' \eqn{\vec{C}}{C=(C_1,...C_n)^t}.  \item It adds the components that are
#' needed to solve the additional initial value problem for the
#' \eqn{^{14}C}{14C} contents of the pools
#' \eqn{\vec{^{14}C}}{14C=(14C_1,...,14C_n)^t}.  } \item \itemize{ \item It
#' provides the single argument for all the functions that are available for an
#' argument of class \code{\linkS4class{Model}}. \item and for additional
#' functions that are available to compute various results from the solution of
#' the additional initial value problem for \eqn{^{14}C}{14C}.  See subsection
#' \code{Methods} and the examples.) } }
#' 
#' The original initial value problem for \eqn{\vec{C}}{(C_1,...,C_n)^t)} as
#' decribed in the docomentation of the superclass \code{\linkS4class{Model}}
#' was given by: \itemize{ \item \eqn{ \frac{d \mathbf{C}(t)}{dt} =
#' \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t) }
#' \item the initial Values \eqn{\vec{C}_0=\vec{C}(t_0)}{C_0=C(t_0)} \item for
#' the times \eqn{\{t_0,....t_m\}}{{t_0,....,t_m}}. } The additional initial
#' value problem for \eqn{^{14}C}{14C} is represented by additional parameters:
#' \itemize{ \item a second ordinary differential equation: \deqn{\frac{d ^{14}
#' \mathbf{C}(t)}{dt} = F(t) \mathbf{I}(t) + \mathbf{A}(t) ^{14}
#' \mathbf{C}(t)-k_{14}\; ^{14}\mathbf{C}(t) }{d 14C/dt = F(t) I(t) +
#' A(t)C(t)-k_14 C(t)} with initial values \eqn{^{14}\mathbf{C}_0=F_0
#' \mathbf{C}_0}{14C_0=F_0 C_0} with: \item the time dependent
#' \eqn{^{14}C}{14C} fraction \eqn{F(t)}{F(t)}, \item the constant
#' \eqn{^{14}C}{14C} fraction of the initial pool contents \eqn{F_0}{F_0},
#' \item the \eqn{^{14}C}{14C} decay rate \eqn{k_{14}}{k_14}. } In an object of
#' class \code{\linkS4class{Model_14}} the components are represented as
#' follows: \itemize{ \item The time-dependent matrix valued function
#' \eqn{\vec{A}(t)}{A(t)} is represented by an object of a subclass of
#' \code{\linkS4class{DecompOp}} (for decomposition operator).  Such objects
#' can be created in different ways from functions, matrices or data.  (see the
#' subclasses of \code{\linkS4class{DecompOp}} and especially their
#' \code{Constructors} sections.  and the \code{examples} section of this help
#' page. \item The vector-valued time-dependent function \eqn{\vec{I}(t)}{I(t)}
#' is in SoilR represented by an object of a subclass of class
#' \code{\linkS4class{InFlux}}.  Such objects can also be created from
#' functions, constant vectors and data.  (see the subclasses of
#' \code{\linkS4class{InFlux}} and especially their \code{Constructors}
#' sections.  \item The initial values for \eqn{\mathbf{C}_0}{C} are
#' represented by a numeric vector \item The value for the
#' \eqn{^{14}\mathbf{C}}{14C} fraction of the initial \eqn{\mathbf{C}}{C} is
#' represented as an object of class \linkS4class{ConstFc} which is a subclass
#' of \code{\linkS4class{Fc}} representing the \eqn{^{14}C}{14C} fraction
#' \eqn{F}{F} and its unit.  (Either "Delta14C" or "afn" for Absolute Fraction
#' Normal) \item The value for the \eqn{^{14}\mathbf{C}}{14C} fraction of the
#' input \eqn{\mathbf{I}(t)}{I(t)} is also represented as an object of a
#' subclass of \code{\linkS4class{Fc}}. It can be time dependent or constant. }
#' 
#' @name Model_14-class
#' @docType class
#' @section Methods: Exported methods directly defined for class Model_14:
#' 
#' \describe{ \item{getC14}{\code{signature(object = "Model_14")}: ... }
#' \code{\link{getC14,Model_14-method}} \item{getF14}{\code{signature(object =
#' "Model_14")}: ... } \code{\link{getF14,Model_14-method}}
#' \item{getF14C}{\code{signature(object = "Model_14")}: ... }
#' \code{\link{getF14C,Model_14-method}} \item{getF14R}{\code{signature(object
#' = "Model_14")}: ... } \code{\link{getF14R,Model_14-method}}
#' \item{getReleaseFlux14}{\code{signature(object = "Model_14")}: ... }
#' \code{\link{getReleaseFlux14,Model_14-method}} }
#' 
#' Methods inherited from superclasses:
#' 
#' from class Model:
#' 
#' \describe{ \item{[}{\code{signature(x = "Model", i = "character", j =
#' "missing", drop = "missing")}: ... }
#' \code{\link{[,Model,character,missing,missing-method}}
#' \item{getAccumulatedRelease}{\code{signature(object = "Model")}: ... }
#' \code{\link{getAccumulatedRelease,Model-method}}
#' \item{getC}{\code{signature(object = "Model")}: ... }
#' \code{\link{getC,Model-method}} \item{getDecompOp}{\code{signature(object =
#' "Model")}: ... } \code{\link{getDecompOp,Model-method}}
#' \item{getReleaseFlux}{\code{signature(object = "Model")}: ... }
#' \code{\link{getReleaseFlux,Model-method}}
#' \item{getTimes}{\code{signature(object = "Model")}: ... }
#' \code{\link{getTimes,Model-method}} }
NULL





#' Model
#' 
#' The class Model is the focal point of SoilR.  \enumerate{ \item It combines
#' all the components that are needed to solve the initial value problem for
#' the pool contents. \eqn{\vec{C}}{C=(C_1,...C_n)^t, }.  \item It provides the
#' single argument for the different functions that are available to compute
#' various results from the solution of the initial value problem.  See
#' subsection \code{Methods} and the examples.) }
#' 
#' The initial value problem is given by: \itemize{ \item the ordinary
#' differential equation \eqn{ \dot{\vec{C}} = \mathbf{A}(t) \vec{C}
#' =\vec{I}(t)}{ d/dt C=A(t)C+I(t),} \item the intial Values
#' \eqn{\vec{C}_0=\vec{C}(t_0)}{C_0=C(t_0),} \item for the times
#' \eqn{\{t_0,....t_m\}}{{t_0,....,t_m}}. } In an object of class Model the
#' components are represented as follows: \itemize{ \item The time-dependent
#' matrix valued function \eqn{\vec{A}(t)}{A(t)} is represented by an object of
#' a class that inherits from class \code{\linkS4class{DecompOp}}.  Such
#' objects can be created in different ways from functions, matrices or data.
#' (see the subclasses of \code{\linkS4class{DecompOp}} and especially their
#' \code{Constructors} sections.  and the \code{examples} section of this help
#' page. \item The vector-valued time-dependent function \eqn{\vec{I}(t)}{I(t)}
#' is in SoilR represented by an object of a class that inherits from class
#' InFlux \code{\linkS4class{InFlux}}.  Such objects can be created from
#' functions, constant vectors and data.  (see the subclasses of
#' \code{\linkS4class{InFlux}} and especially their \code{Constructors}
#' sections.  \item The times for which the results are computed are
#' represented by a numeric vector. \item The initial values are represented by
#' a numeric vector }
#' 
#' @name Model-class
#' @docType class
#' @section Methods: Exported methods directly defined for class Model:
#' 
#' \describe{ \item{[}{\code{signature(x = "Model", i = "character", j =
#' "missing", drop = "missing")}: ... }
#' \code{\link{[,Model,character,missing,missing-method}}
#' \item{getAccumulatedRelease}{\code{signature(object = "Model")}: ... }
#' \code{\link{getAccumulatedRelease,Model-method}}
#' \item{getC}{\code{signature(object = "Model")}: ... }
#' \code{\link{getC,Model-method}} \item{getDecompOp}{\code{signature(object =
#' "Model")}: ... } \code{\link{getDecompOp,Model-method}}
#' \item{getReleaseFlux}{\code{signature(object = "Model")}: ... }
#' \code{\link{getReleaseFlux,Model-method}}
#' \item{getTimes}{\code{signature(object = "Model")}: ... }
#' \code{\link{getTimes,Model-method}} }
#' @examples
#' # examples from external files
#' # inst/examples/ModelExamples.R CorrectNonautonomousLinearModelExplicit:
#' 
#'   # This example describes the creation and use of a Model object that 
#'   # is defined by time dependent functions for decomposition and influx.
#'   # The constructor of the Model-class  (see  ?Model) 
#'   # works for different combinations of 
#'   # arguments.
#'   # Although Model (the constructor function for objects of this class 
#'   # accepts many many more convienient kinds of arguments,
#'   # we will in this example call the constructor whith arguments which 
#'   # are of the same type as one of hte current internal 
#'   # representations in the 
#'   # Model object and create these arguments explicitly beforehand 
#'   # to demonstrate the approach with the most flexibility.
#'   # We start with the Decomposition Operator.
#'   # For this example we assume that we are able to describe the
#'   # decomposition ofperator  by explicit R functions that are valid 
#'   # for a finite time interval.
#'   # Therefore we choose the appropriate  sub class BoundLinDecompOp
#'   # of DecompOp explicitly.  (see ?'BoundLinDecompOp-class') 
#'   A=BoundLinDecompOp(
#'     ## We call the generic constructor (see ?BoundLindDcompOp) 
#'     ## which has a method  
#'     ## that takes a matrix-valued function of time as its first argument.
#'     ## (Although Model accepts time series data directly and 
#'     ## will derive the internally used interpolating for you, 
#'     ## the function argument could for instance represent the result
#'     ## of a very sophisticated interpolation performed by yourself)
#'     function(t){
#'       matrix(nrow=3,ncol=3,byrow=TRUE,
#'          c(
#'            -1,    0,        0,
#'           0.5,   -2,        0,
#'             0,    1, sin(t)-1 
#'         )
#'       )    
#'     },
#'     ## The other two arguments describe the time interval where the 
#'     ## function is valid (the domain of the function)
#'     ## The interval will be checked against the domain of the InFlux
#'     ## argument of Model and against its 't' argument to avoid 
#'     ## invalid computations outside the domain. 
#'     ## (Inf and -Inf are possible values, but should only be used 
#'     ## if the function is really valid for all times, which is 
#'     ## especially untrue for functions resulting from interpolations,
#'     ## which are usually extremely misleading for arguments outside the 
#'     ## domain covered by the data that has been used for the interpolation.)
#'     ## This is a safety net against wrong results origination from unitendet EXTRApolation )
#'     starttime=0,
#'     endtime=20
#'   )  
#'   I=BoundInFlux(
#'      ## The first argument is a vector-valued function of time
#'      function(t){
#'        matrix(nrow=3,ncol=1,byrow=TRUE,
#'            c(-1,    0,    0)
#'        )
#'      },
#'      ## The other two arguments describe the time interval where the 
#'      ## function is valid (the domain of the function)
#'      starttime=0,
#'      endtime=40
#'   )
#'   ## No we specify the points in time where we want 
#'   ## to compute results
#'   t_start=0 
#'   t_end=10 
#'   tn=50
#'   timestep <- (t_end-t_start)/tn 
#'   times <- seq(t_start,t_end,timestep) 
#'   ## and the start values
#'   sv=c(0,0,0)
#'   mod=Model(t=times,A,sv,I)
#' 
#'   ## No we use the model to compute some results
#'   getC(mod)
#'   getReleaseFlux(mod)
#'   #also look at the methods section of Model-class 
#' 
NULL





#' NlModel
#' 
#' serves as a fence to the interface of SoilR functions. So that later
#' implementations can differ
#' 
#' 
#' @name NlModel-class
#' @docType class
#' @section Methods: Exported methods directly defined for class NlModel:
#' 
#' \describe{ \item{$}{\code{signature(x = "NlModel")}: ... }
#' \code{\link{$,NlModel-method}} \item{[}{\code{signature(x = "NlModel", i =
#' "character", j = "ANY", drop = "ANY")}: ... }
#' \code{\link{[,NlModel,character,ANY,ANY-method}}
#' \item{getC}{\code{signature(object = "NlModel")}: ... }
#' \code{\link{getC,NlModel-method}} \item{getDecompOp}{\code{signature(object
#' = "NlModel")}: ... } \code{\link{getDecompOp,NlModel-method}}
#' \item{getReleaseFlux}{\code{signature(object = "NlModel")}: ... }
#' \code{\link{getReleaseFlux,NlModel-method}}
#' \item{getTimes}{\code{signature(object = "NlModel")}: ... }
#' \code{\link{getTimes,NlModel-method}} }
NULL





#' SOILR
#' 
#' The package allows you to study compartmental Soil models.
#' 
#' The typical workflow consists of the following steps: \itemize{ \itemCreate
#' a model \itemInspect it } The simplest way of creating a model is to use one
#' of the top level functions for predefined models:
#' \code{\link{predefinedModels}}. \cr The objects returned by these functions
#' can be of different type, usually either \code{Model} or \code{Model14}.  To
#' inspect the behaviour of a model object these classes provide several
#' methods to be found in their respective descriptions: (
#' \code{\linkS4class{Model}} or \code{\linkS4class{Model_14}} )\cr If none of
#' the predefined models fits your needs you can assemble your own model. The
#' functions that create it are the constructors of the classes
#' \linkS4class{Model} or \linkS4class{Model_14}. By convention they have the
#' same name as the class and are desribed here: \cr \code{\link{Model}},\cr
#' \code{\link{Model_14}}.
#' 
#' @name SoilR-package
#' @aliases SoilR SoilR-package
#' @docType package
NULL





#' TimeMap S4 class
#' 
#' This class enhances a time dependent function by information about its
#' domain. The information about the delay is especially usefull for functions
#' that interpolate data.  Assume that you are given time series data in two
#' vectors \code{times}, \code{values}. You can create an interpolating
#' function with \code{\link{splinefun}} or \code{\link{approxfun}} \code{f <-
#' splinefun(x=times,y=values) } \code{f(t)} will yield sensible values for
#' \eqn{\min_{t \in times}\le t \le max_{t \in
#' times}.}{min(times)<t<max(times).} but will produce unreasonable values for
#' any t outside these limits. Unfortunately the interpolating functions
#' produced by \code{\link{splinefun}} or \code{\link{approxfun}} do not retain
#' any information about their domain which makes it possible to accidentatly
#' apply them to times not at all supported by the original data.  This would
#' not even cause errors in the code but silently corrupt the results. To help
#' you to keep track of the domains of the many time dependent functions used
#' in SoilR's Models this class \code{\linkS4class{TimeMap}} stores the
#' \code{starttime} and \code{endtime} values along with the function
#' represented by \code{map}. SoilR functions that accept time series data will
#' normally convert it to subclasses \code{TimeMap-class} automatically but you
#' can do it explicitly.
#' 
#' 
#' @name TimeMap-class
#' @docType class
#' @section Methods: Exported methods directly defined for class TimeMap:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ...
#' } \code{\link{GeneralDecompOp,TimeMap-method}}
#' \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{GeneralInFlux,TimeMap-method}}
#' \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime =
#' "ANY", times = "ANY", data = "ANY")}: ... }
#' \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
#' \item{add_plot}{\code{signature(x = "TimeMap")}: ... }
#' \code{\link{add_plot,TimeMap-method}} \item{as.character}{\code{signature(x
#' = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
#' \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getFunctionDefinition,TimeMap-method}}
#' \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getTimeRange,TimeMap-method}} }
NULL





#' TimeMap,data.frame,missing,missing,missing,missing-method constructor
#' 
#' create a TimeMap object by interpolating the data.frame
#' 
#' 
#' @name TimeMap,data.frame,missing,missing,missing,missing-method
#' @docType methods
#' @param map : of class data.frame, a data frame containing two columns
#' @param lag : a time delay
#' @param interpolation : the interpolating function
NULL





#' TimeMap,TimeMap,ANY,ANY,ANY,ANY-method pass through constructor
#' 
#' The function just returns its argument. So any function that has to convert
#' one of its argument can just call TimeMap on it even if the argument is
#' allready one.
#' 
#' 
#' @name TimeMap,TimeMap,ANY,ANY,ANY,ANY-method
#' @docType methods
#' @param map : of class TimeMap, the object that will be returned unchanged
NULL





#' TimeMap,missing,missing,missing,numeric,matrix-method Create a TimeMap from
#' a nested list
#' 
#' The method creates an instance of \code{\link{TimeMap-class}} from a vector
#' of times and an array referring to it.
#' 
#' 
#' @name TimeMap,missing,missing,missing,numeric,matrix-method
#' @docType methods
#' @param times : of class numeric
#' @param data : of class matrix, a matrix, every column corresponds to one
#' time step
#' @param lag : a scalar or a vector describing how much the data is laging
#' behind the times
#' @param interpolation : the interpolation method to be used
NULL





#' TimeMap,missing,missing,missing,numeric,list-method Create a TimeMap from a
#' nested list
#' 
#' The method creates an instance of \code{\link{TimeMap-class}} from a vector
#' of times and a list of the same length, containing vectors matrices or
#' arrays
#' 
#' 
#' @name TimeMap,missing,missing,missing,numeric,list-method
#' @docType methods
#' @param times : of class numeric, A vector of times
#' @param data : of class list, A list of the same length as times with every
#' list element refering to one time in times
#' @param lag : Either a scalar or an object of the same size as the list
#' elements describing the time lag of the data
#' @param interpolation : the interpolation method to be used
NULL





#' TimeMap,function,numeric,numeric,missing,missing-method constructor
#' 
#' create a TimeMap object from the function definition and the time interval
#' 
#' 
#' @name TimeMap,function,numeric,numeric,missing,missing-method
#' @docType methods
#' @param map : of class function
#' @param starttime : of class numeric
#' @param endtime : of class numeric
NULL





#' TimeMap,list,missing,missing,missing,missing-method Create a TimeMap from a
#' nested list
#' 
#' The method creates an instance of \code{\link{TimeMap-class}} from a list
#' that contains data and a vector of times referring to it.
#' 
#' The list must have two entries If the entries are not named, the first one
#' is supposed to be a numeric vector of \code{times} and the second to contain
#' the data referring to those times. The \code{data} entry of the list can
#' itself be a list with the same length as the \code{times} entry or an array
#' whose last dimension is equal to the length of the \code{times} entry. If
#' the \code{data} entry is a list the elements must be
#' \code{vectors},\code{matrices} or \code{arrays}.
#' 
#' @name TimeMap,list,missing,missing,missing,missing-method
#' @docType methods
#' @param map : of class list
#' @param lag : either a scalar or an element of the same shape as the elements
#' of the data entry that refer to one time step
#' @param interpolation : the interpolation method to be used
NULL





#' TimeMap,missing,missing,missing,numeric,numeric-method Create a TimeMap from
#' a nested list
#' 
#' The method creates an instance of \code{\link{TimeMap-class}} from a vector
#' of times and an array referring to it.
#' 
#' 
#' @name TimeMap,missing,missing,missing,numeric,numeric-method
#' @docType methods
#' @param times : of class numeric
#' @param data : of class numeric
#' @param lag : a scalar or a vector,matrix or array describing how much the
#' data lag behind the times. If lag is a vector, matrix or array, the
#' resulting ## function will be vector, matrix or array valued accordingly.
#' @param interpolation : the interpolation method to be used
NULL





#' TimeMap,missing,missing,missing,numeric,array-method Create a TimeMap from
#' times and array
#' 
#' The method creates an instance of \code{\link{TimeMap-class}} from a vector
#' of times and an array whose last dimension ### is referring to it.
#' 
#' 
#' @name TimeMap,missing,missing,missing,numeric,array-method
#' @docType methods
#' @param times : of class numeric
#' @param data : of class array
#' @param lag : Either a scalar or an object of the same size as a slice of the
#' array per timestep. It describes the time lag of the array elements
#' @param interpolation : the interpolation method to be used
NULL





#' TimeMap S4 generic
#' 
#' no Description
#' 
#' 
#' @param map see the method arguments for details
#' @param starttime see the method arguments for details
#' @param endtime see the method arguments for details
#' @param times see the method arguments for details
#' @param data see the method arguments for details
#' @param lag a time delay
#' @param interpolation the interpolating function
#' @section Methods: \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}\cr
#' \code{\link{TimeMap,data.frame,missing,missing,missing,missing-method}}\cr
#' \code{\link{TimeMap,function,numeric,numeric,missing,missing-method}}\cr
#' \code{\link{TimeMap,list,missing,missing,missing,missing-method}}\cr
#' \code{\link{TimeMap,missing,missing,missing,numeric,array-method}}\cr
#' \code{\link{TimeMap,missing,missing,missing,numeric,list-method}}\cr
#' \code{\link{TimeMap,missing,missing,missing,numeric,matrix-method}}\cr
#' \code{\link{TimeMap,missing,missing,missing,numeric,numeric-method}}\cr
NULL





#' PREDEFINED MODELS
#' 
#' \tabular{l}{ \code{\link{GaudinskiModel14}}\cr \code{\link{ICBMModel}}\cr
#' \code{\link{OnepModel}}\cr \code{\link{OnepModel14}}\cr
#' \code{\link{RothCModel}}\cr \code{\link{ThreepFeedbackModel}}\cr
#' \code{\link{ThreepFeedbackModel14}}\cr \code{\link{ThreepParallelModel}}\cr
#' \code{\link{ThreepParallelModel14}}\cr \code{\link{ThreepSeriesModel}}\cr
#' \code{\link{ThreepSeriesModel14}}\cr \code{\link{TwopFeedbackModel}}\cr
#' \code{\link{TwopFeedbackModel14}}\cr \code{\link{TwopParallelModel}}\cr
#' \code{\link{TwopParallelModel14}}\cr \code{\link{TwopMMmodel}}\cr
#' \code{\link{ThreepairMMmodel}}\cr \code{\link{TwopSeriesModel}}\cr
#' \code{\link{TwopSeriesModel14}}\cr \code{\link{YassoModel}}\cr
#' \code{\link{bacwaveModel}}\cr \code{\link{Yasso07Model}}\cr
#' \code{\link{SeriesLinearModel}}\cr \code{\link{SeriesLinearModel14}}\cr
#' \code{\link{CenturyModel}}\cr }
#' 
#' 
NULL





#' TransportDecompositionOperator S4 class
#' 
#' no Description
#' 
#' 
#' @name TransportDecompositionOperator-class
#' @docType class
#' @section Methods: Exported methods directly defined for class
#' TransportDecompositionOperator:
#' 
#' \describe{ \item{getFunctionDefinition}{\code{signature(object =
#' "TransportDecompositionOperator")}: ... }
#' \code{\link{getFunctionDefinition,TransportDecompositionOperator-method}} }
#' 
#' Methods inherited from superclasses:
#' 
#' from class TimeMap:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ...
#' } \code{\link{GeneralDecompOp,TimeMap-method}}
#' \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{GeneralInFlux,TimeMap-method}}
#' \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime =
#' "ANY", times = "ANY", data = "ANY")}: ... }
#' \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
#' \item{add_plot}{\code{signature(x = "TimeMap")}: ... }
#' \code{\link{add_plot,TimeMap-method}} \item{as.character}{\code{signature(x
#' = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
#' \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getFunctionDefinition,TimeMap-method}}
#' \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... }
#' \code{\link{getTimeRange,TimeMap-method}} }
NULL





#' constant decomposition operator
#' 
#' no Description
#' 
#' 
#' @name UnBoundInFlux-class
#' @docType class
#' @section Methods: Exported methods directly defined for class UnBoundInFlux:
#' 
#' \describe{ \item{getFunctionDefinition}{\code{signature(object =
#' "UnBoundInFlux")}: ... }
#' \code{\link{getFunctionDefinition,UnBoundInFlux-method}}
#' \item{getTimeRange}{\code{signature(object = "UnBoundInFlux")}: ... }
#' \code{\link{getTimeRange,UnBoundInFlux-method}} }
#' 
#' Methods inherited from superclasses:
#' 
#' from class InFlux:
#' 
#' \describe{ \item{GeneralInFlux}{\code{signature(object = "InFlux")}: ... }
#' \code{\link{GeneralInFlux,InFlux-method}} }
NULL





#' constant decomposition operator
#' 
#' no Description
#' 
#' 
#' @name UnBoundLinDecompOp-class
#' @docType class
#' @section Methods: Exported methods directly defined for class
#' UnBoundLinDecompOp:
#' 
#' \describe{ \item{BoundLinDecompOp}{\code{signature(map =
#' "UnBoundLinDecompOp")}: ... }
#' \code{\link{BoundLinDecompOp,UnBoundLinDecompOp-method}}
#' \item{getFunctionDefinition}{\code{signature(object =
#' "UnBoundLinDecompOp")}: ... }
#' \code{\link{getFunctionDefinition,UnBoundLinDecompOp-method}}
#' \item{getTimeRange}{\code{signature(object = "UnBoundLinDecompOp")}: ... }
#' \code{\link{getTimeRange,UnBoundLinDecompOp-method}} }
#' 
#' Methods inherited from superclasses:
#' 
#' from class DecompOp:
#' 
#' \describe{ \item{GeneralDecompOp}{\code{signature(object = "DecompOp")}: ...
#' } \code{\link{GeneralDecompOp,DecompOp-method}} }
NULL





#' UnBoundLinDecompOp,function-method construct from matrix valued function
#' 
#' This method creates a UnBoundLinDecompOp from a matrix The operator is
#' assumed to act on the vector of carbon stocks by multiplication of the (time
#' dependent) matrix from the left.
#' 
#' 
#' @name UnBoundLinDecompOp,function-method
#' @docType methods
#' @param matFunc : of class function
NULL





#' UnBoundLinDecompOp S4 generic
#' 
#' no Description
#' 
#' 
#' @param matFunc see the method arguments for details
#' @section Methods: \code{\link{UnBoundLinDecompOp,function-method}}\cr
NULL



