#' @param object A modelrun as produced by the constructors: 
#'  \code{\link{Model}},
#'  \code{\link{Model_by_PoolNames}},
#'  \code{\link{Model_14}}
#' the function \code{\link{GeneralModel}} or the functions listed in 
#' \code{\link{predefinedModels}}. \cr
#' A model represents the initial value problem (IVP)  
#' for the contents of the pool consisting of 
#' \itemize{
#'  \item{ The initial values of the pool content}
#'  \item{ The system of ordinary differential equations, as dictated by the fluxes}
#'  \item{ The times for which the solution of the IVP is evaluated.}
#' }
