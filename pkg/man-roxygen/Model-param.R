#' @param object A model(run) as produced by the constructors 
#' \itemize{
#'  \item{\code{\link{Model}}}
#'  \item{\code{\link{Model_by_PoolNames}}}
#'  \item{\code{\link{Model_14}}}
#' }
#' the function \code{\link{GeneralModel}} or the functions listed in 
#' \code{\link{predefinedModels}}. 
#' A model represents the initial value problem (IVP)  
#' for the contents of the pool consisting of 
#' \itemize{
#'  \item{ The initial values of the pool content}
#'  \item{ The system of ordinary differential oqations, as dictated by the fluxes}
#'  \item{ The times for which the solution of the IVP is evaluated.}
#' }
