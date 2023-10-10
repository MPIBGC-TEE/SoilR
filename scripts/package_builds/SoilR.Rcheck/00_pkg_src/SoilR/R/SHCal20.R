#' The SHCal20 southern hemisphere radiocarbon curve for the 0-55,000 yr BP period
#' 
#' @description
#' Atmospheric radiocarbon calibration curve for the period 0 to 55,000 yr BP for the southern hemisphere.
#' 
#' @usage data(SHCal20)
#' @format
#'   A data frame with 9501 rows and 5 variables.
#'   \describe{
#'     \item{\code{CAL.BP}}{Calibrated age in years Before Present (BP).}
#'     \item{\code{C14.age}}{C14 age in years BP.}
#'     \item{\code{Sigma.C14.age}}{Standard deviation for \code{C14.age}.}
#'     \item{\code{Delta.14C}}{Delta.14C value in per mil.}
#'     \item{\code{Sigma.Delta.14C}}{Standard deviation of \code{Delta.14C} in per mil.}
#'   }
#' 
#' @details
#' All details about the derivation of this dataset are provided in Hogg et al. (2020).
#' 
#' @source <https://doi.org/10.1017/RDC.2020.59>
#' 
#' @references
#' Hogg, A., Heaton, T., Hua, Q., Palmer, J., Turney, C., Southon, J., . . . Wacker, L. (2020). 
#' SHCal20 Southern Hemisphere Calibration, 0–55,000 Years cal BP. Radiocarbon, 62(4), 759-778. 
#' doi:10.1017/RDC.2020.59 
#' 
#' @examples
#'     plot(SHCal20$CAL.BP, SHCal20$Delta.14C, type="l", 
#'          xlab="cal BP", ylab="Delta14C (per mil)")

#' 
#' @author Ingrid Chanca \email{ichanca@bgc-jena.mpg.de} 
#' @docType data
#' @keywords datasets
'SHCal20'

