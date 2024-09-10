#' Mole fraction of 14CO2 relative to dry air
#' 
#' Computes the amount of radiocarbon, in units of mol/mol of CO2 based on 
#' its Delta14C, the mole fraction of CO2, and its delta13C signature
#' 
#' @param Delta14CO2 numeric value of Delta14C-CO2 in permil
#' @param f constant value relating the molecular weight of carbon, 
#' the absolute activity of the standard, the radiocarbon decay constant,
#' and the Avogadro number
#' @param delta13CO2 numeric value of delta13C-CO2 in permil
#' @param xCO2 numeric value of mole fraction of CO2 in dry air in ppm
#' @return a numeric value of the mole fraction of 14CO2 in dray air in mol/mol
#' @references S. E. Schwartz, Q. Hua, D. E. Andrews, R. F. Keeling, 
#' S. J. Lehman, J. C. Turnbull, P. J. Reimer, J. B. Miller, and 
#' H. A. J. Meijer. Discussion: presentation of atmospheric 14CO2 data. 
#' Radiocarbon, 66(2): 386–399, 2024. doi: 10.1017/RDC.2024.27.
#' @examples
#' # For present (2022) xCO2 of 420 ppm and Delta14CO2 of 0 permil 
#' # (Schwartz et al. 2024, Appendix A). 
#' x14CO2(Delta14CO2 = 0, xCO2=420)

x14CO2<-function(Delta14CO2, f=1.176e-12, delta13CO2=-7, xCO2){
  g=((1+(delta13CO2/1000))/(1-0.025))^2
  f*g*(1+(Delta14CO2/1000))*xCO2*1e-6
}
