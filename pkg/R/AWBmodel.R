#' Implementation of the microbial model AWB (Allison, Wallenstein, Bradford,
#' 2010)
#' 
#' This function implements the microbial model AWB (Allison, Wallenstein,
#' Bradford, 2010), a four-pool model with a microbial biomass, enzyme, SOC and
#' DOC pools. It is a special case of the general nonlinear model.
#' 
#' This implementation containts default parameters presented in Allison et al.
#' (2010).
#' 
#' @param t vector of times (in hours) to calculate a solution.
#' @param V_M a scalar representing the maximum rate of uptake (mg DOC cm-3
#' h-1). Equivalent to V_maxuptake0 in original paper.
#' @param V_m a scalar representing the maximum rate of decomposition of SOM
#' (mg SOM cm-3 h-1). Equivalent to V_max0 in original paper.
#' @param r_B a scalar representing the rate constant of microbial death (h-1).
#' Equivalent to r_death in original publication.
#' @param r_E a scalar representing the rate constant of enzyme production
#' (h-1). Equivalent to r_EnzProd in original publication.
#' @param r_L a scalar representing the rate constant of enzyme loss (h-1).
#' Equivalent to r_EnzLoss in original publication.
#' @param a_BS a scalar representing the fraction of the dead microbial biomass
#' incorporated to SOC. MICtoSOC in original publication.
#' @param epsilon_0 a scalar representing the intercept of the CUE function (mg
#' mg-1). CUE_0 in original paper.
#' @param epsilon_s a scalar representing the slope of the CUE function
#' (degree-1). CUE_slope in original paper.
#' @param Km_0 a scalar representing the intercept of the half-saturation
#' constant of SOC as a function of temperature (mg cm-3).
#' @param Km_u0 a scalar representing the intercept of the half saturation
#' constant of uptake as a function of temperature (mg cm-3).
#' @param Km_s a scalar representing the slope of the half saturation constant
#' of SOC as a function of temperature (mg cm-3 degree-1).
#' @param Km_us a scalar representing the slope of the half saturation constant
#' of uptake as a function of temperature (mg cm-3 degree-1).
#' @param Ea a scalar representing the activation energy (kJ mol-1).
#' @param R a scalar representing the gas constant (kJ mol-1 degree-1).
#' @param Temp1 a scalar representing the temperature in the output vector.
#' @param Temp2 a scalar representing the temperature in the transfer matrix.
#' @param ival a vector of length 4 with the initial values for the pools (mg
#' cm-3).
#' @param I_S a scalar with the inputs to the SOC pool (mg cm-3 h-1).
#' @param I_D a scalar with the inputs to the DOC pool (mg cm-3 h-1).
#' @return An object of class NlModel that can be further queried.
#' @references Allison, S.D., M.D. Wallenstein, M.A. Bradford. 2010.
#' Soil-carbon response to warming dependent on microbial physiology. Nature
#' Geoscience 3: 336-340.
#' @examples
#' hours=seq(0,800,0.1)
#' 
#' #Run the model with default parameter values
#' bcmodel=AWBmodel(t=hours)
#' Cpools=getC(bcmodel)
#' ##Time solution
#' # fixme mm:
#' # the next line causes trouble on Rforge Windows patched build
#' matplot(hours,Cpools,type="l",ylab="Concentrations",xlab="Hours",lty=1,ylim=c(0,max(Cpools)*1.2))
#' ##State-space diagram
#' plot(as.data.frame(Cpools))
AWBmodel<- function 
  (t, 
   V_M=100000000, 
   V_m=100000000, 
   r_B=0.0002, 
   r_E=0.000005, 
   r_L=0.001, 
   a_BS=0.5, 
   epsilon_0=0.63, 
   epsilon_s=-0.016, 
   Km_0=500, 
   Km_u0=0.1, 
   Km_s=0.5, 
   Km_us=0.1, 
   Ea=47, 
   R=0.008314, 
   Temp1=20, 
   Temp2=20, 
   ival=c(B=2.19159, E=0.0109579, S=111.876, D=0.00144928), 
   I_S=0.005, 
   I_D=0.005 
  )
{
    t_start=min(t)
    t_end=max(t)
    nr=4
    f=function(C,t){
      B=C[[1]] 
      E=C[[2]] 
      S=C[[3]] 
      D=C[[4]] 
      O=matrix(byrow=TRUE,nrow=nr,c((r_B+r_E)*B,
                                     r_L*E,
                                    V_m * exp(-Ea/(R*(Temp1+273))) * (E*S)/((Km_s*Temp1+Km_0)+S),
                                    V_M * exp(-Ea/(R*(Temp1+273))) * (B*D)/((Km_us*Temp1+Km_u0)+D)
                                   )
               )
      return(O)
    }
    alpha=list()
    alpha[["1_to_2"]]=function(C,t){
      r_E/(r_B+r_E)
    }
    alpha[["1_to_3"]]=function(C,t){
      (a_BS*r_B)/(r_B+r_E)
    }
    alpha[["1_to_4"]]=function(C,t){
      r_B*(1-a_BS)/(r_B + r_E) 
    }
    alpha[["4_to_1"]]=function(C,t){
      epsilon_0+epsilon_s*Temp2
    }
    Anl=new("TransportDecompositionOperator",t_start,Inf,nr,alpha,f)
    inputrates=BoundInFluxes(
      function(t){
        matrix(
          nrow=nr,
          ncol=1,
          c(0,0, I_S, I_D)
        )
      },
      t_start,
      t_end
    )
    modnl=GeneralNlModel(t, Anl, ival, inputrates, deSolve.lsoda.wrapper)
    return(modnl)
}
