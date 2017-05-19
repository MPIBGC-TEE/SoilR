#
# vim:set ff=unix expandtab ts=2 sw=2:
AWBmodel<-structure(
  function # Implementation of the microbial model AWB (Allison, Wallenstein, Bradford, 2010)
  ### This function implements the microbial model AWB (Allison, Wallenstein, Bradford, 2010), a four-pool model with a microbial biomass, enzyme, SOC and DOC pools. It is a special case of the general nonlinear model.
  ##details<< This implementation containts default parameters presented in Allison et al. (2010). 
  ##references<< Allison, S.D., M.D. Wallenstein, M.A. Bradford. 2010. Soil-carbon response to warming dependent on microbial physiology.
  ## Nature Geoscience 3: 336-340.
  (t, ##<< vector of times (in hours) to calculate a solution.
   V_M=100000000, ##<< a scalar representing the maximum rate of uptake (mg DOC cm-3 h-1). Equivalent to V_maxuptake0 in original paper.
   V_m=100000000, ##<< a scalar representing the maximum rate of decomposition of SOM (mg SOM cm-3 h-1). Equivalent to V_max0 in original paper.
   r_B=0.0002, ##<< a scalar representing the rate constant of microbial death (h-1). Equivalent to r_death in original publication.
   r_E=0.000005, ##<< a scalar representing the rate constant of enzyme production (h-1). Equivalent to r_EnzProd in original publication.
   r_L=0.001, ##<< a scalar representing the rate constant of enzyme loss (h-1). Equivalent to r_EnzLoss in original publication.
   a_BS=0.5, ##<< a scalar representing the fraction of the dead microbial biomass incorporated to SOC. MICtoSOC in original publication. 
   epsilon_0=0.63, ##<< a scalar representing the intercept of the CUE function (mg mg-1). CUE_0 in original paper.
   epsilon_s=-0.016, ##<< a scalar representing the slope of the CUE function (degree-1). CUE_slope in original paper.
   Km_0=500, ##<< a scalar representing the intercept of the half-saturation constant of SOC as a function of temperature (mg cm-3).
   Km_u0=0.1, ##<< a scalar representing the intercept of the half saturation constant of uptake as a function of temperature (mg cm-3).
   Km_s=0.5, ##<< a scalar representing the slope of the half saturation constant of SOC as a function of temperature (mg cm-3 degree-1).
   Km_us=0.1, ##<< a scalar representing the slope of the half saturation constant of uptake as a function of temperature (mg cm-3 degree-1).
   Ea=47, ##<< a scalar representing the activation energy (kJ mol-1).
   R=0.008314, ##<< a scalar representing the gas constant (kJ mol-1 degree-1).
   Temp1=20, ##<< a scalar representing the temperature in the output vector. 
   Temp2=20, ##<< a scalar representing the temperature in the transfer matrix. 
   ival=c(B=2.19159, E=0.0109579, S=111.876, D=0.00144928), ##<< a vector of length 4 with the initial values for the pools (mg cm-3).
   I_S=0.005, ##<< a scalar with the inputs to the SOC pool (mg cm-3 h-1).
   I_D=0.005 ##<< a scalar with the inputs to the DOC pool (mg cm-3 h-1).
  )
{
    t_start=min(t)
    t_end=max(t)
    nr=4
    
    #function with system of equations
    f=function(C,t){
      B=C[[1]] #Microbial biomass pool
      E=C[[2]] #Enzyme pool
      S=C[[3]] #SOC pool
      D=C[[4]] #DOC pool
      
      #Matrix of outputs
      O=matrix(byrow=TRUE,nrow=nr,c((r_B+r_E)*B,
                                     r_L*E,
                                    V_m * exp(-Ea/(R*(Temp1+273))) * (E*S)/((Km_s*Temp1+Km_0)+S),
                                    V_M * exp(-Ea/(R*(Temp1+273))) * (B*D)/((Km_us*Temp1+Km_u0)+D)
                                   )
               )
      return(O)
    }
    
    #List of transfer coefficients for T matrix
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
    
    inputrates=BoundInFlux(
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
    ### An object of class NlModel that can be further queried.
  }
,
ex=function(){
  
  hours=seq(0,800,0.1)
  
  #Run the model with default parameter values
  bcmodel=AWBmodel(t=hours)

  # fixme mm:
  # the next line causes trouble on Rforge Windows patched build

  #Cpools=getC(bcmodel)
  #
  ##Time solution
  #matplot(hours,Cpools,type="l",ylab="Concentrations",xlab="Hours",lty=1,ylim=c(0,max(Cpools)*1.2))
  #legend("topleft",c("B", "E", "S", "D"),lty=1,col=c(1:4),bty="n")
  #
  ##State-space diagram
  #plot(as.data.frame(Cpools))
  
}
)
