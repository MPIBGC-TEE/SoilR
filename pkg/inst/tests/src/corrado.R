#
# vim:set ff=unix expandtab ts=2 sw=2:
# 	  |C_am |	      |X_01| 
# 	  |C_as	|	      |X_02|
# 	  |C_bm	|	      |X_03|
# 	  |C_bs	|	      |X_04|
# 	  |C_fw	|	      |X_05|
# 	  |C_acw|	      |X_06|
# 	  |C_bcw|	      |X_07|
# 	  |C_mic|	      |X_08|
# 	  |C_slo|	      |X_09|
# X =	|C_pas|   =   |X_10| 
# 	  |N_am	|	      |X_11|  
#   	|N_as	|	      |X_12|
# 	  |N_bm	|	      |X_13|
# 	  |N_bs	|	      |X_14|
# 	  |N_fw	|	      |X_15|
# 	  |N_acw|	      |X_16|
# 	  |N_bcw|	      |X_17|
# 	  |N_mic|	      |X_18|
# 	  |N_slo|	      |X_19|
# 	  |N_pas|	      |X_20|
# 	  |N_ino|	      |X_21|

library(SoilR)
  
# Soil texture
clay_frac = 0.2
silt_frac = 0.3
sand_frac = 0.5

# Decomposition base rates (1/year)
# To change into 1/day!!!
kam  <- 14.8
kas  <- 3.9
kbm  <- 18.5
kbs  <- 4.9
kfw  <- 0.3
kacw <- 0.3 # to change!
kbcw <- 0.3 # to change!
kmic <- 7.3
kslo <- 0.01
kpas <- 0.0045

# Immobilization rate
kn   <- 0.1 # to define!!! This is a default value!

# Fractions of internal fluxes into SOM pools (constant over time)
rslo  = 0.25      # (Fraction into Slow pool. Litter lignin concentration)
rmic  = 1 - rslo  # (Fraction into Microbial pool)
rms   = (1 - 0.004 - 0.17 * (clay_frac + silt_frac))
rp    = 0.003 - 0.009 * clay_frac
rsm   = 1 - 0.55 - rp

# Time range and pool number
  t_start = 0
t_end   = 20
t       = seq(t_start, t_end, by = 1/ 365) # to check!!!

nr      = 21 # Number of pools

tn      = 100 
tol     = .02 / tn


# Temperature and Moisture values ----
# Default values. Given as input from a csv file
TempData = data.frame(t, Temp = 15 + sin(2 * pi * t) 
                      + rnorm(n = length(t), mean = 0, sd = 1))
TempFunction=approxfun(x=TempData$t,y=TempData$Temp)

MoisData = data.frame(t, Mois = 70 + 10 * sin(2 * pi * t - (pi / 7)) 
                      + rnorm(n = length(t), mean = 0, sd = 1))
MoisFunction=approxfun(x=MoisData$t,y=MoisData$Mois)
# chi(t) facto


chi_T1_W1 <- function(t){
  fT.Q10(Temp = TempFunction(t), Q10 = 2)*
    fW.Daycent1(swc =MoisFunction(t)/100)
}
chi_T1_W2 <- function(t){
  fT.Q10(Temp = TempFunction(t), Q10 = 2)*
    fW.Gompertz(theta = MoisFunction(t))
}

chi_T2_W1 <- function(t){
  fT.Century1(Temp = TempFunction(t), Tmax = 45, Topt = 35)*
    fW.Daycent1(swc =MoisFunction(t)/100)
}
chi_T2_W2 <- function(t){
  fT.Century1(Temp = TempFunction(t), Tmax = 45, Topt = 35)*
    fW.Gompertz(theta = MoisFunction(t))
}
chi_T3_W1 <- function(t){
  fT.LandT(Temp = TempFunction(t))*
    fW.Daycent1(swc =MoisFunction(t)/100)
}
chi_T3_W2 <- function(t){
  fT.LandT(Temp = TempFunction(t))*
    fW.Gompertz(theta = MoisFunction(t))
}
#chiFuncs=list(chi_T1_W1,chi_T1_W2,..)
# for (chi in chiFuncs){ 
  #do something}
  chi<-chi_T1_W1 
  # Compute the limitation factor phi_mn(X) ----
  phi_mn <- function(X,t) {
    C_am   <- X[[1]]
    C_as	 <- X[[2]]
    C_bm	 <- X[[3]]
    C_bs	 <- X[[4]]
    C_fw	 <- X[[5]]
    C_acw  <- X[[6]]
    C_bcw  <- X[[7]]
    C_mic  <- X[[8]]
    C_slo  <- X[[9]]
    C_pas  <- X[[10]]
    N_am	 <- X[[11]] 
    N_as	 <- X[[12]]
    N_bm	 <- X[[13]]
    N_bs	 <- X[[14]]
    N_fw	 <- X[[15]]
    N_acw  <- X[[16]]
    N_bcw  <- X[[17]]
    N_mic  <- X[[18]]
    N_slo  <- X[[19]]
    N_pas  <- X[[20]]
    N_ino  <- X[[21]]
    
    term1  = kam * C_am * ( (1 / (C_am / N_am) ) - (0.4 / (C_mic / N_mic)) ) +
               kas * C_as * ( (1 / (C_as / N_as) ) - ((0.4 * rmic) / (C_mic / N_mic)) - (rslo / (C_slo / N_slo)) ) +
               kbm * C_bm * ( (1 / (C_bm / N_bm) ) - (0.45 / (C_mic / N_mic)) ) +
               kbs * C_bs * ( (1 / (C_bs / N_bs) ) - ((0.4 * rmic) / (C_mic / N_mic)) - (rslo / (C_slo / N_slo)) ) +
               kfw * C_fw * ( (1 / (C_fw / N_fw) ) - ((0.45 * rmic) / (C_mic / N_mic)) - (rslo / (C_slo / N_slo)) ) +
               kacw * C_acw * ( (1 / (C_acw / N_acw) ) - ((0.45 * rmic) / (C_mic / N_mic)) - (rslo / (C_slo / N_slo)) ) +
               kbcw * C_bcw * ( (1 / (C_bcw / N_bcw) ) - ((0.45 * rmic) / (C_mic / N_mic)) - (rslo / (C_slo / N_slo)) ) +
               kslo * C_slo * ( (1 / (C_slo / N_slo) ) - (rsm / (C_mic / N_mic)) - (rp / (C_slo / N_slo)) ) +
               kpas * C_pas * ( (1 / (C_pas / N_pas) ) - (0.45 / (C_mic / N_mic)) )
    
    phi_mn=1
    phi   = phi_mn * chi(t) / term1
    
    # Maximum immobilization available
    imm_max  = N_ino * kn * chi(t)
    
    # Different conditions
    if(term1 >= 0) { 
      
      phi_mn = 1 
      
    } else {
      
      if(abs(phi) <= imm_max) {
        phi_mn = 1
      } else {
        phi_mn = -imm_max / phi
      }
      
    }
    return(phi_mn)
  }

   
  # Internal fluxes
  internal_fluxes=list()
  
  # Carbon fluxes ----
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  
 # Flux from Abovegr. Metabolic to Microbial SOM
  internal_fluxes[["1_to_8"]] = function(X,t) {
    # 0.4 * kam * chi(t) * C_am(t) * phi_mn(X)
    C_am <- X[1]
    0.4 * kam * chi(t) * C_am * phi_mn(X,t)
  }
  
  # Flux from Abovegr. Structural to Microbial SOM
  internal_fluxes[["2_to_8"]] = function(X,t) {
    C_as <- X[2]
    0.4 * rmic * kas * chi(t) * C_as(t) * phi_mn(X)
  }
#  
#  # Flux from Abovegr. Structural to Slow SOM
#  internal_fluxes[["2_to_9"]] = function(X,t) {
#    C_as <- X[2]
#    0.7 * rslo * kas * chi(t) * C_as(t) * phi_mn(X)
#  }
#  
#  # Flux from Belowgr. Metabolic to Microbial SOM
#  internal_fluxes[["3_to_8"]] = function(X,t) {
#    C_bm <- X[3]
#    0.45 * kbm * chi(t) * C_bm(t) * phi_mn(X)
#  }
#  
#  # Flux from Belowgr. Structural to Microbial SOM
#  internal_fluxes[["4_to_8"]] = function(X,t) {
#    C_bs <- X[4]
#    0.45 * rmic * kbs * chi(t) * C_bs(t) * phi_mn(X)
#  }
#  
#  # Flux from Belowgr. Structural to Slow SOM
#  internal_fluxes[["4_to_9"]] = function(X,t) {
#    C_bs <- X[4]
#    0.7 * rslo * kbs * chi(t) * C_bs(t) * phi_mn(X)
#  }
#  
#  # Flux from Fine Wood to Microbial SOM
#  internal_fluxes[["5_to_8"]] = function(X,t) {
#    C_fw <- X[5]
#    0.45 * rmic * kfw * chi(t) * C_fw(t) * phi_mn(X)
#  }
#  
#  # Flux from Fine Wood to Slow SOM
#  internal_fluxes[["5_to_9"]] = function(X,t) {
#    C_fw <- X[5]
#    0.7 * rslo * kfw * chi(t) * C_fw(t) * phi_mn(X)
#  }
#  
#  # Flux from Abovegr. Coarse Wood to Microbial SOM
#  internal_fluxes[["6_to_8"]] = function(X,t) {
#    C_acw <- X[6]
#    0.45 * rmic * kacw * chi(t) * C_acw(t) * phi_mn(X)
#  }
#  
#  # Flux from Abovegr. Coarse Wood to Slow SOM
#  internal_fluxes[["6_to_9"]] = function(X,t) {
#    C_acw <- X[6]
#    0.7 * rslo * kacw * chi(t) * C_acw(t) * phi_mn(X)
#  }
#  
#  # Flux from Belowgr. Coarse Wood to Microbial SOM
#  internal_fluxes[["7_to_8"]] = function(X,t) {
#    C_bcw <- X[7]
#    0.45 * rmic * kbcw * chi(t) * C_bcw(t) * phi_mn(X)
#  }
#  
#  # Flux from Belowgr. Coarse Wood to Slow SOM
#  internal_fluxes[["7_to_9"]] = function(X,t) {
#    C_bcw <- X[7]
#    0.7 * rslo * kbcw * chi(t) * C_bcw(t) * phi_mn(X)
#  }
#  
#  # Flux from Microbial SOM to Slow SOM
#  internal_fluxes[["8_to_9"]] = function(X,t) {
#    C_mic <- X[8]
#    rms * kmic * chi(t) * C_mic * phi_mn(X)
#  }
#  
#  # Flux from Microbial SOM to Passive SOM
#  internal_fluxes[["8_to_10"]] = function(X,t) {
#    C_mic <- X[8]
#    0.004 * kmic * chi(t) * C_mic * phi_mn(X)
#  }
#  
#  # Flux from Slow SOM to Microbial SOM
#  internal_fluxes[["9_to_8"]] = function(X,t) {
#    C_slo <- X[9]
#    rsm * kslo * chi(t) * C_slo(t) * phi_mn(X)
#  }
#  
#  # Flux from Slow SOM to Passive SOM
#  internal_fluxes[["9_to_10"]] = function(X,t) {
#    C_slo <- X[9]
#    rp * kslo * chi(t) * C_slo(t) * phi_mn(X)
#  }
#  
#  # Flux from Passive SOM to Microbial SOM
#  internal_fluxes[["10_to_8"]] = function(X,t) {
#    C_pas <- X[10]
#    0.45 * kpas * chi(t) * C_pas(t) * phi_mn(X)
#  }
#  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#  
#  
#  
#  # Nitrogen fluxes ----
#  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#  
#  if(term1 >= 0) { # Case 1 (Mineralization)
#  
#    # Nitrogen Flux from Abovegr. Metabolic to Microbial SOM
#    internal_fluxes[["11_to_18"]] = function(X,t) {
#      C_am  <- X[1]
#      N_am  <- X[11]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      0.4 * ( (C_am(t) / N_am(t)) / (C_mic / N_mic) ) * kam * chi(t) * phi_mn(X) * N_am(t)
#    }
#    
#    # Nitrogen Flux from Abovegr. Metabolic to Inorganic N pool
#    internal_fluxes[["11_to_21"]] = function(X,t) {
#      C_am  <- X[1]
#      N_am  <- X[11]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      ( 1 - 0.4 * ( (C_am(t) / N_am(t)) / (C_mic / N_mic) ) ) * kam * chi(t) * phi_mn(X) * N_am(t)
#    }
#    
#    # Nitrogen Flux from Abovegr. Structural to Microbial SOM
#    internal_fluxes[["12_to_18"]] = function(X,t) {
#      C_as  <- X[2]
#      N_as  <- X[12]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      0.4 * ( (C_as(t) / N_as(t)) / (C_mic / N_mic) ) * kas * chi(t) * phi_mn(X) * rmic * N_as(t)
#    }
#    
#    # Nitrogen Flux from Abovegr. Structural to Slow SOM
#    internal_fluxes[["12_to_19"]] = function(X,t) {
#      C_as  <- X[2]
#      N_as  <- X[12]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( (C_as(t) / N_as(t)) / (C_slo(t) / N_slo(t)) ) * kas * chi(t) * phi_mn(X) * rslo * N_as(t)
#    }
#    
#    # Nitrogen Flux from Abovegr. Structural to Inorganic N pool
#    internal_fluxes[["12_to_21"]] = function(X,t) {
#      C_as  <- X[2]
#      N_as  <- X[12]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( 1 - 0.4 * ( (C_as(t) / N_as(t)) / (C_mic / N_mic) ) * rmic - rslo * ( (C_as(t) / N_as(t)) / (C_slo(t) / N_slo(t)) ) ) * kas * chi(t) * phi_mn(X) * N_as(t)
#    }
#    
#    # Nitrogen Flux from Belowgr. Metabolic to Microbial SOM
#    internal_fluxes[["13_to_18"]] = function(X,t) {
#      C_bm  <- X[3]
#      N_bm  <- X[13]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      0.45 * ( (C_bm(t) / N_bm(t)) / (C_mic / N_mic) ) * kbm * chi(t) * phi_mn(X) * N_bm(t)
#    }
#    
#    # Nitrogen Flux from Belowgr. Metabolic to Inorganic N pool
#    internal_fluxes[["13_to_21"]] = function(X,t) {
#      C_bm  <- X[3]
#      N_bm  <- X[13]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      ( 1 - 0.45 * ( (C_bm(t) / N_bm(t)) / (C_mic / N_mic) ) ) * kbm * chi(t) * phi_mn(X) * N_bm(t)
#    }
#    
#    # Nitrogen Flux from Belowgr. Structural to Microbial SOM
#    internal_fluxes[["14_to_18"]] = function(X,t) {
#      C_bs  <- X[4]
#      N_bs  <- X[14]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      0.45 * ( (C_bs(t) / N_bs(t)) / (C_mic / N_mic) ) * kbs * chi(t) * phi_mn(X) * rmic * N_bs(t)
#    }
#    
#    # Nitrogen Flux from Belowgr. Structural to Slow SOM
#    internal_fluxes[["14_to_19"]] = function(X,t) {
#      C_bs  <- X[4]
#      N_bs  <- X[14]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( (C_bs(t) / N_bs(t)) / (C_slo(t) / N_slo(t)) ) * kbs * chi(t) * phi_mn(X) * rslo * N_bs(t)
#    }
#    
#    # Nitrogen Flux from Belowgr. Structural to Inorganic N pool
#    internal_fluxes[["14_to_21"]] = function(X,t) {
#      C_bs  <- X[4]
#      N_bs  <- X[14]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( 1 - 0.4 * ( (C_bs(t) / N_bs(t)) / (C_mic / N_mic) ) * rmic - rslo * ( (C_bs(t) / N_bs(t)) / (C_slo(t) / N_slo(t)) ) ) * kbs * chi(t) * phi_mn(X) * N_bs(t)
#    }
#    
#    # Nitrogen Flux from Fine Wood to Microbial SOM
#    internal_fluxes[["15_to_18"]] = function(X,t) {
#      C_fw  <- X[5]
#      N_fw  <- X[15]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      0.45 * ( (C_fw(t) / N_fw(t)) / (C_mic / N_mic) ) * kfw * chi(t) * phi_mn(X) * rmic * N_fw(t)
#    }
#    
#    # Nitrogen Flux from Fine Wood to Slow SOM
#    internal_fluxes[["15_to_19"]] = function(X,t) {
#      C_fw  <- X[5]
#      N_fw  <- X[15]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( (C_fw(t) / N_fw(t)) / (C_slo(t) / N_slo(t)) ) * kfw * chi(t) * phi_mn(X) * rslo * N_fw(t)
#    }
#    
#    # Nitrogen Flux from Fine Wood to Inorganic N pool
#    internal_fluxes[["15_to_21"]] = function(X,t) {
#      C_fw  <- X[5]
#      N_fw  <- X[15]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( 1 - 0.4 * ( (C_fw(t) / N_fw(t)) / (C_mic / N_mic) ) * rmic - rslo * ( (C_fw(t) / N_fw(t)) / (C_slo(t) / N_slo(t)) ) ) * kfw * chi(t) * phi_mn(X) * N_fw(t)
#    }
#    
#    # Nitrogen Flux from Abovegr. Coarse Wood to Microbial SOM
#    internal_fluxes[["16_to_18"]] = function(X,t) {
#      C_acw  <- X[6]
#      N_acw  <- X[16]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      0.45 * ( (C_acw(t) / N_acw(t)) / (C_mic / N_mic) ) * kacw * chi(t) * phi_mn(X) * rmic * N_acw(t)
#    }
#    
#    # Nitrogen Flux from Abovegr. Coarse Wood to Slow SOM
#    internal_fluxes[["16_to_19"]] = function(X,t) {
#      C_acw  <- X[6]
#      N_acw  <- X[16]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( (C_acw(t) / N_acw(t)) / (C_slo(t) / N_slo(t)) ) * kacw * chi(t) * phi_mn(X) * rslo * N_acw(t)
#    }
#    
#    # Nitrogen Flux from Abovegr. Coarse Wood to Inorganic N pool
#    internal_fluxes[["16_to_21"]] = function(X,t) {
#      C_acw  <- X[6]
#      N_acw  <- X[16]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( 1 - 0.4 * ( (C_acw(t) / N_acw(t)) / (C_mic / N_mic) ) * rmic - rslo * ( (C_acw(t) / N_acw(t)) / (C_slo(t) / N_slo(t)) ) ) * kacw * chi(t) * phi_mn(X) * N_acw(t)
#    }
#    
#    # Nitrogen Flux from Belowgr. Coarse Wood to Microbial SOM
#    internal_fluxes[["17_to_18"]] = function(X,t) {
#      C_bcw  <- X[7]
#      N_bcw  <- X[17]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      0.45 * ( (C_bcw(t) / N_bcw(t)) / (C_mic / N_mic) ) * kbcw * chi(t) * phi_mn(X) * rmic * N_bcw(t)
#    }
#    
#    # Nitrogen Flux from Belowgr. Coarse Wood to Slow SOM
#    internal_fluxes[["17_to_19"]] = function(X,t) {
#      C_bcw  <- X[7]
#      N_bcw  <- X[17]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( (C_bcw(t) / N_bcw(t)) / (C_slo(t) / N_slo(t)) ) * kbcw * chi(t) * phi_mn(X) * rslo * N_bcw(t)
#    }
#    
#    # Nitrogen Flux from Belowgr. Coarse Wood to Inorganic N pool
#    internal_fluxes[["17_to_21"]] = function(X,t) {
#      C_bcw  <- X[7]
#      N_bcw  <- X[17]
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      ( 1 - 0.4 * ( (C_bcw(t) / N_bcw(t)) / (C_mic / N_mic) ) * rmic - rslo * ( (C_bcw(t) / N_bcw(t)) / (C_slo(t) / N_slo(t)) ) ) * kbcw * chi(t) * phi_mn(X) * N_bcw(t)
#    }
#    
#    # Nitrogen Flux from Microbial SOM to Slow SOM
#    internal_fluxes[["18_to_19"]] = function(X,t) {
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_pas <- X[10]
#      N_pas <- X[20]
#      ( 1 - 0.004 * ( (C_mic / N_mic) / (C_pas(t) / N_pas(t)) ) ) * kmic * chi(t) * phi_mn(X) * N_mic
#    }
#    
#    # Nitrogen Flux from Microbial SOM to Passive SOM
#    internal_fluxes[["18_to_20"]] = function(X,t) {
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_pas <- X[10]
#      N_pas <- X[20]
#      0.004 * ( (C_mic / N_mic) / (C_pas(t) / N_pas(t)) ) * kmic * chi(t) * phi_mn(X) * N_mic
#    }
#    
#    # Nitrogen Flux from Slow SOM to Microbial SOM
#    internal_fluxes[["19_to_18"]] = function(X,t) {
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      rsm * ( (C_slo(t) / N_slo(t)) / (C_mic / N_mic) ) * kslo * chi(t) * phi_mn(X) * N_slo(t)
#    }
#    
#    # Nitrogen Flux from Slow SOM to Passive SOM
#    internal_fluxes[["19_to_20"]] = function(X,t) {
#      C_slo <- X[9]
#      N_slo <- X[19]
#      C_pas <- X[10]
#      N_pas <- X[20]
#      rp * ( (C_slo(t) / N_slo(t)) / (C_pas(t) / N_pas(t)) ) * kslo * chi(t) * phi_mn(X) * N_slo(t)
#    }
#    
#    # Nitrogen Flux from Slow SOM to Inorganic N pool
#    internal_fluxes[["19_to_21"]] = function(X,t) {
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_slo <- X[9]
#      N_slo <- X[19]
#      C_pas <- X[10]
#      N_pas <- X[20]
#      ( 1 - rsm * ( (C_slo(t) / N_slo(t)) / (C_mic / N_mic) ) - rp * ( (C_slo(t) / N_slo(t)) / (C_pas(t) / N_pas(t)) ) ) * kslo * chi(t) * phi_mn(X) * N_slo(t)
#    }
#    
#    # Nitrogen Flux from Passive SOM to Microbial SOM
#    internal_fluxes[["20_to_18"]] = function(X,t) {
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_pas <- X[10]
#      N_pas <- X[20]
#      0.45 * ( (C_pas(t) / N_pas(t)) / (C_mic / N_mic) ) * kpas * chi(t) * phi_mn(X) * N_pas(t)
#    }
#    
#    # Nitrogen Flux from Passive SOM to Inorganic N pool
#    internal_fluxes[["20_to_21"]] = function(X,t) {
#      C_mic <- X[8]
#      N_mic <- X[18]
#      C_pas <- X[10]
#      N_pas <- X[20]
#      ( 1 - 0.45 * ( (C_pas(t) / N_pas(t)) / (C_mic / N_mic) ) ) * kpas * chi(t) * phi_mn(X) * N_pas(t)
#    }
#  
#  } else {
#    
#    if(phi(X) <= imm_max(X)) {
#      
#      # Case 2 (Immobilization with phi_mn = 1) (phi_mn alreay set in the phi_mn function)
#      # Different Nitrogen fluxes
#      
#    } else {
#      
#      phi_mn(X) = -imm_max(X) / phi(X)
#      
#    }
#  
#    # Case 3 (Immobilization with phi_mn = -imm_max(X) / phi(X)) (phi_mn already set in the phi_mn function)
#    # Different Nitrogen fluxes
#   
#    
#       
#    
#}
#      
#  
#  # ----
#  internal_fluxes[["3_to_2"]] = function(C,t) {2}
#  out_fluxes = list()
#  out_fluxes[["1"]] = function(X,t) {
#  }
#
