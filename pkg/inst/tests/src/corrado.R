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
source("functions_corrado.R")

# Compute the limitation factor phi_mn ----
# The phi_mn factor controls the reduction of C,N decomposition fluxes due to the inorganic N limitations
imm_min_func <- function(X,t) {
  
  C_am   <- X[[1]]
  C_as	<- X[[2]]
  C_bm	<- X[[3]]
  C_bs	<- X[[4]]
  C_fw	<- X[[5]]
  C_acw  <- X[[6]]
  C_bcw  <- X[[7]]
  C_mic  <- X[[8]]
  C_slo  <- X[[9]]
  C_pas  <- X[[10]]
  N_am	<- X[[11]]
  N_as	<- X[[12]]
  N_bm	<- X[[13]]
  N_bs	<- X[[14]]
  N_fw	<- X[[15]]
  N_acw  <- X[[16]]
  N_bcw  <- X[[17]]
  N_mic  <- X[[18]]
  N_slo  <- X[[19]]
  N_pas  <- X[[20]]
  N_ino  <- X[[21]]

  term1  = kam * N_am * (1 - 0.4 * ((C_am / N_am) / (C_mic / N_mic)) )                                                          +
     kas * N_as * ( (1 - (0.4 * rmic * ((C_as / N_as) / (C_mic / N_mic))) - (rslo * ((C_as / N_as) / (C_slo / N_slo)))))        +
     kbm * N_bm * (1 - 0.45 * ((C_bm / N_bm) / (C_mic / N_mic)) )                                                               +
     kbs * N_bs * ( (1 - (0.45 * rmic * ((C_bs / N_bs) / (C_mic / N_mic))) - (rslo * ((C_bs / N_bs) / (C_slo / N_slo)))))       +
     kfw * N_fw * ( (1 - (0.45 * rmic * ((C_fw / N_fw) / (C_mic / N_mic))) - (rslo * ((C_fw / N_fw) / (C_slo / N_slo)))))       +
     kacw * N_acw * ( (1 - (0.45 * rmic * ((C_acw / N_acw) / (C_mic / N_mic))) - (rslo * ((C_acw / N_acw) / (C_slo / N_slo))))) +
     kbcw * N_bcw * ( (1 - (0.45 * rmic * ((C_bcw / N_bcw) / (C_mic / N_mic))) - (rslo * ((C_bcw / N_bcw) / (C_slo / N_slo))))) +
     kslo * N_slo * ( (1 - (rsm * ((C_slo / N_slo) / (C_mic / N_mic))) - (rp * ((C_slo / N_slo) / (C_slo / N_slo)))))           +
     kpas * N_pas * (1 - 0.45 * ((C_pas / N_pas) / (C_mic / N_mic)) )
  
  # Initial value of phi_mn
  phi_mn = 1
  
  # Immobilization/Mineralization rate
  phi    = phi_mn * chi(t) * term1
  
  # Maximum immobilization available
  imm_max  = N_ino * kn * chi(t)
  
  # Different conditions
  phi_mn = ifelse(term1 < 0 & abs(phi) > imm_max, -kn * N_ino / term1, 1)
  
  
  imm_min_list = list("term1" = term1, "phi_mn" = phi_mn, "phi" = phi, "imm_max" = imm_max)
  
  return(imm_min_list)
}



# Internal fluxes
internal_fluxes = list()

# Carbon fluxes ----
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Flux from Abovegr. Metabolic to Microbial SOM
internal_fluxes[["1_to_8"]] = function(X,t) {
  C_am <- X[1]
  imm_min_vec = imm_min_func(X,t)
  0.4 * kam * chi(t) * C_am * imm_min_vec$phi_mn
}

# Flux from Abovegr. Structural to Microbial SOM
internal_fluxes[["2_to_8"]] = function(X,t) {
  C_as <- X[2]
  imm_min_vec = imm_min_func(X,t)
  0.4 * rmic * kas * chi(t) * C_as * imm_min_vec$phi_mn
}

 # Flux from Abovegr. Structural to Slow SOM
 internal_fluxes[["2_to_9"]] = function(X,t) {
   C_as <- X[2]
   imm_min_vec = imm_min_func(X,t)
   0.7 * rslo * kas * chi(t) * C_as * imm_min_vec$phi_mn
 }

 # Flux from Belowgr. Metabolic to Microbial SOM
 internal_fluxes[["3_to_8"]] = function(X,t) {
   C_bm <- X[3]
   imm_min_vec = imm_min_func(X,t)
   0.45 * kbm * chi(t) * C_bm * imm_min_vec$phi_mn
 }

 # Flux from Belowgr. Structural to Microbial SOM
 internal_fluxes[["4_to_8"]] = function(X,t) {
   C_bs <- X[4]
   imm_min_vec = imm_min_func(X,t)
   0.45 * rmic * kbs * chi(t) * C_bs * imm_min_vec$phi_mn
 }

 # Flux from Belowgr. Structural to Slow SOM
 internal_fluxes[["4_to_9"]] = function(X,t) {
   C_bs <- X[4]
   imm_min_vec = imm_min_func(X,t)
   0.7 * rslo * kbs * chi(t) * C_bs * imm_min_vec$phi_mn
 }

 # Flux from Fine Wood to Microbial SOM
 internal_fluxes[["5_to_8"]] = function(X,t) {
   C_fw <- X[5]
   imm_min_vec = imm_min_func(X,t)
   0.45 * rmic * kfw * chi(t) * C_fw * imm_min_vec$phi_mn
 }

 # Flux from Fine Wood to Slow SOM
 internal_fluxes[["5_to_9"]] = function(X,t) {
   C_fw <- X[5]
   imm_min_vec = imm_min_func(X,t)
   0.7 * rslo * kfw * chi(t) * C_fw * imm_min_vec$phi_mn
 }

 # Flux from Abovegr. Coarse Wood to Microbial SOM
 internal_fluxes[["6_to_8"]] = function(X,t) {
   C_acw <- X[6]
   imm_min_vec = imm_min_func(X,t)
   0.45 * rmic * kacw * chi(t) * C_acw * imm_min_vec$phi_mn
 }

 # Flux from Abovegr. Coarse Wood to Slow SOM
 internal_fluxes[["6_to_9"]] = function(X,t) {
   C_acw <- X[6]
   imm_min_vec = imm_min_func(X,t)
   0.7 * rslo * kacw * chi(t) * C_acw * imm_min_vec$phi_mn
 }

 # Flux from Belowgr. Coarse Wood to Microbial SOM
 internal_fluxes[["7_to_8"]] = function(X,t) {
   C_bcw <- X[7]
   imm_min_vec = imm_min_func(X,t)
   0.45 * rmic * kbcw * chi(t) * C_bcw * imm_min_vec$phi_mn
 }

 # Flux from Belowgr. Coarse Wood to Slow SOM
 internal_fluxes[["7_to_9"]] = function(X,t) {
   C_bcw <- X[7]
   imm_min_vec = imm_min_func(X,t)
   0.7 * rslo * kbcw * chi(t) * C_bcw * imm_min_vec$phi_mn
 }

 # Flux from Microbial SOM to Slow SOM
 internal_fluxes[["8_to_9"]] = function(X,t) {
   C_mic <- X[8]
   imm_min_vec = imm_min_func(X,t)
   rms * kmic * chi(t) * C_mic * imm_min_vec$phi_mn
 }

 # Flux from Microbial SOM to Passive SOM
 internal_fluxes[["8_to_10"]] = function(X,t) {
   C_mic <- X[8]
   imm_min_vec = imm_min_func(X,t)
   0.004 * kmic * chi(t) * C_mic * imm_min_vec$phi_mn
 }

 # Flux from Slow SOM to Microbial SOM
 internal_fluxes[["9_to_8"]] = function(X,t) {
   C_slo <- X[9]
   imm_min_vec = imm_min_func(X,t)
   rsm * kslo * chi(t) * C_slo * imm_min_vec$phi_mn
 }

 # Flux from Slow SOM to Passive SOM
 internal_fluxes[["9_to_10"]] = function(X,t) {
   C_slo <- X[9]
   imm_min_vec = imm_min_func(X,t)
   rp * kslo * chi(t) * C_slo * imm_min_vec$phi_mn
 }

 # Flux from Passive SOM to Microbial SOM
 internal_fluxes[["10_to_8"]] = function(X,t) {
   C_pas <- X[10]
   imm_min_vec = imm_min_func(X,t)
   0.45 * kpas * chi(t) * C_pas * imm_min_vec$phi_mn
 }
 # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

 # Nitrogen fluxes ----
 # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Nitrogen Flux from Abovegr. Metabolic to Microbial SOM
internal_fluxes[["11_to_18"]] = function(X,t) {
   N_am  <- X[11]
   imm_min_vec = imm_min_func(X,t)
   kam * chi(t) * imm_min_vec$phi_mn * N_am
}

# Nitrogen Flux from Abovegr. Structural to Microbial SOM
internal_fluxes[["12_to_18"]] = function(X,t) {
   C_as  <- X[2]
   N_as  <- X[12]
   C_slo <- X[9]
   N_slo <- X[19]
   imm_min_vec = imm_min_func(X,t)
   kas * chi(t) * imm_min_vec$phi_mn * N_as * (1 - rslo * ( (C_as / N_as) / (C_slo / N_slo) ) )
}

# Nitrogen Flux from Abovegr. Structural to Slow SOM
internal_fluxes[["12_to_19"]] = function(X,t) {
   N_as  <- X[12]
   imm_min_vec = imm_min_func(X,t)
   kas * chi(t) * imm_min_vec$phi_mn * rslo * N_as
}

# Nitrogen Flux from Belowgr. Metabolic to Microbial SOM
internal_fluxes[["13_to_18"]] = function(X,t) {
   N_bm  <- X[13]
   imm_min_vec = imm_min_func(X,t)
   kbm * chi(t) * imm_min_vec$phi_mn * N_bm
}

# Nitrogen Flux from Belowgr. Structural to Microbial SOM
internal_fluxes[["14_to_18"]] = function(X,t) {
   C_bs  <- X[4]
   N_bs  <- X[14]
   C_slo <- X[9]
   N_slo <- X[19]
   imm_min_vec = imm_min_func(X,t)
   kbs * chi(t) * imm_min_vec$phi_mn * N_bs * (1 - rslo * ( (C_bs / N_bs) / (C_slo / N_slo) ) )
}

# Nitrogen Flux from Belowgr. Structural to Slow SOM
internal_fluxes[["14_to_19"]] = function(X,t) {
   N_bs  <- X[14]
   imm_min_vec = imm_min_func(X,t)
   kbs * chi(t) * imm_min_vec$phi_mn * rslo * N_bs
}

# Nitrogen Flux from Fine Wood to Microbial SOM
internal_fluxes[["15_to_18"]] = function(X,t) {
   C_fw  <- X[5]
   N_fw  <- X[15]
   C_slo <- X[9]
   N_slo <- X[19]
   imm_min_vec = imm_min_func(X,t)
   kfw * chi(t) * imm_min_vec$phi_mn * N_fw * (1 - rslo * ( (C_fw / N_fw) / (C_slo / N_slo) ) )
}

# Nitrogen Flux from Fine Wood to Slow SOM
internal_fluxes[["15_to_19"]] = function(X,t) {
   N_fw  <- X[15]
   imm_min_vec = imm_min_func(X,t)
   kfw * chi(t) * imm_min_vec$phi_mn * rslo * N_fw
}

# Nitrogen Flux from Abovegr. Coarse Wood to Microbial SOM
internal_fluxes[["16_to_18"]] = function(X,t) {
   C_acw  <- X[6]
   N_acw  <- X[16]
   C_slo <- X[9]
   N_slo <- X[19]
   imm_min_vec = imm_min_func(X,t)
   kacw * chi(t) * imm_min_vec$phi_mn * N_acw * (1 - rslo * ( (C_acw / N_acw) / (C_slo / N_slo) ) )
}

# Nitrogen Flux from Abovegr. Coarse Wood to Slow SOM
internal_fluxes[["16_to_19"]] = function(X,t) {
   N_acw  <- X[16]
   imm_min_vec = imm_min_func(X,t)
   kacw * chi(t) * imm_min_vec$phi_mn * rslo * N_acw
}

# Nitrogen Flux from Belowgr. Coarse Wood to Microbial SOM
internal_fluxes[["17_to_18"]] = function(X,t) {
   C_bcw  <- X[7]
   N_bcw  <- X[17]
   C_slo <- X[9]
   N_slo <- X[19]
   imm_min_vec = imm_min_func(X,t)
   kbcw * chi(t) * imm_min_vec$phi_mn * N_bcw * (1 - rslo * ( (C_bcw / N_bcw) / (C_slo / N_slo) ) )
}

# Nitrogen Flux from Belowgr. Coarse Wood to Slow SOM
internal_fluxes[["17_to_19"]] = function(X,t) {
   N_bcw  <- X[17]
   imm_min_vec = imm_min_func(X,t)
   kbcw * chi(t) * imm_min_vec$phi_mn * rslo * N_bcw
}

# Nitrogen Flux from Microbial SOM to Slow SOM
internal_fluxes[["18_to_19"]] = function(X,t) {
   C_mic <- X[8]
   N_mic <- X[18]
   C_pas <- X[10]
   N_pas <- X[20]
   imm_min_vec = imm_min_func(X,t)
   ( 1 - 0.004 * ( (C_mic / N_mic) / (C_pas / N_pas) ) ) * kmic * chi(t) * imm_min_vec$phi_mn * N_mic
}

# Nitrogen Flux from Microbial SOM to Passive SOM
internal_fluxes[["18_to_20"]] = function(X,t) {
   C_mic <- X[8]
   N_mic <- X[18]
   C_pas <- X[10]
   N_pas <- X[20]
   imm_min_vec = imm_min_func(X,t)
   0.004 * ( (C_mic / N_mic) / (C_pas / N_pas) ) * kmic * chi(t) * imm_min_vec$phi_mn * N_mic
}

 # Nitrogen Flux from Slow SOM to Microbial SOM
 internal_fluxes[["19_to_18"]] = function(X,t) {
    C_slo <- X[9]
    N_slo <- X[19]
    C_pas <- X[10]
    N_pas <- X[20]
    imm_min_vec = imm_min_func(X,t)
    (1 - rp * ( (C_slo / N_slo) / (C_pas / N_pas) ) ) * kslo * chi(t) * imm_min_vec$phi_mn * N_slo
 }

 # Nitrogen Flux from Slow SOM to Passive SOM
 internal_fluxes[["19_to_20"]] = function(X,t) {
    C_slo <- X[9]
    N_slo <- X[19]
    C_pas <- X[10]
    N_pas <- X[20]
    imm_min_vec = imm_min_func(X,t)
    rp * ( (C_slo / N_slo) / (C_pas / N_pas) ) * kslo * chi(t) * imm_min_vec$phi_mn * N_slo
 }

 # Nitrogen Flux from Passive SOM to Microbial SOM
 internal_fluxes[["20_to_18"]] = function(X,t) {
    C_pas <- X[10]
    N_pas <- X[20]
    imm_min_vec = imm_min_func(X,t)
    kpas * chi(t) * imm_min_vec$phi_mn * N_pas
 }


 # Three different cases to simulate mineralization and immobilization fluxes

 # Mineralization (case_1)
 internal_fluxes[["18_to_21"]] = function(X,t) {
    
    C_am   <- X[[1]]
    C_as	<- X[[2]]
    C_bm	<- X[[3]]
    C_bs	<- X[[4]]
    C_fw	<- X[[5]]
    C_acw  <- X[[6]]
    C_bcw  <- X[[7]]
    C_mic  <- X[[8]]
    C_slo  <- X[[9]]
    C_pas  <- X[[10]]
    N_am	<- X[[11]]
    N_as	<- X[[12]]
    N_bm	<- X[[13]]
    N_bs	<- X[[14]]
    N_fw	<- X[[15]]
    N_acw  <- X[[16]]
    N_bcw  <- X[[17]]
    N_mic  <- X[[18]]
    N_slo  <- X[[19]]
    N_pas  <- X[[20]]
    N_ino  <- X[[21]]
    
    imm_min_vec = imm_min_func(X,t)
    
    ifelse(imm_min_vec$term1 >= 0, imm_min_vec$phi, 0)
    
 }
 
 # Immobilization (case_2 and case_3)
 internal_fluxes[["21_to_18"]] = function(X,t) {
    
    C_am   <- X[[1]]
    C_as	<- X[[2]]
    C_bm	<- X[[3]]
    C_bs	<- X[[4]]
    C_fw	<- X[[5]]
    C_acw  <- X[[6]]
    C_bcw  <- X[[7]]
    C_mic  <- X[[8]]
    C_slo  <- X[[9]]
    C_pas  <- X[[10]]
    N_am	<- X[[11]]
    N_as	<- X[[12]]
    N_bm	<- X[[13]]
    N_bs	<- X[[14]]
    N_fw	<- X[[15]]
    N_acw  <- X[[16]]
    N_bcw  <- X[[17]]
    N_mic  <- X[[18]]
    N_slo  <- X[[19]]
    N_pas  <- X[[20]]
    N_ino  <- X[[21]]
    
    imm_min_vec = imm_min_func(X,t)
    
    # Immobilization (without restrictions) (case_2)
    ifelse(imm_min_vec$term1 < 0 & abs(imm_min_vec$phi) <= imm_min_vec$imm_max,
           
           -imm_min_vec$phi,
           
           ifelse(imm_min_vec$term1 < 0 & abs(imm_min_vec$phi) > imm_min_vec$imm_max,
                  # Immobilization (with restrictions) (case_3)
                  imm_min_vec$imm_max, 0)
           
    )
    
 }
 
 
 

#  # ----
#  internal_fluxes[["3_to_2"]] = function(C,t) {2}
#  out_fluxes = list()
#  out_fluxes[["1"]] = function(X,t) {
#  }
#
term1<-function(
  C_am
  ,N_am
  ,C_mic
  ,N_mic
  ,N_as
  ,C_as
  ,C_slo
  ,N_slo
  ,C_bm
  ,N_bm
  ,C_bs
  ,N_bs
  ,C_fw
  ,N_fw
  ,C_acw
  ,N_acw
  ,C_bcw
  ,N_bcw
  ,C_pas
  ,N_pas
){  
  term=  kam * N_am * (1 - 0.4 * ((C_am / N_am) / (C_mic / N_mic)) )
+
     kas * N_as * ( (1 - (0.4 * rmic * ((C_as / N_as) / (C_mic / N_mic))) - (rslo * ((C_as / N_as) / (C_slo / N_slo)))))
    +
     kbm * N_bm * (1 - 0.45 * ((C_bm / N_bm) / (C_mic / N_mic)) )                                                               +
     kbs * N_bs * ( (1 - (0.45 * rmic * ((C_bs / N_bs) / (C_mic / N_mic))) - (rslo * ((C_bs / N_bs) / (C_slo / N_slo)))))       +
     kfw * N_fw * ( (1 - (0.45 * rmic * ((C_fw / N_fw) / (C_mic / N_mic))) - (rslo * ((C_fw / N_fw) / (C_slo / N_slo)))))       +
     kacw * N_acw * ( (1 - (0.45 * rmic * ((C_acw / N_acw) / (C_mic / N_mic))) - (rslo * ((C_acw / N_acw) / (C_slo / N_slo))))) +
     kbcw * N_bcw * ( (1 - (0.45 * rmic * ((C_bcw / N_bcw) / (C_mic / N_mic))) - (rslo * ((C_bcw / N_bcw) / (C_slo / N_slo))))) +
     kslo * N_slo * ( (1 - (rsm * ((C_slo / N_slo) / (C_mic / N_mic))) - (rp * ((C_slo / N_slo) / (C_slo / N_slo)))))           +
     kpas * N_pas * (1 - 0.45 * ((C_pas / N_pas) / (C_mic / N_mic)) )
   
   term
}
internal_fluxes_by_PoolName=list()
internal_fluxes_by_PoolName[["C_am->C_mic"]] = function(
  C_am
  ,N_am
  ,C_mic
  ,N_mic
  ,N_as
  ,C_as
  ,C_slo
  ,N_slo
  ,C_bm
  ,N_bm
  ,C_bs
  ,N_bs
  ,C_fw
  ,N_fw
  ,C_acw
  ,N_acw
  ,C_bcw
  ,N_bcw
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
){  
  phi_mn=phi_mn_scalar(
    C_am
    ,N_am
    ,C_mic
    ,N_mic
    ,N_as
    ,C_as
    ,C_slo
    ,N_slo
    ,C_bm
    ,N_bm
    ,C_bs
    ,N_bs
    ,C_fw
    ,N_fw
    ,C_acw
    ,N_acw
    ,C_bcw
    ,N_bcw
    ,C_pas
    ,N_pas
    ,N_ino
    ,t=0
  )
  # Initial value of phi_mn
  0.4 * kam * chi(t) * C_am * phi_mn
}
 

internal_fluxes_by_PoolName[["C_as->C_mic"]] = function(
  C_am
  ,N_am
  ,C_mic
  ,N_mic
  ,N_as
  ,C_as
  ,C_slo
  ,N_slo
  ,C_bm
  ,N_bm
  ,C_bs
  ,N_bs
  ,C_fw
  ,N_fw
  ,C_acw
  ,N_acw
  ,C_bcw
  ,N_bcw
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
){  
  # Initial value of phi_mn
  phi_mn=phi_mn_scalar(
    C_am
    ,N_am
    ,C_mic
    ,N_mic
    ,N_as
    ,C_as
    ,C_slo
    ,N_slo
    ,C_bm
    ,N_bm
    ,C_bs
    ,N_bs
    ,C_fw
    ,N_fw
    ,C_acw
    ,N_acw
    ,C_bcw
    ,N_bcw
    ,C_pas
    ,N_pas
    ,t
  )
  0.4 * kam * chi(t) * C_am * phi_mn
  
}
