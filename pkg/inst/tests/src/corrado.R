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

# Call functions to compute term1, imm_max, phi_mn, phi
source("functions_corrado.R")


# Internal fluxes
internal_fluxes = list()

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Carbon fluxes # ----
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Flux from Abovegr. Metabolic to Microbial SOM
internal_fluxes[["C_am_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.4 * kam * xi(t) * C_am * phi_mn
}

# Flux from Abovegr. Structural to Microbial SOM
internal_fluxes[["C_as_to_C_mic"]] = function( 
                                               C_am
                                               ,N_am
                                               ,C_as
                                               ,N_as
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
                                               ,C_mic
                                               ,N_mic
                                               ,C_slo
                                               ,N_slo
                                               ,C_pas
                                               ,N_pas
                                               ,N_ino
                                               ,t
                                               
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.4 * rmic * kas * xi(t) * C_as * phi_mn
}

# Flux from Abovegr. Structural to Slow SOM
internal_fluxes[["C_as_to_C_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.7 * rslo * kas * xi(t) * C_as * phi_mn
}

# Flux from Belowgr. Metabolic to Microbial SOM
internal_fluxes[["C_bm_to_C_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.45 * kbm * xi(t) * C_bm * phi_mn
}

# Flux from Belowgr. Structural to Microbial SOM
internal_fluxes[["C_bs_to_C_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.45 * rmic * kbs * xi(t) * C_bs * phi_mn
}

# Flux from Belowgr. Structural to Slow SOM
internal_fluxes[["C_bs_to_C_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.7 * rslo * kbs * xi(t) * C_bs * phi_mn
}

# Flux from Fine Wood to Microbial SOM
internal_fluxes[["C_fw_to_C_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.45 * rmic * kfw * xi(t) * C_fw * phi_mn
}

# Flux from Fine Wood to Slow SOM
internal_fluxes[["C_fw_to_C_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.7 * rslo * kfw * xi(t) * C_fw * phi_mn
}

# Flux from Abovegr. Coarse Wood to Microbial SOM
internal_fluxes[["C_acw_to_C_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.45 * rmic * kacw * xi(t) * C_acw * phi_mn
}

# Flux from Abovegr. Coarse Wood to Slow SOM
internal_fluxes[["C_acw_to_C_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.7 * rslo * kacw * xi(t) * C_acw * phi_mn
}

# Flux from Belowgr. Coarse Wood to Microbial SOM
internal_fluxes[["C_bcw_to_C_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.45 * rmic * kbcw * xi(t) * C_bcw * phi_mn
}

# Flux from Belowgr. Coarse Wood to Slow SOM
internal_fluxes[["C_bcw_to_C_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.7 * rslo * kbcw * xi(t) * C_bcw * phi_mn
}

# Flux from Microbial SOM to Slow SOM
internal_fluxes[["C_mic_to_C_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  rms * kmic * xi(t) * C_mic * phi_mn
}

# Flux from Microbial SOM to Passive SOM
internal_fluxes[["C_mic_to_C_pas"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.004 * kmic * xi(t) * C_mic * phi_mn
}

# Flux from Slow SOM to Microbial SOM
internal_fluxes[["C_slo_to_C_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  rsm * kslo * xi(t) * C_slo * phi_mn
}

# Flux from Slow SOM to Passive SOM
internal_fluxes[["C_slo_to_C_pas"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  rp * kslo * xi(t) * C_slo * phi_mn
}

# Flux from Passive SOM to Microbial SOM
internal_fluxes[["C_pas_to_C_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.45 * kpas * xi(t) * C_pas * phi_mn
}


# *-*
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Nitrogen fluxes ----
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# *-*
# Nitrogen Flux from Abovegr. Metabolic to Microbial SOM
internal_fluxes[["N_am_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  kam * xi(t) * N_am * phi_mn
}

# Nitrogen Flux from Abovegr. Structural to Microbial SOM
internal_fluxes[["N_as_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  (1 - rslo * ( (C_as / N_as) / (C_slo / N_slo) ) ) * kas * xi(t) * N_as * phi_mn
}

# Nitrogen Flux from Abovegr. Structural to Slow SOM
internal_fluxes[["N_as_to_N_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  kas * xi(t) * rslo * N_as * phi_mn
}

# Nitrogen Flux from Belowgr. Metabolic to Microbial SOM
internal_fluxes[["N_bm_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  kbm * xi(t) * N_bm * phi_mn
}

# Nitrogen Flux from Belowgr. Structural to Microbial SOM
internal_fluxes[["N_bs_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  (1 - rslo * ( (C_bs / N_bs) / (C_slo / N_slo) ) ) * kbs * xi(t) * N_bs* phi_mn
}

# Nitrogen Flux from Belowgr. Structural to Slow SOM
internal_fluxes[["N_bs_to_N_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  kbs * xi(t) * rslo * N_bs * phi_mn
}

# Nitrogen Flux from Fine Wood to Microbial SOM
internal_fluxes[["N_fw_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  (1 - rslo * ( (C_fw / N_fw) / (C_slo / N_slo) ) ) * kfw * xi(t) * N_fw * phi_mn
}

# Nitrogen Flux from Fine Wood to Slow SOM
internal_fluxes[["N_fw_to_N_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  kfw * xi(t) * rslo * N_fw * phi_mn
}

# Nitrogen Flux from Abovegr. Coarse Wood to Microbial SOM
internal_fluxes[["N_acw_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  (1 - rslo * ( (C_acw / N_acw) / (C_slo / N_slo) ) ) * kacw * xi(t) * N_acw * phi_mn
}
 
# Nitrogen Flux from Abovegr. Coarse Wood to Slow SOM
internal_fluxes[["N_acw_to_N_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  kacw * xi(t) * rslo * N_acw * phi_mn
}

# Nitrogen Flux from Belowgr. Coarse Wood to Microbial SOM
internal_fluxes[["N_bcw_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  (1 - rslo * ( (C_bcw / N_bcw) / (C_slo / N_slo) ) ) * kbcw * xi(t) * N_bcw * phi_mn
}

# Nitrogen Flux from Belowgr. Coarse Wood to Slow SOM
internal_fluxes[["N_bcw_to_N_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  kbcw * xi(t) * rslo * N_bcw * phi_mn
}

# Nitrogen Flux from Microbial SOM to Slow SOM
internal_fluxes[["N_mic_to_N_slo"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  ( 1 - 0.004 * ( (C_mic / N_mic) / (C_pas / N_pas) ) ) * kmic * xi(t) * N_mic * phi_mn
}

# Nitrogen Flux from Microbial SOM to Passive SOM
internal_fluxes[["N_mic_to_N_pas"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  0.004 * ( (C_mic / N_mic) / (C_pas / N_pas) ) * kmic * xi(t) * N_mic * phi_mn
}

# Nitrogen Flux from Slow SOM to Microbial SOM
internal_fluxes[["N_slo_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  (1 - rp * ( (C_slo / N_slo) / (C_pas / N_pas) ) ) * kslo * xi(t) * N_slo * phi_mn
}

# Nitrogen Flux from Slow SOM to Passive SOM
internal_fluxes[["N_slo_to_N_pas"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  rp * ( (C_slo / N_slo) / (C_pas / N_pas) ) * kslo * xi(t) * N_slo * phi_mn
}

# Nitrogen Flux from Passive SOM to Microbial SOM
internal_fluxes[["N_pas_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  phi_mn = phi_mn_scalar(
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
  )
  
  kpas * xi(t) * N_pas * phi_mn
}

# Three different cases to simulate mineralization and immobilization fluxes
# Mineralization (case_1)
internal_fluxes[["N_mic_to_N_ino"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  term1 = term1(
    C_am
    ,N_am
    ,C_as
    ,N_as
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
    ,C_mic
    ,N_mic
    ,C_slo
    ,N_slo
    ,C_pas
    ,N_pas
    ,N_ino
  )
  
  phi = phi(
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
  )
  
  ifelse(term1 >= 0, phi, 0)
}
# Immobilization (case_2 and case_3)
internal_fluxes[["N_ino_to_N_mic"]] = function( 
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  term1 = term1(
    C_am
    ,N_am
    ,C_as
    ,N_as
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
    ,C_mic
    ,N_mic
    ,C_slo
    ,N_slo
    ,C_pas
    ,N_pas
    ,N_ino
  )
  
  imm_max = imm_max(
    C_am
    ,N_am
    ,C_as
    ,N_as
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
    ,C_mic
    ,N_mic
    ,C_slo
    ,N_slo
    ,C_pas
    ,N_pas
    ,N_ino
    ,t
  )
  
  phi = phi(
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
  )
  
  # Immobilization (without restrictions) (case_2)
  ifelse(term1 < 0 & abs(phi) <= imm_max,
         
         -phi,
         
         ifelse(term1 < 0 & abs(phi) > imm_max,
                # Immobilization (with restrictions) (case_3)
                imm_max, 0)
         
  )
}





# Outfluxes ----
# internal_fluxes[["3_to_2"]] = function(C,t) {2}

out_fluxes = list()

# Flux from Abovegr. Metabolic to Microbial SOM
out_fluxes[["C_am_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.6 * kam * xi(t) * C_am * phi_mn
}

# Flux from Abovegr. Structural to Microbial SOM
out_fluxes[["C_as_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.6 * rmic * kas * xi(t) * C_as * phi_mn
}

# Flux from Abovegr. Structural to Slow SOM
out_fluxes[["C_as_to_C_slo"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.3 * rslo * kas * xi(t) * C_as * phi_mn
}
 
# Flux from Belowgr. Metabolic to Microbial SOM
out_fluxes[["C_bm_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.55 * kbm * xi(t) * C_bm * phi_mn
}
 
# Flux from Belowgr. Structural to Microbial SOM
out_fluxes[["C_bs_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.55 * rmic * kbs * xi(t) * C_bs * phi_mn
}
 
# Flux from Belowgr. Structural to Slow SOM
out_fluxes[["C_bs_to_C_slo"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.3 * rslo * kbs * xi(t) * C_bs * phi_mn
}
 
# Flux from Fine Wood to Microbial SOM
out_fluxes[["C_fw_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.55 * rmic * kfw * xi(t) * C_fw * phi_mn
}
 
# Flux from Fine Wood to Slow SOM
out_fluxes[["C_fw_to_C_slo"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.3 * rslo * kfw * xi(t) * C_fw * phi_mn
}
 
# Flux from Abovegr. Coarse Wood to Microbial SOM
out_fluxes[["C_acw_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.55 * rmic * kacw * xi(t) * C_acw * phi_mn
}
 
# Flux from Abovegr. Coarse Wood to Slow SOM
out_fluxes[["C_acw_to_C_slo"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.3 * rslo * kacw * xi(t) * C_acw * phi_mn
}
 
# Flux from Belowgr. Coarse Wood to Microbial SOM
out_fluxes[["C_bcw_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.55 * rmic * kbcw * xi(t) * C_bcw * phi_mn
}
 
# Flux from Belowgr. Coarse Wood to Slow SOM
out_fluxes[["C_bcw_to_C_slo"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.3 * rslo * kbcw * xi(t) * C_bcw * phi_mn
} 

# Flux from Microbial SOM to Slow SOM and Passive SOM
out_fluxes[["C_mic_to_C_slo_to_C_pas"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  ((0.85 - 0.68) * (clay_frac + silt_frac)) * kmic * xi(t) * C_mic * phi_mn
} 

# Flux from Slow SOM to Microbial SOM and Passive SOM
out_fluxes[["C_slo_to_C_mic_to_C_pas"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.55 * kslo * xi(t) * C_slo * phi_mn
} 

# Flux from Passive SOM to Microbial SOM
out_fluxes[["C_pas_to_C_mic"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  phi_mn = phi_mn_scalar(
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
  )
  0.55 * kpas * xi(t) * C_pas * phi_mn
} 

# Flux from Inorganic Nitrogen (Plant Uptake)
out_fluxes[["plant_uptake"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {

  kup * N_ino
} 



 
 
 

# Input fluxes ----
# 1. Constant (Carbon fluxes) ----
input_fluxes_const = list()

# Input C flux into Abovegr. Metabolic Lit
input_fluxes_const[["C_to_C_am"]] = function() {
  # 0.5
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# 2. Dependent on time (Carbon fluxes) ----
input_fluxes_time = list()

# I have a data.frame with Carbon input for each litter and CWD pool
# InFluxC (a data.frame):
#             times | C_in_C_am | C_in_C_as | C_in_C_bm | C_in_C_bs | C_in_C_fw | ... | ... 
#               0   |     0.1   |     0.2   |    0.1    |    ...    |    ...
#               1   |     0.2   |     0.3   |    0.1    |    ...    |    ...

# Input C flux into Abovegr. Metabolic Lit
input_fluxes_time[["C_to_C_am"]] = function(t) {
  # InFunc_C = approxfun(x = InFluxC$times, y = InFluxC$C_in_C_am)
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# 3. Dependent on time and state (Nitrogen fluxes) ----
input_fluxes_time_state = list()

# InFluxN (a data.frame):
#             times | N_in_N_am | N_in_N_as | N_in_N_bm | N_in_N_bs | N_in_N_fw | ... | ... 
#               0   |     0.1   |     0.2   |    0.1    |    ...    |    ...
#               1   |     0.2   |     0.3   |    0.1    |    ...    |    ...

# Input N flux into Abovegr. Metabolic Lit
input_fluxes_time_state[["N_to_N_am"]] = function(
  C_am
  ,N_am
  ,C_as
  ,N_as
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
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  # InFunc_N = approxfun(x = InFluxN$times, y = InFluxN$N_in_N_am)
  # InFunc_N + 0.5 * kup * N_ino
}




