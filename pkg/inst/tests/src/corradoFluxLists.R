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

# Call functions to simulate CN fluxes
source("functions_corrado.R")


# Internal fluxes
internal_fluxes = list()

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Carbon fluxes # ----
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Flux from Abovegr. Metabolic to Surface Microbial SOM
internal_fluxes[["C_am->C_srfmic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # C:N ratio of new material entering Microbial SOM
  c_cont = C_am
  n_cont = N_am
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_am * xi(t) * C_am

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_am
                  ,tcflow
                  ,C_am
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_am_to_c_srfmic = ifelse(C_am > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2am) * tcflow
                                    ,0
                            )
                            ,0
  )

}

# Flux from Abovegr. Structural to Surface Microbial SOM
internal_fluxes[["C_as->C_srfmic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
) {
  
  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )
  
  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_c_srfmic = ifelse(C_as > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2as1) * (1 - ligfr_as) * tcflow
                                    ,0
                            )
                            ,0
  )

}

# Flux from Abovegr. Structural to Slow SOM
internal_fluxes[["C_as->C_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- rnewas(c_cont
                       ,n_cont
                       ,rad1p1
                       ,rad1p2
                       ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)
  
  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )
  
  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_c_slo = ifelse(C_as > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2as2) * ligfr_as * tcflow
                                 ,0
                         )
                         ,0
  )
  
}

# Flux from Belowgr. Metabolic to Microbial SOM
internal_fluxes[["C_bm->C_mic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # Compute the C:N ratio of new material entering Microbial SOM
  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )


  # Verify if decomposition can occur (Nitrogen limitations)
  c_cont = C_bm
  n_cont = N_bm
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bm * xi(t) * C_bm * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bm
                  ,tcflow
                  ,C_bm
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bm_to_c_mic = ifelse(C_bm > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bm) * tcflow
                                 ,0
                         )
                         ,0
  )

}

# Flux from Belowgr. Structural to Microbial SOM
internal_fluxes[["C_bs->C_mic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # Verify if decomposition can occur (Nitrogen limitations)
  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_1
  
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_c_mic = ifelse(C_bs > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bs1) * (1 - ligfr_bs) * tcflow
                                 ,0
                         )
                         ,0
  )

}

# Flux from Belowgr. Structural to Slow SOM
internal_fluxes[["C_bs->C_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # Verify if decomposition can occur (Nitrogen limitations)
  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_2
  
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_c_slo = ifelse(C_bs > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bs2) * ligfr_bs * tcflow
                                 ,0
                         )
                         ,0
  )

}

# Flux from Fine Wood to Surface Microbial SOM
internal_fluxes[["C_fw->C_srfmic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- agdrat(c_cont
                     ,n_cont
                     ,biocnv_wood
                     ,cemicb
                     ,pcemic1
                     ,pcemic2
                     ,pcemic3)
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_c_srfmic = ifelse(C_fw > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2fw1) * (1 - ligfr_fw) * tcflow
                                    ,0
                            )
                            ,0
  )

}

# Flux from Fine Wood to Slow SOM
internal_fluxes[["C_fw->C_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- rneww1_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )
  
  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_c_slo = ifelse(C_fw > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2fw2) * ligfr_fw * tcflow
                                 ,0
                         )
                         ,0
  )

}

# Flux from Abovegr. Coarse Wood to Surface Microbial SOM
internal_fluxes[["C_acw->C_srfmic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )
  
  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_c_srfmic = ifelse(C_acw > 0
                             ,ifelse(cando == TRUE
                                     ,(1 - co2acw1) * (1 - ligfr_acw) * tcflow
                                     ,0
                             )
                             ,0
  )

}

# Flux from Abovegr. Coarse Wood to Slow SOM
internal_fluxes[["C_acw->C_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- rneww2_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_c_slo = ifelse(C_acw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2acw2) * ligfr_acw * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Belowgr. Coarse Wood to Microbial SOM
internal_fluxes[["C_bcw->C_mic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  
  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat1_1
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_c_mic = ifelse(C_bcw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2bcw1) * (1 - ligfr_bcw) * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Belowgr. Coarse Wood to Slow SOM
internal_fluxes[["C_bcw->C_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat2_2
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_c_slo = ifelse(C_bcw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2bcw2) * ligfr_bcw * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Surface Microbial SOM to Slow SOM
internal_fluxes[["C_srfmic->C_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  c_cont = C_srfmic
  n_cont = N_srfmic
  
  radds1 = rad1p1 + rad1p2 * ((c_cont / n_cont) - pcemic2)
  cn_rat_new = c_cont / n_cont + radds1 # rceto2 in CENTURY4
  cn_rat_new = max(cn_rat_new, rad1p3)
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_srfmic * xi(t) * C_srfmic
  
  c_srfmic_to_c_slo = ifelse(C_srfmic > 0
                          ,ifelse(cando == TRUE
                                  ,frsrfmicslo * tcflow
                                  ,0
                          )
                          ,0
  )
  
}

# Flux from Microbial SOM to Slow SOM
internal_fluxes[["C_mic->C_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_mic
  n_cont = N_mic
  
  varatx_1 = varat2_1
  varatx_2 = varat2_2
  varatx_3 = varat2_3
  cn_rat_new <- bgdrat(N_ino
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_c_slo = ifelse(C_mic > 0
                          ,ifelse(cando == TRUE
                                  ,frmicslo * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Microbial SOM to Passive SOM
internal_fluxes[["C_mic->C_pas"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_mic
  n_cont = N_mic
  
  varatx_1 = varat3_1
  varatx_2 = varat3_2
  varatx_3 = varat3_3
  cn_rat_new = bgdrat(N_ino # rceto3 in CENTURY4
                      ,varatx_1
                      ,varatx_2
                      ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_c_pas = ifelse(C_mic > 0
                          ,ifelse(cando == TRUE
                                  ,frmicpas * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Slow SOM to Microbial SOM
internal_fluxes[["C_slo->C_mic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  
  c_cont = C_slo
  n_cont = N_slo

  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1 in CENTURY4
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb
  
  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_c_mic = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,frslomic * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Slow SOM to Passive SOM
internal_fluxes[["C_slo->C_pas"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  
  c_cont = C_slo
  n_cont = N_slo

  varatx_1 = varat3_1
  varatx_2 = varat3_2
  varatx_3 = varat3_3
  cn_rat_new <- bgdrat(N_ino # rceto in CENTURY4
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_c_pas = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,frslopas * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Passive SOM to Microbial SOM
internal_fluxes[["C_pas->C_mic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_slo
  n_cont = N_slo
  
  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1 in CENTURY4
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_pas * xi(t) * C_pas * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_pas_to_c_mic = ifelse(C_pas > 0
                          ,ifelse(cando == TRUE
                                  ,frpasmic * tcflow
                                  ,0
                          )
                          ,0
  )

}


# # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Nitrogen fluxes ----
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Nitrogen Flux from Abovegr. Metabolic to Surface Microbial SOM
internal_fluxes[["N_am->N_srfmic"]] = function(
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

  # Compute the C:N ratio of new material entering in Microbial SOM
  c_cont = C_am
  n_cont = N_am
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_am * xi(t) * C_am

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_am
                  ,tcflow
                  ,C_am
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_am_to_c_srfmic = ifelse(C_am > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2am) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_am_to_c_srfmic
  outofa = c_am_to_c_srfmic / (C_am / N_am)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_am_to_n_srfmic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Abovegr. Structural to Surface Microbial SOM
internal_fluxes[["N_as->N_srfmic"]] = function(
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

  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_c_srfmic = ifelse(C_as > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2as1) * (1 - ligfr_as) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_as_to_c_srfmic
  outofa = c_as_to_c_srfmic / (C_as / N_as)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_as_to_n_srfmic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Abovegr. Structural to Slow SOM
internal_fluxes[["N_as->N_slo"]] = function(
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

  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- rnewas(c_cont
                       ,n_cont
                       ,rad1p1
                       ,rad1p2
                       ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_c_slo = ifelse(C_as > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2as2) * ligfr_as * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_as_to_c_slo
  outofa = c_as_to_c_slo / (C_as / N_as)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_as_to_n_mic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Belowgr. Metabolic to Microbial SOM
internal_fluxes[["N_bm->N_mic"]] = function(
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

  c_cont = C_bm
  n_cont = N_bm

  # C:N ratio of new material entering Microbial SOM
  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bm * xi(t) * C_bm * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bm
                  ,tcflow
                  ,C_bm
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bm_to_c_mic = ifelse(C_bm > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bm) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bm_to_c_mic
  outofa = c_bm_to_c_mic / (C_bm / N_bm)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_bm_to_n_mic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Belowgr. Structural to Microbial SOM
internal_fluxes[["N_bs->N_mic"]] = function(
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

  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_1

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_c_mic = ifelse(C_bs > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bs1) * (1 - ligfr_bs) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bs_to_c_mic
  outofa = c_bs_to_c_mic / (C_bs / N_bs)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_bs_to_n_mic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Belowgr. Structural to Slow SOM
internal_fluxes[["N_bs->N_slo"]] = function(
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

  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_2

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_c_slo = ifelse(C_bs > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bs2) * ligfr_bs * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bs_to_c_slo
  outofa = c_bs_to_c_slo / (C_bs / N_bs)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_bs_to_n_slo <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Fine Wood to Surface Microbial SOM
internal_fluxes[["N_fw->N_srfmic"]] = function(
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

  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_c_srfmic = ifelse(C_fw > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2fw1) * (1 - ligfr_fw) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_fw_to_c_srfmic
  outofa = c_fw_to_c_srfmic / (C_fw / N_fw)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_fw_to_n_srfmic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Fine Wood to Slow SOM
internal_fluxes[["N_fw->N_slo"]] = function(
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

  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- rneww1_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_c_slo = ifelse(C_fw > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2fw2) * ligfr_fw * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_fw_to_c_slo
  outofa = c_fw_to_c_slo / (C_fw / N_fw)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_fw_to_n_slo <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Abovegr. Coarse Wood to Surface Microbial SOM
internal_fluxes[["N_acw->N_srfmic"]] = function(
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

  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_c_srfmic = ifelse(C_acw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2acw1) * (1 - ligfr_acw) * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_acw_to_c_srfmic
  outofa = c_acw_to_c_srfmic / (C_acw / N_acw)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_acw_to_n_srfmic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Abovegr. Coarse Wood to Slow SOM
internal_fluxes[["N_acw->N_slo"]] = function(
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

  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- rneww2_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_c_slo = ifelse(C_acw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2acw2) * ligfr_acw * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_acw_to_c_slo
  outofa = c_acw_to_c_slo / (C_acw / N_acw)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_acw_to_n_slo <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Belowgr. Coarse Wood to Microbial SOM
internal_fluxes[["N_bcw->N_mic"]] = function(
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

  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat1_1

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_c_mic = ifelse(C_bcw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2bcw1) * (1 - ligfr_bcw) * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_bcw_to_c_mic
  outofa = c_bcw_to_c_mic / (C_bcw / N_bcw)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_bcw_to_n_mic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Belowgr. Coarse Wood to Slow SOM
internal_fluxes[["N_bcw->N_slo"]] = function(
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

  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat2_2

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_c_slo = ifelse(C_bcw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2bcw2) * ligfr_bcw * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_bcw_to_c_slo
  outofa = c_bcw_to_c_slo / (C_bcw / N_bcw)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_bcw_to_n_slo <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Surface Microbial SOM to Slow SOM
internal_fluxes[["N_srfmic->N_slo"]] = function(
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

  c_cont = C_srfmic
  n_cont = N_srfmic

  radds1 = rad1p1 + rad1p2 * ((c_cont / n_cont) - pcemic2)
  cn_rat_new = (c_cont / n_cont) + radds1 # rceto2
  cn_rat_new = max(cn_rat_new, rad1p3)

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_srfmic * xi(t) * C_srfmic

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_srfmic_to_c_slo = ifelse(C_srfmic > 0
                          ,ifelse(cando == TRUE
                                  ,frsrfmicslo * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_srfmic_to_c_slo
  outofa = c_srfmic_to_c_slo / (C_srfmic / N_srfmic)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_srfmic_to_n_slo <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Microbial SOM to Slow SOM
internal_fluxes[["N_mic->N_slo"]] = function(
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

  c_cont = C_mic
  n_cont = N_mic

  varatx_1 = varat2_1
  varatx_2 = varat2_2
  varatx_3 = varat2_3
  cn_rat_new <- bgdrat(N_ino # rceto2
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_c_slo = ifelse(C_mic > 0
                          ,ifelse(cando == TRUE
                                  ,frmicslo * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_mic_to_c_slo
  outofa = c_mic_to_c_slo / (C_mic / N_mic)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_mic_to_n_slo <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Microbial SOM to Passive SOM
internal_fluxes[["N_mic->N_pas"]] = function(
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

  c_cont = C_mic
  n_cont = N_mic

  varatx_1 = varat3_1
  varatx_2 = varat3_2
  varatx_3 = varat3_3
  cn_rat_new <- bgdrat(N_ino # rceto3
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_c_pas = ifelse(C_mic > 0
                          ,ifelse(cando == TRUE
                                  ,frmicpas * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_mic_to_c_pas
  outofa = c_mic_to_c_pas / (C_mic / N_mic)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_mic_to_n_pas <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Slow SOM to Microbial SOM
internal_fluxes[["N_slo->N_mic"]] = function(
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

  c_cont = C_slo
  n_cont = N_slo

  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_c_mic = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,frslomic * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_slo_to_c_mic
  outofa = c_slo_to_c_mic / (C_slo / N_slo)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_slo_to_n_mic <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Slow SOM to Passive SOM
internal_fluxes[["N_slo->N_pas"]] = function(
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

  c_cont = C_slo
  n_cont = N_slo

  varatx_1 = varat3_1
  varatx_2 = varat3_2
  varatx_3 = varat3_3
  cn_rat_new <- bgdrat(N_ino # rceto
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_c_pas = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,frslopas * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_slo_to_c_pas
  outofa = c_slo_to_c_pas / (C_slo / N_slo)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_slo_to_n_pas <- esched(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Passive SOM to Microbial SOM
internal_fluxes[["N_pas->N_mic"]] = function(
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

  c_cont = C_pas
  n_cont = N_pas

  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_pas * xi(t) * C_pas * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_pas_to_c_mic = ifelse(C_pas > 0
                          ,ifelse(cando == TRUE
                                  ,frpasmic * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_pas_to_c_mic
  outofa = c_pas_to_c_mic / (C_pas / N_pas)

  # Transfer Nitrogen flux if N lost from the pool is positive
  n_pas_to_n_mic <- esched(c_transf, outofa, cn_rat_new)

}


# Mineralization fluxes ----

# Nitrogen Flux from Abovegr. Metabolic to Inorganic Nitrogen
internal_fluxes[["N_am->N_ino"]] = function(
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

  # Compute the C:N ratio of new material entering in Microbial SOM
  c_cont = C_am
  n_cont = N_am
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_am * xi(t) * C_am

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_am
                  ,tcflow
                  ,C_am
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_am_to_c_srfmic = ifelse(C_am > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2am) * tcflow
                                    ,0
                            )
                            ,0
  )

  c_transf = c_am_to_c_srfmic
  outofa = c_am_to_c_srfmic / (C_am / N_am)

  # Mineralized Nitrogen
  n_am_to_n_ino <- mineral_n(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Abovegr. Structural to Inorganic Nitrogen
internal_fluxes[["N_as->N_ino"]] = function(
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

  # AS --> SRF MIC
  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_c_srfmic = ifelse(C_as > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2as1) * (1 - ligfr_as) * tcflow
                                    ,0
                            )
                            ,0
  )

  c_transf = c_as_to_c_srfmic
  outofa = c_as_to_c_srfmic / (C_as / N_as)

  # Mineralized Nitrogen
  n_as_to_n_ino_1 <- mineral_n(c_transf, outofa, cn_rat_new)

  # AS --> SLO
  # ***************
  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- rnewas(c_cont
                       ,n_cont
                       ,rad1p1
                       ,rad1p2
                       ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_c_slo = ifelse(C_as > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2as2) * ligfr_as * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_as_to_c_slo
  outofa = c_as_to_c_slo / (C_as / N_as)

  # Mineralized Nitrogen
  n_as_to_n_ino_2 <- mineral_n(c_transf, outofa, cn_rat_new)

  # Total Mineralized Nitrogen
  n_as_to_n_ino = n_as_to_n_ino_1 + n_as_to_n_ino_2

}

# Nitrogen Flux from Belowgr. Metabolic to Inorganic Nitrogen
internal_fluxes[["N_bm->N_ino"]] = function(
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

  c_cont = C_bm
  n_cont = N_bm

  # C:N ratio of new material entering Microbial SOM
  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bm * xi(t) * C_bm * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bm
                  ,tcflow
                  ,C_bm
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bm_to_c_mic = ifelse(C_bm > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bm) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bm_to_c_mic
  outofa = c_bm_to_c_mic / (C_bm / N_bm)

  # Mineralized Nitrogen
  n_bm_to_n_ino <- mineral_n(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Belowgr. Structural to Inorganic Nitrogen
internal_fluxes[["N_bs->N_ino"]] = function(
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

  # BS --> MIC
  # ***************
  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_1

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_c_mic = ifelse(C_bs > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bs1) * (1 - ligfr_bs) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bs_to_c_mic
  outofa = c_bs_to_c_mic / (C_bs / N_bs)

  # Mineralized Nitrogen
  n_bs_to_n_ino_1 <- mineral_n(c_transf, outofa, cn_rat_new)

  # BS --> SLO
  # ***************
  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_2

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_c_slo = ifelse(C_bs > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bs2) * ligfr_bs * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bs_to_c_slo
  outofa = c_bs_to_c_slo / (C_bs / N_bs)

  # Mineralized Nitrogen
  n_bs_to_n_ino_2 <- mineral_n(c_transf, outofa, cn_rat_new)

  # Total Mineralized Nitrogen
  n_bs_to_n_ino = n_bs_to_n_ino_1 + n_bs_to_n_ino_2

}

# Nitrogen Flux from Fine Wood to Inorganic Nitrogen
internal_fluxes[["N_fw->N_ino"]] = function(
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

  # FW --> SRF MIC
  # ***************
  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_c_srfmic = ifelse(C_fw > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2fw1) * (1 - ligfr_fw) * tcflow
                                    ,0
                            )
                            ,0
  )

  c_transf = c_fw_to_c_srfmic
  outofa = c_fw_to_c_srfmic / (C_fw / N_fw)

  # Mineralized Nitrogen
  n_fw_to_n_ino_1 <- mineral_n(c_transf, outofa, cn_rat_new)

  # FW --> SLO
  # ***************
  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- rneww1_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_c_slo = ifelse(C_fw > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2fw2) * ligfr_fw * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_fw_to_c_slo
  outofa = c_fw_to_c_slo / (C_fw / N_fw)

  # Mineralized Nitrogen
  n_fw_to_n_ino_2 <- mineral_n(c_transf, outofa, cn_rat_new)

  # Total Mineralized Nitrogen
  n_fw_to_n_ino = n_fw_to_n_ino_1 + n_fw_to_n_ino_2

}

# Nitrogen Flux from Abovegr. Coarse Wood to Inorganic Nitrogen
internal_fluxes[["N_acw->N_ino"]] = function(
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

  # ACW --> SRF MIC
  # ***************
  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_c_srfmic = ifelse(C_acw > 0
                             ,ifelse(cando == TRUE
                                     ,(1 - co2acw1) * (1 - ligfr_acw) * tcflow
                                     ,0
                             )
                             ,0
  )

  c_transf = c_acw_to_c_srfmic
  outofa = c_acw_to_c_srfmic / (C_acw / N_acw)

  # Mineralized Nitrogen
  n_acw_to_n_ino_1 <- mineral_n(c_transf, outofa, cn_rat_new)

  # ACW --> SLO
  # ***************
  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- rneww2_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_c_slo = ifelse(C_acw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2acw2) * ligfr_acw * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_acw_to_c_slo
  outofa = c_acw_to_c_slo / (C_acw / N_acw)

  # Mineralized Nitrogen
  n_acw_to_n_ino_2 <- mineral_n(c_transf, outofa, cn_rat_new)

  # Total Mineralized Nitrogen
  n_acw_to_n_ino = n_acw_to_n_ino_1 + n_acw_to_n_ino_2

}

# Nitrogen Flux from Belowgr. Coarse Wood to Inorganic Nitrogen
internal_fluxes[["N_bcw->N_ino"]] = function(
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

  # BCW --> MIC
  # ***************
  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat1_1

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_c_mic = ifelse(C_bcw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2bcw1) * (1 - ligfr_bcw) * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_bcw_to_c_mic
  outofa = c_bcw_to_c_mic / (C_bcw / N_bcw)

  # Mineralized Nitrogen
  n_bcw_to_n_ino_1 <- mineral_n(c_transf, outofa, cn_rat_new)

  # BCW --> SLO
  # ***************
  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat2_2

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_c_slo = ifelse(C_bcw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2bcw2) * ligfr_bcw * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_bcw_to_c_slo
  outofa = c_bcw_to_c_slo / (C_bcw / N_bcw)

  # Mineralized Nitrogen
  n_bcw_to_n_ino_2 <- mineral_n(c_transf, outofa, cn_rat_new)

  # Total Mineralized Nitrogen
  n_bcw_to_n_ino = n_bcw_to_n_ino_1 + n_bcw_to_n_ino_2

}

# Nitrogen Flux from Surface Microbial SOM to Inorganic Nitrogen
internal_fluxes[["N_srfmic->N_ino"]] = function(
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

  c_cont = C_srfmic
  n_cont = N_srfmic

  radds1 = rad1p1 + rad1p2 * ((c_cont / n_cont) - pcemic2)
  cn_rat_new = c_cont / n_cont + radds1 # rceto2
  cn_rat_new = max(cn_rat_new, rad1p3)

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_srfmic * xi(t) * C_srfmic

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_srfmic_to_c_slo = ifelse(C_srfmic > 0
                             ,ifelse(cando == TRUE
                                     ,frsrfmicslo * tcflow
                                     ,0
                             )
                             ,0
  )

  c_transf = c_srfmic_to_c_slo
  outofa = c_srfmic_to_c_slo / (C_srfmic / N_srfmic)

  # Mineralized Nitrogen
  n_srfmic_to_n_ino_1 <- mineral_n(c_transf, outofa, cn_rat_new)

}

# Nitrogen Flux from Microbial SOM to Inorganic Nitrogen
internal_fluxes[["N_mic->N_ino"]] = function(
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

  # MIC --> SLO
  # ***************
  c_cont = C_mic
  n_cont = N_mic

  varatx_1 = varat2_1
  varatx_2 = varat2_2
  varatx_3 = varat2_3
  cn_rat_new <- bgdrat(N_ino # rceto2
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_c_slo = ifelse(C_mic > 0
                          ,ifelse(cando == TRUE
                                  ,frmicslo * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_mic_to_c_slo
  outofa = c_mic_to_c_slo / (C_mic / N_mic)

  # Mineralized Nitrogen
  n_mic_to_n_ino_1 <- mineral_n(c_transf, outofa, cn_rat_new)

  # MIC --> PAS
  # ***************
  c_cont = C_mic
  n_cont = N_mic

  varatx_1 = varat3_1
  varatx_2 = varat3_2
  varatx_3 = varat3_3
  cn_rat_new <- bgdrat(N_ino # rceto3
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_c_pas = ifelse(C_mic > 0
                          ,ifelse(cando == TRUE
                                  ,frmicpas * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_mic_to_c_pas
  outofa = c_mic_to_c_pas / (C_mic / N_mic)
  # Mineralized Nitrogen
  n_mic_to_n_ino_2 <- mineral_n(c_transf, outofa, cn_rat_new)

  # Total Mineralized Nitrogen
  n_mic_to_n_ino = n_mic_to_n_ino_1 + n_mic_to_n_ino_2

}

# Nitrogen Flux from Slow SOM to Inorganic Nitrogen
internal_fluxes[["N_slo->N_ino"]] = function(
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

  # SLO --> MIC
  # ***************
  c_cont = C_slo
  n_cont = N_slo

  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_c_mic = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,frslomic * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_slo_to_c_mic
  outofa = c_slo_to_c_mic / (C_slo / N_slo)

  # Mineralized Nitrogen
  n_slo_to_n_ino_1 <- mineral_n(c_transf, outofa, cn_rat_new)

  # SLO --> PAS
  # ***************
  c_cont = C_slo
  n_cont = N_slo

  varatx_1 = varat3_1
  varatx_2 = varat3_2
  varatx_3 = varat3_3
  cn_rat_new <- bgdrat(N_ino # rceto
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_c_pas = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,frslopas * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_slo_to_c_pas
  outofa = c_slo_to_c_pas / (C_slo / N_slo)

  # Mineralized Nitrogen
  n_slo_to_n_ino_2 <- mineral_n(c_transf, outofa, cn_rat_new)

  # Total Mineralized Nitrogen
  n_slo_to_n_ino = n_slo_to_n_ino_1 + n_slo_to_n_ino_2

}

# Nitrogen Flux from Passive SOM to Inorganic Nitrogen
internal_fluxes[["N_pas->N_ino"]] = function(
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

  c_cont = C_pas
  n_cont = N_pas

  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_pas * xi(t) * C_pas * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_pas_to_c_mic = ifelse(C_pas > 0
                          ,ifelse(cando == TRUE
                                  ,frpasmic * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_pas_to_c_mic
  outofa = c_pas_to_c_mic / (C_pas / N_pas)

  # Mineralized Nitrogen
  n_pas_to_n_ino <- mineral_n(c_transf, outofa, cn_rat_new)

}

# Immobilization ----
# Nitrogen Flux from Inorganic Nitrogen to Surface Microbial SOM
internal_fluxes[["N_ino->N_srfmic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # AM
  # ******
  # Compute the C:N ratio of new material entering in Microbial SOM
  c_cont = C_am
  n_cont = N_am
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_am * xi(t) * C_am

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_am
                  ,tcflow
                  ,C_am
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_am_to_c_srfmic = ifelse(C_am > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2am) * tcflow
                                    ,0
                            )
                            ,0
  )

  c_transf = c_am_to_c_srfmic
  outofa = c_am_to_c_srfmic / (C_am / N_am)

  # Immobilized Nitrogen
  n_ino_to_n_srfmic_1 <- immob_n(c_transf, outofa, cn_rat_new)

  # AS
  # ******
  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_c_srfmic = ifelse(C_as > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2as1) * (1 - ligfr_as) * tcflow
                                    ,0
                            )
                            ,0
  )

  c_transf = c_as_to_c_srfmic
  outofa = c_as_to_c_srfmic / (C_as / N_as)

  # Immobilized Nitrogen
  n_ino_to_n_srfmic_2 <- immob_n(c_transf, outofa, cn_rat_new)

  # FW
  # ******
  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_c_srfmic = ifelse(C_fw > 0
                            ,ifelse(cando == TRUE
                                    ,(1 - co2fw1) * (1 - ligfr_fw) * tcflow
                                    ,0
                            )
                            ,0
  )

  c_transf = c_fw_to_c_srfmic
  outofa = c_fw_to_c_srfmic / (C_fw / N_fw)

  # Immobilized Nitrogen
  n_ino_to_n_srfmic_3 <- immob_n(c_transf, outofa, cn_rat_new)

  # ACW
  # ******
  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_c_srfmic = ifelse(C_acw > 0
                             ,ifelse(cando == TRUE
                                     ,(1 - co2acw1) * (1 - ligfr_acw) * tcflow
                                     ,0
                             )
                             ,0
  )

  c_transf = c_acw_to_c_srfmic
  outofa = c_acw_to_c_srfmic / (C_acw / N_acw)

  # Immobilized Nitrogen
  n_ino_to_n_srfmic_4 <- immob_n(c_transf, outofa, cn_rat_new)

  # Total Immobilized Nitrogen
  n_ino_to_n_srfmic = n_ino_to_n_srfmic_1 + n_ino_to_n_srfmic_2 + n_ino_to_n_srfmic_3 + n_ino_to_n_srfmic_4
  

}

# Nitrogen Flux from Inorganic Nitrogen to Microbial SOM
internal_fluxes[["N_ino->N_mic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # BM
  # ******
  c_cont = C_bm
  n_cont = N_bm

  # C:N ratio of new material entering Microbial SOM
  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bm * xi(t) * C_bm * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bm
                  ,tcflow
                  ,C_bm
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bm_to_c_mic = ifelse(C_bm > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bm) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bm_to_c_mic
  outofa = c_bm_to_c_mic / (C_bm / N_bm)

  # Immobilized Nitrogen
  n_ino_to_n_mic_1 <- immob_n(c_transf, outofa, cn_rat_new)

  # BS
  # ******
  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_1

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_c_mic = ifelse(C_bs > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bs1) * (1 - ligfr_bs) * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bs_to_c_mic
  outofa = c_bs_to_c_mic / (C_bs / N_bs)

  # Immobilized Nitrogen
  n_ino_to_n_mic_2 <- immob_n(c_transf, outofa, cn_rat_new)

  # BCW
  # ******
  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat1_1

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_c_mic = ifelse(C_bcw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2bcw1) * (1 - ligfr_bcw) * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_bcw_to_c_mic
  outofa = c_bcw_to_c_mic / (C_bcw / N_bcw)

  # Immobilized Nitrogen
  n_ino_to_n_mic_3 <- immob_n(c_transf, outofa, cn_rat_new)

  # SLO
  # ******
  c_cont = C_slo
  n_cont = N_slo

  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_c_mic = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,frslomic * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_slo_to_c_mic
  outofa = c_slo_to_c_mic / (C_slo / N_slo)

  # Immobilized Nitrogen
  n_ino_to_n_mic_4 <- immob_n(c_transf, outofa, cn_rat_new)

  # PAS
  # ******
  c_cont = C_pas
  n_cont = N_pas

  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_pas * xi(t) * C_pas * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_pas_to_c_mic = ifelse(C_pas > 0
                          ,ifelse(cando == TRUE
                                  ,frpasmic * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_pas_to_c_mic
  outofa = c_pas_to_c_mic / (C_pas / N_pas)

  # Immobilized Nitrogen
  n_ino_to_n_mic_5 <- immob_n(c_transf, outofa, cn_rat_new)

  # Total Immobilized Nitrogen
  n_ino_to_n_mic = n_ino_to_n_mic_1 + n_ino_to_n_mic_2 + n_ino_to_n_mic_3 + n_ino_to_n_mic_4
  + n_ino_to_n_mic_5

}

# Nitrogen Flux from Inorganic Nitrogen to Slow SOM
internal_fluxes[["N_ino->N_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  # AS
  # ******
  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- rnewas(c_cont
                       ,n_cont
                       ,rad1p1
                       ,rad1p2
                       ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_c_slo = ifelse(C_as > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2as2) * ligfr_as * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_as_to_c_slo
  outofa = c_as_to_c_slo / (C_as / N_as)

  # Immobilized Nitrogen
  n_ino_to_n_slo_1 <- immob_n(c_transf, outofa, cn_rat_new)

  # BS
  # ******
  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_2

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_c_slo = ifelse(C_bs > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2bs2) * ligfr_bs * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_bs_to_c_slo
  outofa = c_bs_to_c_slo / (C_bs / N_bs)

  # Immobilized Nitrogen
  n_ino_to_n_slo_2 <- immob_n(c_transf, outofa, cn_rat_new)

  # FW
  # ******
  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- rneww1_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_c_slo = ifelse(C_fw > 0
                         ,ifelse(cando == TRUE
                                 ,(1 - co2fw2) * ligfr_fw * tcflow
                                 ,0
                         )
                         ,0
  )

  c_transf = c_fw_to_c_slo
  outofa = c_fw_to_c_slo / (C_fw / N_fw)

  # Immobilized Nitrogen
  n_ino_to_n_slo_3 <- immob_n(c_transf, outofa, cn_rat_new)

  # ACW
  # ******
  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- rneww2_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_c_slo = ifelse(C_acw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2acw2) * ligfr_acw * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_acw_to_c_slo
  outofa = c_acw_to_c_slo / (C_acw / N_acw)

  # Immobilized Nitrogen
  n_ino_to_n_slo_4 <- immob_n(c_transf, outofa, cn_rat_new)

  # BCW
  # ******
  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat2_2

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_c_slo = ifelse(C_bcw > 0
                          ,ifelse(cando == TRUE
                                  ,(1 - co2bcw2) * ligfr_bcw * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_bcw_to_c_slo
  outofa = c_bcw_to_c_slo / (C_bcw / N_bcw)

  # Immobilized Nitrogen
  n_ino_to_n_slo_5 <- immob_n(c_transf, outofa, cn_rat_new)

  # SRF MIC
  # ******
  c_cont = C_srfmic
  n_cont = N_srfmic

  radds1 = rad1p1 + rad1p2 * ((c_cont / n_cont) - pcemic2)
  cn_rat_new = c_cont / n_cont + radds1 # rceto2
  cn_rat_new = max(cn_rat_new, rad1p3)

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_srfmic * xi(t) * C_srfmic

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_srfmic_to_c_slo = ifelse(C_srfmic > 0
                             ,ifelse(cando == TRUE
                                     ,frsrfmicslo * tcflow
                                     ,0
                             )
                             ,0
  )

  c_transf = c_srfmic_to_c_slo
  outofa = c_srfmic_to_c_slo / (C_srfmic / N_srfmic)

  # Immobilized Nitrogen
  n_ino_to_n_slo_6 <- immob_n(c_transf, outofa, cn_rat_new)


  # MIC
  # ******
  c_cont = C_mic
  n_cont = N_mic

  varatx_1 = varat2_1
  varatx_2 = varat2_2
  varatx_3 = varat2_3
  cn_rat_new <- bgdrat(N_ino # rceto2
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_c_slo = ifelse(C_mic > 0
                          ,ifelse(cando == TRUE
                                  ,frmicslo * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_mic_to_c_slo
  outofa = c_mic_to_c_slo / (C_mic / N_mic)

  # Immobilized Nitrogen
  n_ino_to_n_slo_7 <- immob_n(c_transf, outofa, cn_rat_new)

  # Total Immobilized Nitrogen
  n_ino_to_n_slo = n_ino_to_n_slo_1 + n_ino_to_n_slo_2 + n_ino_to_n_slo_3 + n_ino_to_n_slo_4
  + n_ino_to_n_slo_5 + n_ino_to_n_slo_6 + n_ino_to_n_slo_7

}

# Nitrogen Flux from Inorganic Nitrogen to Passive SOM
internal_fluxes[["N_ino->N_pas"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # MIC
  # ******
  c_cont = C_mic
  n_cont = N_mic

  varatx_1 = varat3_1
  varatx_2 = varat3_2
  varatx_3 = varat3_3
  cn_rat_new <- bgdrat(N_ino # rceto3
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_c_pas = ifelse(C_mic > 0
                          ,ifelse(cando == TRUE
                                  ,frmicpas * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_mic_to_c_pas
  outofa = c_mic_to_c_pas / (C_mic / N_mic)

  # Immobilized Nitrogen
  n_ino_to_n_pas_1 <- immob_n(c_transf, outofa, cn_rat_new)

  # SLO
  # ******
  c_cont = C_slo
  n_cont = N_slo

  varatx_1 = varat3_1
  varatx_2 = varat3_2
  varatx_3 = varat3_3
  cn_rat_new <- bgdrat(N_ino # rceto
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )

  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )

  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_c_pas = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,frslopas * tcflow
                                  ,0
                          )
                          ,0
  )

  c_transf = c_slo_to_c_pas
  outofa = c_slo_to_c_pas / (C_slo / N_slo)

  # Immobilized Nitrogen
  n_ino_to_n_pas_2 <- immob_n(c_transf, outofa, cn_rat_new)

  # Total Immobilized Nitrogen
  n_ino_to_n_pas = n_ino_to_n_pas_1 + n_ino_to_n_pas_2

}

# Outfluxes ----

out_fluxes = list()

# Flux from Abovegr. Metabolic
out_fluxes[["C_am"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # C:N ratio of new material entering Microbial SOM
  c_cont = C_am
  n_cont = N_am
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_am * xi(t) * C_am

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_am
                  ,tcflow
                  ,C_am
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_am_to_co2 = ifelse(C_am > 0
                         ,ifelse(cando == TRUE
                                 ,co2am * tcflow
                                 ,0
                         )
                         ,0
  )

}


# Flux from Abovegr. Structural
out_fluxes[["C_as"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # AS -> SRF MIC
  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_co2_1 = ifelse(C_as > 0
                         ,ifelse(cando == TRUE
                                 ,co2as1 * (1 - ligfr_as) * tcflow
                                 ,0
                         )
                         ,0
  )

  # AS -> SLO
  c_cont = C_as
  n_cont = N_as
  cn_rat_new <- rnewas(c_cont
                       ,n_cont
                       ,rad1p1
                       ,rad1p2
                       ,rad1p3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_as * xi(t) * C_as * exp(-pligst * ligfr_as)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_as
                  ,tcflow
                  ,C_as
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_as_to_co2_2 = ifelse(C_as > 0
                       ,ifelse(cando == TRUE
                               ,co2as2 * ligfr_as * tcflow
                               ,0
                       )
                       ,0
  )

  c_as_to_co2 = c_as_to_co2_1 + c_as_to_co2_2

}

# Flux from Belowgr. Metabolic
out_fluxes[["C_bm"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # Compute the C:N ratio of new material entering Microbial SOM
  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  c_cont = C_bm
  n_cont = N_bm
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bm * xi(t) * C_bm * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bm
                  ,tcflow
                  ,C_bm
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_bm_to_c_co2 = ifelse(C_bm > 0
                         ,ifelse(cando == TRUE
                                 ,co2bm * tcflow
                                 ,0
                         )
                         ,0
  )

}

# Flux from Belowgr. Structural
out_fluxes[["C_bs"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # AS -> MIC
  # Verify if decomposition can occur (Nitrogen limitations)
  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_1
  
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_co2_1 = ifelse(C_bs > 0
                       ,ifelse(cando == TRUE
                               ,co2bs1 * (1 - ligfr_bs) * tcflow
                               ,0
                       )
                       ,0
  )

  # AS -> SLO
  # Verify if decomposition can occur (Nitrogen limitations)
  c_cont = C_bs
  n_cont = N_bs
  cn_rat_new = varat1_2
  
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bs * xi(t) * C_bs * exp(-pligst * ligfr_bs) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bs
                  ,tcflow
                  ,C_bs
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_bs_to_co2_2 = ifelse(C_bs > 0
                       ,ifelse(cando == TRUE
                               ,co2bs2 * ligfr_bs * tcflow
                               ,0
                       )
                       ,0
  )

  c_bs_to_co2 = c_bs_to_co2_1 + c_bs_to_co2_2

}

# Flux from Fine Wood
out_fluxes[["C_fw"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # FW -> SRF MIC
  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3)
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_co2_1 = ifelse(C_fw > 0
                       ,ifelse(cando == TRUE
                               ,co2fw1 * (1 - ligfr_fw) * tcflow
                               ,0
                       )
                       ,0
  )

  # FW -> SLO
  c_cont = C_fw
  n_cont = N_fw
  cn_rat_new <- rneww1_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_fw * xi(t) * C_fw * exp(-pligst * ligfr_fw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_fw
                  ,tcflow
                  ,C_fw
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_fw_to_co2_2 = ifelse(C_fw > 0
                       ,ifelse(cando == TRUE
                               ,co2fw2 * ligfr_fw * tcflow
                               ,0
                       )
                       ,0
  )

  c_fw_to_co2 = c_fw_to_co2_1 + c_fw_to_co2_2

}

# Flux from Abovegr. Coarse Wood
out_fluxes[["C_acw"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # ACW -> MIC
  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- agdrat(c_cont
                       ,n_cont
                       ,biocnv_wood
                       ,cemicb
                       ,pcemic1
                       ,pcemic2
                       ,pcemic3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_co2_1 = ifelse(C_acw > 0
                       ,ifelse(cando == TRUE
                               ,co2acw1 * (1 - ligfr_acw) * tcflow
                               ,0
                       )
                       ,0
  )

  # ACW -> SLO
  c_cont = C_acw
  n_cont = N_acw
  cn_rat_new <- rneww2_2(c_cont
                         ,n_cont
                         ,rad1p1
                         ,rad1p2
                         ,rad1p3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_acw * xi(t) * C_acw * exp(-pligst * ligfr_acw)

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_acw
                  ,tcflow
                  ,C_acw
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_acw_to_co2_2 = ifelse(C_acw > 0
                        ,ifelse(cando == TRUE
                                ,co2acw2 * ligfr_acw * tcflow
                                ,0
                        )
                        ,0
  )

  c_acw_to_co2 = c_acw_to_co2_1 + c_acw_to_co2_2

}

# Flux from Belowgr. Coarse Wood
out_fluxes[["C_bcw"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  # BCW -> MIC
  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat1_1
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_co2_1 = ifelse(C_bcw > 0
                        ,ifelse(cando == TRUE
                                ,co2bcw1 * (1 - ligfr_bcw) * tcflow
                                ,0
                        )
                        ,0
  )

  # BCW -> SLO
  c_cont = C_bcw
  n_cont = N_bcw
  cn_rat_new = varat2_2
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_bcw * xi(t) * C_bcw * exp(-pligst * ligfr_bcw) * anerb

  # If decomposed carbon is higher then Carbon amount in the pool
  tcflow = ifelse(tcflow <= C_bcw
                  ,tcflow
                  ,C_bcw
  )

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_bcw_to_co2_2 = ifelse(C_bcw > 0
                        ,ifelse(cando == TRUE
                                ,co2bcw2 * ligfr_bcw * tcflow
                                ,0
                        )
                        ,0
  )

  c_bcw_to_co2 = c_bcw_to_co2_1 + c_bcw_to_co2_2

}

# Flux from Surface Microbial SOM
out_fluxes[["C_srfmic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t
  
) {
  
  c_cont = C_srfmic
  n_cont = N_srfmic
  
  radds1 = rad1p1 + rad1p2 * ((c_cont / n_cont) - pcemic2)
  cn_rat_new = c_cont / n_cont + radds1 # rceto2 in CENTURY4
  cn_rat_new = max(cn_rat_new, rad1p3)
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_srfmic * xi(t) * C_srfmic
  
  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_srfmic_to_c_slo = ifelse(C_srfmic > 0
                             ,ifelse(cando == TRUE
                                     ,co2srfmic * tcflow
                                     ,0
                             )
                             ,0
  )
  
}

# Flux from Microbial SOM
out_fluxes[["C_mic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {

  c_cont = C_mic
  n_cont = N_mic
  
  varatx_1 = varat2_1
  varatx_2 = varat2_2
  varatx_3 = varat2_3
  cn_rat_new <- bgdrat(N_ino
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_mic * xi(t) * C_mic * eftext * anerb

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_mic_to_co2 = ifelse(C_mic > 0
                        ,ifelse(cando == TRUE
                                ,co2mic * tcflow
                                ,0
                        )
                        ,0
  )

}

# Flux from Slow SOM
out_fluxes[["C_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  
  c_cont = C_slo
  n_cont = N_slo
  
  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1 in CENTURY4
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_slo * xi(t) * C_slo * anerb

  # Respired Carbon flux if C is positive and Nitrogen is not limiting
  c_slo_to_co2 = ifelse(C_slo > 0
                          ,ifelse(cando == TRUE
                                  ,co2slo * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Passive SOM
out_fluxes[["C_pas"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  
  c_cont = C_slo
  n_cont = N_slo
  
  varatx_1 = varat1_1
  varatx_2 = varat1_2
  varatx_3 = varat1_3
  cn_rat_new <- bgdrat(N_ino # rceto1 in CENTURY4
                       ,varatx_1
                       ,varatx_2
                       ,varatx_3
  )
  
  # Verify if decomposition can occur (Nitrogen limitations)
  cando = ifelse(sim_type == 'c-only'
                 ,TRUE
                 ,candec(c_cont
                         ,n_cont
                         ,cn_rat_new
                         ,N_ino
                 )
  )
  
  # Carbon lost by the pool
  tcflow = k_pas * xi(t) * C_pas * anerb

  # Transfer Carbon flux if C is positive and Nitrogen is not limiting
  c_pas_to_co2 = ifelse(C_pas > 0
                          ,ifelse(cando == TRUE
                                  ,co2pas * tcflow
                                  ,0
                          )
                          ,0
  )

}

# Flux from Aboveground Metabolic Nitrogen
out_fluxes[["N_am"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Aboveground Structural Nitrogen
out_fluxes[["N_as"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Belowgr. Metabolic   Nitrogen
out_fluxes[["N_bm"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Belowgr. Structural Nitrogen
out_fluxes[["N_bs"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Fine Wood Nitrogen
out_fluxes[["N_fw"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Abovegr. Coarse Wood Nitrogen
out_fluxes[["N_acw"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Belowgr. Coarse Wood Nitrogen
out_fluxes[["N_bcw"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Microbial SOM Nitrogen
out_fluxes[["N_mic"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Slow SOM Nitrogen
out_fluxes[["N_slo"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Passive SOM Nitrogen
out_fluxes[["N_pas"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0
}

# Flux from Inorganic Nitrogen
out_fluxes[["N_ino"]] = function(
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
  ,C_srfmic
  ,N_srfmic
  ,C_mic
  ,N_mic
  ,C_slo
  ,N_slo
  ,C_pas
  ,N_pas
  ,N_ino
  ,t

) {
  0 #kup * N_ino
}

# Input fluxes - Constant (Carbon fluxes) ----
input_fluxes_const = list()

# Input C flux into Abovegr. Metabolic Lit
input_fluxes_const[["C_am"]] = function() {
  leaf_inp * fm_frac('abg', ligfr_as)
}

# Input C flux into Abovegr. Structural Lit
input_fluxes_const[["C_as"]] = function() {
  leaf_inp * (1 - fm_frac('abg', ligfr_as))
}

# Input C flux into Belowgr. Metabolic Lit
input_fluxes_const[["C_bm"]] = function() {
  froo_inp * fm_frac('blg', ligfr_bs)
}

# Input C flux into Belowgr. Structural Lit
input_fluxes_const[["C_bs"]] = function() {
  froo_inp * (1 - fm_frac('blg', ligfr_bs))
}

# Input C flux into Fine Wood Lit
input_fluxes_const[["C_fw"]] = function() {
  fw_inp
}

# Input C flux into Abovegr. Coarse Wood Lit
input_fluxes_const[["C_acw"]] = function() {
  acw_inp
}

# Input C flux into Belowgr. Coarse Wood Lit
input_fluxes_const[["C_bcw"]] = function() {
  bcw_inp
}

# Input N flux into Abovegr. Metabolic Lit
input_fluxes_const[["N_am"]] = function() {
  leaf_inp * fm_frac('abg', ligfr_as) / cn_leaf
}

# Input N flux into Abovegr. Structural Lit
input_fluxes_const[["N_as"]] = function() {
  leaf_inp * (1 - fm_frac('abg', ligfr_as)) / cn_leaf
}

# Input N flux into Belowgr. Metabolic Lit
input_fluxes_const[["N_bm"]] = function() {
  froo_inp * fm_frac('blg', ligfr_bs) / cn_froo
}

# Input N flux into Belowgr. Structural Lit
input_fluxes_const[["N_bs"]] = function() {
  froo_inp * (1 - fm_frac('blg', ligfr_bs)) / cn_froo
}

# Input N flux into Fine Wood Lit
input_fluxes_const[["N_fw"]] = function() {
  fw_inp / cn_wood
}

# Input N flux into Abovegr. Coarse Wood Lit
input_fluxes_const[["N_acw"]] = function() {
  acw_inp / cn_wood
}

# Input N flux into Belowgr. Coarse Wood Lit
input_fluxes_const[["N_bcw"]] = function() {
  bcw_inp / cn_wood
}