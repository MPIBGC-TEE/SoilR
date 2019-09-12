print_loud<-function(obj){
  print("#######################################")
  print(obj)
  print("#######################################")
}

# Preliminary ----

# General info *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Site name:          Hainich
# Country:            Germany
# Type:               Forest
# Specie:             Beech
# Latitude (°):       51◦4′45.36′′N
# Longitude (°):      10◦27′7.20′′E
# Elevation (m):      440 m a.s.l.  (Kutsch et al., 2010)
# Mean Annual Air T:  7.5-8 °C      (Kutsch et al., 2010)
# Annual Rainfall:    750-800 mm    (Kutsch et al., 2010)
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Simulation run ('c-only' or 'cn-coupled')
sim_type = 'cn-coupled'

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Soil texture
# Depth 50-60 cm
clay_frac = 0.7000
silt_frac = 0.2790
sand_frac = 0.0174
# Check if total soil texture is = 1
tot_soilfrac = clay_frac + silt_frac + sand_frac
if (tot_soilfrac < 0.99 | tot_soilfrac > 1) stop('ERROR: clay + silt + sand fractions are not equal 1')

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Lignin content (gr lignin/gr biomass)
ligfr_as  = 0.18
ligfr_bs  = 0.18
ligfr_fw  = 0.22
ligfr_acw = 0.26
ligfr_bcw = 0.26
# Effect of Lignin/Structural ratio on decomposition
pligst    = 3

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Effect of anaerobic conditions on decomposition
anerb  = 1 # Computed in "anerob.f" in CENTURY4.0 code
# Slope term to simulate the effect of anaerobic conditions on decomposition
animpt = 5

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Decomposition base rates (1/year) (in parentesis the values from CENW model)
k_am     = 14.8 #26.9
k_as     = 3.9 #7.3
k_bm     = 18.5 #33.6
k_bs     = 4.9 #9
k_fw     = 1.5 #3.65
k_acw    = 0.5 #0.73
k_bcw    = 0.6 #0.73
k_srfmic = 6.0
k_mic    = 7.3 #13.4
k_slo    = 0.2 #0.36 # 0.01
k_pas    = 0.0045 #0.013

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Fraction of Carbon lost as CO2
# Metabolic litter
co2am     = 0.55
co2bm     = 0.55
# Structural litter
co2as1    = 0.45 # to mic SOM
co2as2    = 0.30 # to slo SOM
co2bs1    = 0.55 # to mic SOM
co2bs2    = 0.30 # to slo SOM
# Wood
co2fw1    = 0.45
co2fw2    = 0.30
co2acw1   = 0.45
co2acw2   = 0.30
co2bcw1   = 0.55
co2bcw2   = 0.30
# Microbial SOM
co2mic1   = 0.17
co2mic2   = 0.68
co2mic    = co2mic1 + co2mic2 * sand_frac
co2slo    = 0.55
co2pas    = 0.55 * anerb
co2srfmic = 0.60

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Transfer fractions among SOM pools
ps1s3a      = 0.003
ps1s3b      = 0.032
frmicpas    = (ps1s3a + ps1s3b * clay_frac) * (1 + animpt * (1 - anerb))
frmicslo    = 1 - co2mic - frmicpas
ps2s3a      = 0.003
ps2s3b      = 0.009
frslopas    = (ps2s3a + ps2s3b * clay_frac) * (1 + animpt * (1 - anerb))
frslomic    = 1 - co2slo - frslopas
frpasmic    = 1 - co2pas
frsrfmicslo = 1 - co2srfmic

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  
# Effect of soil texture on the microbial decomposition rate
peftxa = 0.25
peftxb = 0.75
eftext = peftxa + peftxb * sand_frac

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Soil temperature effect on SOM decomposition
# Parameters 'a' and 'b' for the exponential equation implemented in CENTURY4 code
par_a = 0.125
par_b = 0.070

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# scaling factor for potential evapotranspiration (pevap.f in CENTURY4 code)
fwloss_4 = 0.70

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Plant uptake
# kup = 0.05

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Run settings ----

# Time range and pool number
t_start = 0
t_end   = 10000
times   = seq(t_start, t_end, by = 1)

nr      = 23 # Number of pools

tn      = 100 
tol     = .02 / tn

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Carbon and Nitrogen input in Litter and Woody pools
# cn_input = data.frame('t' = times, read.csv(file = '/home/corrado/SoilR-exp/pkg/inst/tests/src/cn_input.csv', header = T, sep = ','))

# Input data ----

# Litter input (gC*m-2*y-1)
leaf_inp = 314 #340
froo_inp = 178
fw_inp   = 20
acw_inp  = 20
bcw_inp  = 20

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# CN ratios of residues
cn_leaf = 50
cn_froo = 40
cn_wood = 200

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Initial C:N ratio for litter and SOM pools
ini_cn_rat_abgmet = 88
ini_cn_rat_abgstr = 88
ini_cn_rat_blwmet = 66
ini_cn_rat_blwstr = 66
ini_cn_rat_fw     = 100 # set by me
ini_cn_rat_acw    = 100 # set by me
ini_cn_rat_bcw    = 100 # set by me
ini_cn_rat_srfmic = 15
ini_cn_rat_mic    = 14
ini_cn_rat_slo    = 25
ini_cn_rat_pas    = 12

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

requireNamespace('SoilR')
# Climate data
# (PPT = monthly precipitation (mm); PET = monthly evapotranspiration (mm); TempData = monthly temperature (°C))
# Function "pevap" Compute Potential Evapotranspiration ----
pevap <- function() {
  
  # Avg temp for each month
  avgtemp = (maxtemp - mintemp) / 2
  
  # Minimum and Maximum temperature for each year
  highest = max(avgtemp)
  lowest  = min(avgtemp)
  lowest  = max(lowest, -10)
  
  # Avg temp range
  ra = abs(highest - lowest)
  
  # Temp range
  tr = maxtemp - max(-10, mintemp)
  t_fac  = tr / (2 + mintemp)
  tm = t_fac + 0.006 * elev_site
  td = 0.0023 * elev_site + 0.37 * t_fac + 0.53 * tr + 0.35 * ra -10.9
  e_fac  = ((700 * tm / (100 - abs(latit_site))) + 15 * td) / (80 - t)
  monpet = (e_fac * 30) / 10
  
  monpet = ifelse(monpet < 0.5
                  ,0.5
                  ,monpet
  )
  
  pevap = monpet * fwloss_4
  
  
}

# Soil temperature and moisture effect on SOM decomposition ----

TempData = c(length = length(times))
TempData = data.frame('times' = times, 'Temp' = rep(7.9, TempData))
# TempData = data.frame(times, 'Temp' = meteo$Temp)
# Approximation function of temperature
# TempFunction = approxfun(x = meteo$times, y = meteo$Temp)
TempFunction = approxfun(x = TempData$times, y = TempData$Temp)

# Temperature (function implemented in CENTURY 4)
fT.Century4 <- function(Temp, par_a, par_b) {
  par_a * exp(par_b * Temp)
}

# xi scalar computed by different combinations of Temp and Mois funcs
xi_T1_W1 <- function(t){
  fT.Century4(Temp = TempFunction(t), par_a, par_b)*
    fW.Century(PPT = 800, PET = 500)
}

xi <- xi_T1_W1

# # Put all xi_T_W functions in a list
# xiFuncs = list(xi_T1_W1, xi_T1_W2, xi_T2_W1, xi_T2_W2, xi_T3_W1, xi_T3_W2)
# # The entire system in a for loop of "xi" in the xi_T_W func list
# # for (xi in xiFuncs){

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "fm_frac"_Split C,N input into Metabolic and Structural litter # ----
fm_frac <- function(layer, ln_rat) {
  
  ifelse(layer == 'abg'
         ,0.99 - ln_rat * 0.018
         ,0.85 - ln_rat * 0.018
  )
  
}

# Function "candec"_Verify if decomposition can occur (C,N decomposition limited by available inorganic Nitrogen) # ----
candec <- function(c_cont, n_cont, cn_rat_new, N_ino) {
  
  ifelse(N_ino < 0
         ,ifelse((c_cont / n_cont > cn_rat_new)
                 ,FALSE
                 ,TRUE
         )
         ,TRUE
  )
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "agdrat"_Compute the C:N ratio of new material from Aboveground pools entering Microbial SOM # ----
pcemic1 = 16
pcemic2 = 10
pcemic3 = 0.02

# Biomass conversion factor
biocnv = 2.5 # non-wood material
biocnv_wood = 2 # wood material

cemicb = (pcemic2 - pcemic1) / pcemic3

agdrat <- function(c_cont
                   ,n_cont
                   ,biocnv
                   ,cemicb
                   ,pcemic1
                   ,pcemic2
                   ,pcemic3) {
        
  econt = ifelse((c_cont * biocnv) <= 0
                 ,0
                 ,n_cont/(c_cont*biocnv)
  )
  
  ifelse(econt > pcemic3
         ,pcemic2
         ,pcemic1 + econt * cemicb
         
  )  
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "bgdrat"_Compute the C:N ratio of new material from Soil pools entering Microbial SOM # ----
varat1_1 = 18
varat1_2 = 8
varat1_3 = 2

varat2_1 = 40
varat2_2 = 12
varat2_3 = 2

varat3_1 = 20
varat3_2 = 6
varat3_3 = 2

bgdrat <- function(
  N_ino
  ,varatx_1
  ,varatx_2
  ,varatx_3
) {
  
  ifelse(N_ino <= 0
         ,varatx_1
         ,ifelse(N_ino > varatx_3
                 ,varatx_2
                 ,(1 - N_ino / varatx_3) * (varatx_1 - varatx_2) + varatx_2
         )
  )
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "rnewas"_Compute C:N ratio from Abovegr. Structural Litter entering Slow SOM # ----
rad1p1 = 12
rad1p2 = 3
rad1p3 = 5

pcemic2 = 10

rnewas <- function(c_cont
                   ,n_cont
                   ,rad1p1
                   ,rad1p2
                   ,rad1p3
) 
  {
  
  rnewas1 = agdrat (c_cont
                    ,n_cont
                    ,biocnv
                    ,cemicb
                    ,pcemic1
                    ,pcemic2
                    ,pcemic3
  )
  
  radds1 = rad1p1 + rad1p2 * (rnewas1 - pcemic2)
  rnewas2 = rnewas1 + radds1
  rnewas2 = max(rnewas2, rad1p3)
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "rneww1_2"_Compute C:N ratio to Slow SOM for Fine Wood # ----
rneww1_2 <- function(c_cont
                     ,n_cont
                     ,rad1p1
                     ,rad1p2
                     ,rad1p3
) 
{
  
  rneww1 = agdrat (c_cont
                   ,n_cont
                    ,biocnv
                    ,cemicb
                    ,pcemic1
                    ,pcemic2
                    ,pcemic3
  )
  
  radds1 = rad1p1 + rad1p2 * (rneww1 - pcemic2)
  rneww1_2 = rneww1 + radds1
  rneww1_2 = max(rneww1_2, rad1p3)
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "rneww2_2"_Compute C:N ratio to Slow SOM for Abovegr. Coarse Wood # ----
rneww2_2 <- function(c_cont
                     ,n_cont
                     ,rad1p1
                     ,rad1p2
                     ,rad1p3
) 
{
  
  rneww2 = agdrat (c_cont
                   ,n_cont
                   ,biocnv
                   ,cemicb
                   ,pcemic1
                   ,pcemic2
                   ,pcemic3
  )
  
  radds1 = rad1p1 + rad1p2 * (rneww2 - pcemic2)
  rneww2_2 = rneww2 + radds1
  rneww2_2 = max(rneww2_2, rad1p3)
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "esched"_Schedule Organic Nitrogen flux # ----
esched <- function(c_transf, outofa, cn_rat_new) {

  ifelse(outofa > 0,
         ifelse((c_transf / outofa) > cn_rat_new
                ,outofa
                ,c_transf / cn_rat_new
         )
         ,0
  )

}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "mineral_n"_Schedule Nitrogen mineralization pathway # ----
mineral_n <- function(c_transf, outofa, cn_rat_new) {
  
  ifelse(outofa > 0,
         ifelse((c_transf / outofa) > cn_rat_new
                ,0
                ,outofa - (c_transf / cn_rat_new)
         )
         ,0
  )
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Function "immob_n"_Schedule Nitrogen immobilization pathway # ----
immob_n <- function(c_transf, outofa, cn_rat_new) {
  
  ifelse(outofa > 0,
         ifelse((c_transf / outofa) > cn_rat_new
                ,(c_transf / cn_rat_new) - outofa
                ,0
         )
         ,0
  )
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# NO!_Function "c_leaf" ----
c_leaf <- function(t) {
  
  pos1 = which(cn_input$t == t)
  if(length(pos1) > 0) cn_input$C_leaf[pos1]
  
}

# NO!_Function "n_leaf" ----
n_leaf <- function(t) {
  
  pos1 = which(cn_input$t == t)
  if(length(pos1) > 0) cn_input$N_leaf[pos1]
  
}
