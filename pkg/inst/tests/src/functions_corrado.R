print_loud<-function(obj){
  print("#######################################")
  print(obj)
  print("#######################################")
}

# Preliminary ----
# Constants and parameters used in the model
# Soil texture
clay_frac = 0.2
silt_frac = 0.3
sand_frac = 0.5

# Decomposition base rates (1/year)
# To change into 1/day!!!
kam  = 14.8
kas  = 3.9
kbm  = 18.5
kbs  = 4.9
kfw  = 0.3
kacw = 0.3 # to change!
kbcw = 0.3 # to change!
kmic = 7.3
kslo = 0.01
kpas = 0.0045

# Immobilization rate
kn = 0.1 # to define!!! This is a default value!

# Plant uptake
kup = 0.1 # to define!!! This is a default value!

# Fractions of internal fluxes into SOM pools (constant over time)
rslo  = 0.25      # (Fraction into Slow pool. Litter lignin concentration)
rmic  = 1 - rslo  # (Fraction into Microbial pool)
rms   = (1 - 0.004 - 0.17 * (clay_frac + silt_frac))
rp    = 0.003 - 0.009 * clay_frac
rsm   = 1 - 0.55 - rp

# Time range and pool number
t_start = 0
t_end   = 20
times       = seq(t_start, t_end, by = 1) # to check!!!

nr      = 21 # Number of pools

tn      = 100 
tol     = .02 / tn


# Temperature and Moisture values ----
# Now set as default values. Given as input from a csv file
TempData = data.frame(times, Temp = 15 + sin(2 * pi * times) 
                      + rnorm(n = length(times), mean = 0, sd = 1))
# Approximation function from temperature data. It's called by xi_T_W functions
TempFunction = approxfun(x=TempData$times,y=TempData$Temp)

MoisData = data.frame(times, Mois = 70 + 10 * sin(2 * pi * times - (pi / 7)) 
                      + rnorm(n = length(times), mean = 0, sd = 1))
# Approximation function from moisture data. It's called by xi_T_W functions
MoisFunction = approxfun(x=MoisData$times,y=MoisData$Mois)

# xi scalar computed by different combinations of Temp and Mois funcs
requireNamespace('SoilR')
xi_T1_W1 <- function(t){
  fT.Q10(Temp = TempFunction(t), Q10 = 2)*
    fW.Daycent1(swc =MoisFunction(t)/100)$wfunc
}
xi_T1_W2 <- function(t){
  fT.Q10(Temp = TempFunction(t), Q10 = 2)*
    fW.Gompertz(theta = MoisFunction(t))
}

xi_T2_W1 <- function(t){
  fT.Century1(Temp = TempFunction(t), Tmax = 45, Topt = 35)*
    fW.Daycent1(swc =MoisFunction(t)/100)
}
xi_T2_W2 <- function(t){
  fT.Century1(Temp = TempFunction(t), Tmax = 45, Topt = 35)*
    fW.Gompertz(theta = MoisFunction(t))
}
xi_T3_W1 <- function(t){
  fT.LandT(Temp = TempFunction(t))*
    fW.Daycent1(swc =MoisFunction(t)/100)
}
xi_T3_W2 <- function(t){
  fT.LandT(Temp = TempFunction(t))*
    fW.Gompertz(theta = MoisFunction(t))
}

# Put all xi_T_W functions in a list
xiFuncs = list(xi_T1_W1, xi_T1_W2, xi_T2_W1, xi_T2_W2, xi_T3_W1, xi_T3_W2)

# The entire system in a for loop of "xi" in the xi_T_W func list
# for (xi in xiFuncs){

# For now set "xi" equal to xi_T1_W1
xi <- xi_T1_W1

# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
term1 <- function( # ----
                   
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
                   
){
  
  term =  kam * N_am * (1 - 0.4 * ((C_am / N_am) / (C_mic / N_mic)) )                                                          +
    kas * N_as * ( (1 - (0.4 * rmic * ((C_as / N_as) / (C_mic / N_mic))) - (rslo * ((C_as / N_as) / (C_slo / N_slo)))))        +
    kbm * N_bm * (1 - 0.45 * ((C_bm / N_bm) / (C_mic / N_mic)) )                                                               +
    kbs * N_bs * ( (1 - (0.45 * rmic * ((C_bs / N_bs) / (C_mic / N_mic))) - (rslo * ((C_bs / N_bs) / (C_slo / N_slo)))))       +
    kfw * N_fw * ( (1 - (0.45 * rmic * ((C_fw / N_fw) / (C_mic / N_mic))) - (rslo * ((C_fw / N_fw) / (C_slo / N_slo)))))       +
    kacw * N_acw * ( (1 - (0.45 * rmic * ((C_acw / N_acw) / (C_mic / N_mic))) - (rslo * ((C_acw / N_acw) / (C_slo / N_slo))))) +
    kbcw * N_bcw * ( (1 - (0.45 * rmic * ((C_bcw / N_bcw) / (C_mic / N_mic))) - (rslo * ((C_bcw / N_bcw) / (C_slo / N_slo))))) +
    kslo * N_slo * ( (1 - (rsm * ((C_slo / N_slo) / (C_mic / N_mic))) - (rp * ((C_slo / N_slo) / (C_slo / N_slo)))))           +
    kpas * N_pas * (1 - 0.45 * ((C_pas / N_pas) / (C_mic / N_mic)) )
  
  term
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
imm_max <- function( # ----
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
  
  # Maximum immobilization available
  imm_max  = N_ino * kn * xi(t)

}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
phi_mn_scalar <- function( # ----
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
  
){
  
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
  
  # Different conditions
  # abs(xi(t) * term1) should be abs(phi), with "phi" =  xi(t) * term1 * phi_mn, with "phi_mn" = 1
  phi_mn = ifelse(term1 < 0 & abs(xi(t) * term1) > imm_max, -kn * N_ino / term1, 1)
  return(phi_mn) 
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
phi <- function( # ----
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
  
  phi_mn_scalar = phi_mn_scalar(
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
  )
  
  phi = phi_mn_scalar * xi(t) * term1
  
}
# *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-