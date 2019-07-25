print_loud<-function(obj){
        print("#######################################")
        print(obj)
        print("#######################################")
}
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
# Approximation function from temperature data. It's called by chi_T_W functions
TempFunction=approxfun(x=TempData$times,y=TempData$Temp)

MoisData = data.frame(times, Mois = 70 + 10 * sin(2 * pi * times - (pi / 7)) 
                      + rnorm(n = length(times), mean = 0, sd = 1))
# Approximation function from moisture data. It's called by chi_T_W functions
MoisFunction=approxfun(x=MoisData$times,y=MoisData$Mois)

# Chi scalar computed by different combinations of Temp and Mois funcs
requireNamespace('SoilR')
chi_T1_W1 <- function(t){
    fT.Q10(Temp = TempFunction(t), Q10 = 2)*
    fW.Daycent1(swc =MoisFunction(t)/100)$wfunc
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

# Put all chi_T_W functions in a list
chiFuncs = list(chi_T1_W1, chi_T1_W2, chi_T2_W1, chi_T2_W2, chi_T3_W1, chi_T3_W2)

# The entire system in a for loop of "chi" in the chi_T_W func list
# for (chi in chiFuncs){

# For now set "chi" equal to chi_T1_W1
chi <- chi_T1_W1


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
#------------------------------------------
phi_mn_scalar<-function(
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
  term1=term1(
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
  )
  # Initial value of phi_mn
  phi_mn = 1
  
  # Immobilization/Mineralization rate
  phi    = phi_mn * chi(t) * term1
  
  # Maximum immobilization available
  imm_max  = N_ino * kn * chi(t)

  # Different conditions
  phi_mn = ifelse(term1 < 0 & abs(phi) > imm_max, -kn * N_ino / term1, 1)
  return(phi_mn) 
}

