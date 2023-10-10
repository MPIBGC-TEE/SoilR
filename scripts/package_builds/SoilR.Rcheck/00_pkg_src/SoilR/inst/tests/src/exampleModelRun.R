library(ggplot2)
require("pkgload")
source("../testhelpers.R")
source("corradoFluxLists.R")
pkgload::load_all('../../../')
# formulate a linear autonomous model
# (constant matrix and constant influxes)
iv<-c(
  # Initial values from CENTURY4 ('tdecid.100' in PARAMETER_FILES folder)
  # gC*m-2; gN*m-2
  C_am 		  = 0.0001 #335.5 #0.1
  ,C_as		  = 0.0001 #9.1 #0.1
  ,C_bm		  = 0.0001 #150.7 #0.1
  ,C_bs		  = 0.0001 #44 #0.1 
  ,C_fw		  = 0.0001 #118.7 #0.1
  ,C_acw		= 0.0001 #401.6 #0.1
  ,C_bcw		= 0.0001 #334.6 #0.1
  ,C_srfmic = 0.0001 #130.2 #0.1
  ,C_mic		= 0.0001 #456.6 #0.1
  ,C_slo		= 0.0001 #5367.9 #0.1
  ,C_pas		= 0.0001 #7167.4 #0.1
  ,N_am		  = 335.5 / ini_cn_rat_abgmet
  ,N_as		  = 9.1 / ini_cn_rat_abgstr
  ,N_bm		  = 150.7 / ini_cn_rat_blwmet
  ,N_bs		  = 44 / ini_cn_rat_blwstr
  ,N_fw		  = 118.7 / ini_cn_rat_fw
  ,N_acw		= 401.6 / ini_cn_rat_acw
  ,N_bcw		= 334.6 / ini_cn_rat_bcw
  ,N_srfmic = 130.2 / ini_cn_rat_srfmic
  ,N_mic		= 456.6 / ini_cn_rat_mic
  ,N_slo		= 5367.9 / ini_cn_rat_slo
  ,N_pas		= 7167.4 / ini_cn_rat_pas
  ,N_ino		= 10
)
poolNames=names(iv)
state_variable_names=names(iv)
timeSymbol='t'
times = seq(0, 15000, by = 100)
# times = c(0,10000)

# now we formulate the same Model as (possibly) nonlinear Model
# which does not change the solution but hides the information
# of linearity from SoilR
names=names(internal_fluxes)
#print(names)
intfs=InternalFluxList_by_PoolName(internal_fluxes)
#  
ofs=OutFluxList_by_PoolName(out_fluxes)
ifs=InFluxList_by_PoolName(input_fluxes_const)
obn<- UnBoundNonLinDecompOp_by_PoolNames(
  internal_fluxes=intfs
  ,out_fluxes=ofs
  ,timeSymbol='t'
)
mod=GeneralModel(
      t=times
      ,A=obn
      ,ivList=iv 
      ,inputFluxes=ifs
      ,timeSymbol='t'
)
sol=getC(mod)
co2=getReleaseFlux(mod)
round(sol, digits = 4)
df=cbind(sol,times)

colnames(sol)<-poolNames
  for ( i in 1:ncol(sol)){
    plot(x=times,y=sol[,i],ylab=paste('Carbon Stocks (gC*m-2) ','sol',i), type = 'l', cex.axis = 1.3)
  }

c_n_stocks = data.frame('times' = times, sol)

# Plot results
dir_out = '/home/corrado/SoilR-exp/pkg/inst/tests/src/'

