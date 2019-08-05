require("pkgload")
source("../testhelpers.R")
source("corradoFluxLists.R")
pkgload::load_all('../../../')
# formulate a linear autonomous model
# (constant matrix and constant influxes)
iv<-c(
    C_am 		=3.1
    ,C_as		=3.2
    ,C_bm		=3.3
    ,C_bs		=3.4
    ,C_fw		=3.5
    ,C_acw		=3.6
    ,C_bcw		=3.7
    ,C_mic		=3.8
    ,C_slo		=3.9
    ,C_pas		=3.11
    ,N_am		=3.12
    ,N_as		=3.13
    ,N_bm		=3.14
    ,N_bs		=3.15
    ,N_fw		=3.16
    ,N_acw		=3.17
    ,N_bcw		=3.18
    ,N_mic		=3.19
    ,N_slo		=3.121
    ,N_pas		=3.21
    ,N_ino		=3.31
)
poolNames=names(iv)
state_variable_names=names(iv)
timeSymbol='t'
times=seq(0,10,by=0.05)

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
df=cbind(sol,times)

colnames(sol)<-poolNames
for ( i in 1:ncol(sol)){
    plot(x=times,y=sol[,i],ylab=paste('sol',i))
}
# example 
name='C_am->C_mic'
f=internal_fluxes[[name]]
colnames(df)=c(poolNames,timeSymbol)
flux_values=as.numeric(lapply(
  1:length(times)
  ,function(i){t=times[i]
    flux=do.call("f",as.list(df[i,formalArgs(f)]))
  }
))
plot(times,flux_values,ylab=name)
