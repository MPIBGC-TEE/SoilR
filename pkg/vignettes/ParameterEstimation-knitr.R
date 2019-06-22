## ------------------------------------------------------------------------
library(pkgload)
#pkgload::load_all('..')
library(FME)
library(MASS)
library(lattice)
BorealCO2=eCO2
names(BorealCO2)<-c("time","eCO2","eCO2sd")


## ----echo=TRUE-----------------------------------------------------------
# plot(BorealCO2[,1:2], xlab="Days", ylab="Evolved CO2 (mgC g-1 
#      soil day-1)",ylim=c(0,50))
# arrows(BorealCO2[,1],BorealCO2[,2]-BsrealCO2[,3],BorealCO2[,1], 
#        BorealCO2[,2]+BorealCO2[,3],code=3,angle=90,length=0.1)


## ----tidy=TRUE-----------------------------------------------------------
days=seq(0,35)
#C concentration * 450 g soil
Ctotal=mean(c(0.04683975, 0.04703255, 0.04687287))*450 
#Changed units
BorealCO2=data.frame(
  time=BorealCO2[,1],
  BorealCO2[,2:3]*1e-06*Ctotal
) 

eCO2func=function(pars){
  #k1=0.155
  k1=pars[1]
  #k2=pars[2]
  k2=1e-4*k1
  alpha=pars[2]
  #alpha=1e-3
  #gamma=pars[2]
  gamma=.0001
  #mod=TwopSeriesModel(
  mod=TwopFeedbackModel(
    t=days
    ,ks=c(k1,k2)
    ,a21=alpha
    ,a12=0
    ,C0=Ctotal*c(gamma,1-gamma)
    ,In=0
    ,pass=TRUE
  )
  Rt=getReleaseFlux(mod)
  return(data.frame(time=days,eCO2=rowSums(Rt)))
}



## ------------------------------------------------------------------------
eCO2cost=function(pars){
  modelOutput=eCO2func(pars)
  return(modCost(model=modelOutput, obs=BorealCO2, err="eCO2sd"))
}


## ------------------------------------------------------------------------
inipars=c(
    1/3
   #,.0001
   ,1/40
   #,1e-4
)
up=c(1,1)#,1)#,1,1)
lo=c(0,0)#,0)#,0,0)

fit=modFit(
    f=eCO2cost
    ,p=inipars
    #,method="Pseudo"
    #,method='Newton'
    ,upper=up
    ,lower=lo
)
print(inipars)
print(fit$par)
fitmod=eCO2func(fit$par)

plot(BorealCO2[,1:2], xlab="Days", ylab="Evolved CO2 (gC day-1)",ylim=c(0,9e-04))
arrows(BorealCO2[,1],BorealCO2[,2]-BorealCO2[,3],BorealCO2[,1],
       BorealCO2[,2]+BorealCO2[,3],code=3,angle=90,length=0.1)
lines(fitmod)
print(fit$par)
print(summary(fit))
# ----echo=FALSE,results='hide'-------------------------------------------
var0=fit$var_ms_unweighted

eCO2mcmc=modMCMC(f=eCO2cost, p=fit$par, niter=3000, jump=NULL
                 ,var0=var0
                 , wvar0=NULL, updatecov=100
                 , lower=lo, upper=up)

summary(eCO2mcmc)
sR1=sensRange(func=eCO2func, parInput=eCO2mcmc$par)
predRange=sensRange(func=eCO2func, parInput=eCO2mcmc$par)
plot(summary(predRange),ylim=c(0,9e-04),xlab="Days",
       ylab="Evolved CO2 (g C day-1)",main="")
  points(BorealCO2)
  arrows(BorealCO2[,1],BorealCO2[,2]-BorealCO2[,3],BorealCO2[,1],
         BorealCO2[,2]+BorealCO2[,3],code=3,angle=90,length=0.1)

