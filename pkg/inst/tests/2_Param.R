### R code from vignette source 'ParameterEstimation.Rnw'

###################################################
### code chunk number 1: ParameterEstimation.Rnw:58-64
###################################################
require('pkgload')
pkgload::load_all('..')
library(SoilR)
library(FME)
library(MASS)
library(lattice)
#
###################################################
### code chunk number 15: ParameterEstimation.Rnw:240-245
###################################################
time=C14Atm_NH$YEAR
t_start=min(time)
t_end=max(time)
inputFluxes=InFlux( c(270,150,0))


###################################################
### code chunk number 16: ParameterEstimation.Rnw:250-251
###################################################
C0=c(390,220+390+1376,90+1800+560) 


###################################################
### code chunk number 17: ParameterEstimation.Rnw:258-277
###################################################
Fc=BoundFc(C14Atm_NH,lag=0,format="Delta14C")
np=3
Mod1<-function(ks,pass=TRUE){
  print(class(ks))
  print(ks)
  At=ConstLinDecompOp(
       internal_flux_rates=c("1_to_2"=ks[[4]],"1_to_3"=ks[[5]])
      ,out_flux_rates=c("1"=ks[[1]],"2"=ks[[2]],"3"=ks[[3]])
      ,numberOfPools = np
    )
    #       c("1_to_2"
    #       matrix(nrow=3,ncol=3,byrow=TRUE,c(ks[1],0,0,
    #                                         ks[4],ks[2],0,
    #                                         ks[5],0,ks[3]))
  #) 
  mod=GeneralModel_14(
	t=time,
	A=At,
	ivList=C0,
	initialValF=ConstFc(rep(0,np),"Delta14C"),
	inputFluxes=inputFluxes,
	inputFc=Fc,
	pass=TRUE
  ) 
  R14t=getF14R(mod)
  return(data.frame(time=time,R14t=R14t))
}


###################################################
### code chunk number 18: ParameterEstimation.Rnw:281-284
###################################################
DataR14t=cbind(time=HarvardForest14CO2[,1],
               R14t=HarvardForest14CO2[,2],
               sd=sd(HarvardForest14CO2[,2]))


###################################################
### code chunk number 19: ParameterEstimation.Rnw:290-308
###################################################
#Create the cost function
R14tCost <- function(pars){
  R14t <- Mod1(pars)
  return(modCost(model=R14t,obs=DataR14t,err="sd"))
}

#Fit the model to the observed data given some random initial value for the parameters
nk=5 # number of parameters 
Fit <- modFit(f=R14tCost,p=runif(nk),lower=rep(0,nk))

 # Run an MCMC using the variance and covariance results from the previous optimization
 number_of_iterations=100
 var0 <- Fit$var_ms_unweighted
 #cov0 <- summary(Fit)$cov.scaled 
 MCMC <- modMCMC(
     f=R14tCost
    ,p = Fit$par
    ,niter = number_of_iterations
    ,jump = NULL
    ,var0 = var0
    ,wvar0 = 0
    ,lower=rep(0,nk)
    #,upper=c(0,0,0,1,1)
)
 
 
 
 
 ###################################################
 ### code chunk number 20: ParameterEstimation.Rnw:314-315
 ###################################################
 pairs(MCMC,nsample=floor(number_of_iterations/4))
 
 
# ###################################################
# ### code chunk number 21: ParameterEstimation.Rnw:324-329
# ###################################################
# #The sensitivity range is calculated from the output of the MCMC
sR=sensRange(func=Mod1, parInput=MCMC$pars)
# par(mar=c(5,5,4,1))
# plot(summary(sR),xlim=c(1950,2010),ylim=c(0,1000),xlab="Year",
#      ylab=expression(paste(Delta^14,"C ","(\u2030)")),main="")
# points(DataR14t,pch=20)
# lines(C14Atm_NH,col=4)
