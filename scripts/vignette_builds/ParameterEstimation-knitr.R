#!/usr/bin/Rscript
library('pkgload')
pkgload::load_all('../../pkg')
library(FME)
library(MASS)
library(lattice)
#BorealCO2=eCO2
#names(BorealCO2)<-c("time","eCO2","eCO2sd")
#
##C concentration * 450 g soil
#Ctotal=mean(c(0.04683975, 0.04703255, 0.04687287))*450 
##Changed units
#BorealCO2=data.frame(
#  time=BorealCO2[,1],
#  BorealCO2[,2:3]*1e-06*Ctotal
#) 
#cols=c('black','blue','red')
## plot the data
#plot(
#    BorealCO2[,1:2]
#    ,xlab="Days"
#    ,ylab="Evolved CO2 (mgC g-1 soil day-1)"
#    ,ylim=c(0,9e-04)
#  #  ,ylim=c(0,50)
#)
#add_data_plot<-function(){
#    points(BorealCO2)
#    arrows(
#        BorealCO2[,1]
#        ,BorealCO2[,2]-BorealCO2[,3]
#        ,BorealCO2[,1]
#        ,BorealCO2[,2]+BorealCO2[,3]
#        ,code=3
#        ,angle=90
#        ,length=0.1
#    )
#}
#add_data_plot()
### -------------- model 1 ----------------------------------------------------------
#eCO2P1=function(pars){
# At=ConstLinDecompOp(
#     out_flux_rates=c("1"=pars[[1]])
#     ,numberOfPools =1
#
# )
# mod=GeneralModel(
#    t=BorealCO2[,'time']
#   ,A=At
#   ,ivList=c(Ctotal)
#   ,inputFluxes=0
#   #,pass=TRUE
# )
# Rt<-getReleaseFlux(mod)
# return(data.frame(time=BorealCO2[,'time'],eCO2=rowSums(Rt)))
#}
#
#
##r=c(1:4,34:35)
#eCO2P1cost=function(pars){
# return(
#   modCost(
#     model=eCO2P1(pars)
#     ,obs=BorealCO2
#     ,err="eCO2sd"
#   )
# )
#}
#
#
#inipars=c( 1/2000 )
#upP1=c(1)
#loP1=c(0)
#
#eCO2P1fit=modFit(
#   f=eCO2P1cost
#   ,p=inipars
#   #,method="Marq"
#   ,upper=upP1
#   ,lower=loP1
#)
#
#lines(eCO2P1(eCO2P1fit$par))
#
#### ------------------- model 2--------------------------------------------
#eCO2P1a=function(pars){
#  At=ConstLinDecompOp(
#      out_flux_rates=c("1"=pars[[1]])
#      ,numberOfPools =1 
#  ) 
#  mod=GeneralModel(
#     t=BorealCO2[,'time']
#    ,A=At
#    ,ivList=c(Ctotal*pars[2])
#    ,inputFluxes=0
#    #,pass=TRUE
#  )
#  Rt<-getReleaseFlux(mod)
#  return(data.frame(time=BorealCO2[,'time'],eCO2=rowSums(Rt)))
#}
#
#eCO2P1acost=function(pars){
#  return(
#    modCost(
#      model=eCO2P1a(pars)
#      ,obs=BorealCO2
#      ,err="eCO2sd"
#    )
#  )
#}
#upP1a=c(1,1)
#loP1a=c(0,0)
#
#initPars=c(0.023284236, 0.001191257 )
#eCO2P1afit=modFit(
#    f=eCO2P1acost
#    ,p=initPars
#    ,method="Marq"
#    ,upper=upP1a
#    ,lower=loP1a
#)
#print(initPars)
#print(eCO2P1afit$par)
#
#
#lines(eCO2P1a(eCO2P1afit$par),col=cols[2])
#var0=eCO2P1afit$var_ms_unweighted
#
# eCO2P1amcmc=modMCMC(
#    f=eCO2P1acost
#   ,p=eCO2P1afit$par
#   ,niter=3000
#   ,jump=NULL
#   #,var0=var0
#   ,wvar0=NULL
#   ,updatecov=10
#   ,upper=upP1a
#   ,lower=loP1a
# )
# summary(eCO2P1amcmc)
# sR1=sensRange(func=eCO2P1a, parInput=eCO2P1amcmc$par)
# #pairs(eCO2P1amcmc)
# predRange=sensRange(func=eCO2P1a, parInput=eCO2P1amcmc$par)
# plot(
#  summary(predRange)
#  ,ylim=c(0,9e-04)
#  ,xlab="Days",
#  ylab="Evolved CO2 (g C day-1)"
#  ,main=""
#  )
# add_data_plot()
### --------- model 3 ------------------------------
#
#eCO2P2=function(pars){
#   At=ConstLinDecompOp(
#       out_flux_rates=c("1"=pars[[1]],"2"=pars[[2]])
#       ,numberOfPools =2
#   )
#   gamma=pars[[3]]
#   mod=GeneralModel(
#      t=BorealCO2[,'time']
#     ,A=At
#     ,ivList=Ctotal*c(gamma,1-gamma)
#     ,inputFluxes=0
#     ,pass=TRUE
#   )
#   Rt<-getReleaseFlux(mod)
#   return(data.frame(time=BorealCO2[,'time'],eCO2=rowSums(Rt)))
#}
##r=c(1:4,34:35)
#eCO2P2cost=function(pars){
#  return(
#    modCost(
#      model=eCO2P2(pars)
#      ,obs=BorealCO2
#      ,err="eCO2sd"
#    )
#  )
#}
### ------------------------------------------------------------------------
##initPars=c(.02,.0001,0.999)
#upP2=c(.1,.1,1)
#loP2=c(0,0,0)
#n=5
#rsP2<-lapply(1:3,function(i){loP2[i]+(1:n)/n*(upP2[i]-loP2[i])})
#require('sets')             
#
#combinations=set_cartesian(rsP2[[1]],rsP2[[2]],rsP2[[3]])
#for (tup in combinations){
#    eCO2P2fit=modFit(
#        f=eCO2P2cost
#        ,p=as.numeric(tup)
#        #,p=initPars
#        #,method="Marq"
#        ,upper=upP2
#        ,lower=loP2
#    )
#    print(initPars)
#    print(eCO2P2fit$par)
#    ## ----echo=TRUE-----------------------------------------------------------
#    lines(
#      eCO2P2(eCO2P2fit$par)
#      ,col=cols[3]
#    )
#}
#
#### ----echo=FALSE,results='hide'-------------------------------------------
#var0=eCO2P2fit$var_ms_unweighted
#
#eCO2P2mcmc=modMCMC(
#   f=eCO2P2cost
#  ,p=eCO2P2fit$par
#  ,niter=3000
#  ,jump=NULL
#  #,var0=var0
#  ,wvar0=NULL
#  ,updatecov=10
#  ,upper=upP2
#  ,lower=loP2
#)
#
#
### ------------------------------------------------------------------------
#summary(eCO2P2mcmc)
#sR1=sensRange(func=eCO2P2, parInput=eCO2P2mcmc$par)
#
#
### ----echo=TRUE-----------------------------------------------------------
### pairs(eCO2P2mcmc)
#
#
### ----echo=TRUE-----------------------------------------------------------
#plot(
##lines(
#  summary(
#    sensRange(
#      func=eCO2P2
#      ,parInput=eCO2P2mcmc$par
#    )
#  )
#  ,ylim=c(0,9e-04)
#  ,xlab="Days"
#  ,ylab="Evolved CO2 (g C day-1)"
#  ,main=""
#  ,col=cols[3]
#)
##points(BorealCO2)
##arrows(BorealCO2[,1],BorealCO2[,2]-BorealCO2[,3],BorealCO2[,1],
##       BorealCO2[,2]+BorealCO2[,3],code=3,angle=90,length=0.1)
#
######################################################################
######################################################################
######################################################################
######################################################################
##### ------------------------------------------------------------------------
##
# ----echo=FALSE----------------------------------------------------------
plot(D14C~Year,data=HarvardForest14CO2,
    ylab=expression(paste(Delta^14,"C ","(\u2030)")))


## ------------------------------------------------------------------------
time=C14Atm_NH$YEAR
t_start=min(time)
t_end=max(time)


## ------------------------------------------------------------------------
inputFluxes=InFlux( c(270,150,0))


## ------------------------------------------------------------------------
C0=c(390,220+390+1376,90+1800+560)


## ------------------------------------------------------------------------
Fc=BoundFc(C14Atm_NH,lag=0,format="Delta14C")
np=3
Mod1<-function(ks,pass=TRUE){
	At=ConstLinDecompOp(
	    internal_flux_rates=c(
	    	"1_to_2"=ks[[4]]
	  		#,"1_to_3"=ks[[5]]
	  	)
	    ,out_flux_rates=c(
	    	"1"=ks[[1]]
	  		,"2"=ks[[2]]
	  	  	,"3"=ks[[3]]
	     )
	    ,numberOfPools = np
	
	)
	mod=GeneralModel_14(
	  t=time,
	  A=At,
	  ivList=C0,
	  initialValF=ConstFc(rep(0,3),"Delta14C"),
	  inputFluxes=inputFluxes,
	  inputFc=Fc,
	  pass=TRUE
	)
	R14t=getF14R(mod)
	return(data.frame(time=time,R14t=R14t))
}


## ------------------------------------------------------------------------
DataR14t=cbind(
	time=HarvardForest14CO2[,1]
	,R14t=HarvardForest14CO2[,2]
)
#	, sd=sd(HarvardForest14CO2[,2]))


## ----echo=FALSE,cache=TRUE,results='hide'--------------------------------
#Create the cost function
R14tCost <- function(pars){
	R14t <- Mod1(pars)
	return(
		modCost(
			model=R14t
			,obs=DataR14t
			#,err="sd"
		)
	)
}

#Fit the model to the observed data given some initial value for the parameters
nk=4
up=rep(1,nk)
lo=rep(0,nk)
Fit <- modFit(
	f=R14tCost
	,p=c(.01,.02,.03,.9)
	,lower=lo
	,upper=up
)

var0 <- Fit$var_ms_unweighted
cov0 <- summary(Fit)$cov.scaled
MCMC <- modMCMC(
    f=R14tCost
   ,p = Fit$par
   ,niter = 3000
   ,jump = NULL
   ,covscale= cov0
   ,var0 = var0
   ,wvar0 = 0
   ,lower=lo
   ,upper=up
)




## ----echo=TRUE-----------------------------------------------------------
pairs(MCMC)#,nsample=floor(number_of_iterations/4))
#
#
### ----echo=TRUE-----------------------------------------------------------
##The sensitivity range is calculated from the output of the MCMC
#sR=sensRange(func=Mod1, parInput=MCMC$par)
#par(mar=c(5,5,4,1))
#plot(summary(sR),xlim=c(1950,2010),ylim=c(0,1000),xlab="Year",
#     ylab=expression(paste(Delta^14,"C ","(\u2030)")),main="")
#points(DataR14t,pch=20)
#lines(C14Atm_NH,col=4)

