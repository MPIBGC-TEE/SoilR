library(SoilR)
# Step 1. Prepare what you need for a SoilR pool model
AtmF14<-bind.C14curves(prebomb=IntCal20,postbomb=Hua2021$NHZone2,time.scale="AD")
years<-seq(0,2019, by=1)
fs<-0.17 
k1<-0.268
k2<-0.0031
k3<-1e-05
a21<-0.05*k1 # 50% of C decomposed from pool 1 moves to pool 2
a32<-0.04*k2 # 10% of C decomposed from pool 2 moves to pool 1
initialC<-c(1.24,5.45,4.15) # You can put any number here. They will be ignored because steady-state will be assumed
initialF14<-c(0,0,0) # Same here. For the depth simulation the age of the steady-state carbon will be used to set initial F14C


#Step 2. Create SoilR pool model
PoolModel=ThreepSeriesModel14(t=years,ks=c(k1,k2,k3),C0=initialC, F0_Delta14C=initialF14,In=fs,a21=a21,a32=a32,inputFc= AtmF14[,1:2])

# Step 3. Create Vertical Transport model
depthInterval<-0.01 # in m 
depthLayers<-seq(0,1,by=depthInterval)
fd0=0.48; fd1=3.1 #3.1
rootInputs<-fd0*exp(-fd1*depthLayers)*depthInterval
#rootInputs<-rep(0,length(depthLayers))
downwardTransferRate<-depthInterval*0.42/1000 # 0.42 mm -> m multiplied by depth interval
upwardTransferRate<-0 
xi<-exp(-1*depthLayers)

target_yr<-which(years==2019)

EB95<-VTLM(Model=PoolModel, lyrs=depthLayers, latIn = rootInputs, vrm=xi, d=downwardTransferRate, 
           u=upwardTransferRate, ivalF14 = "model")

sol<-solveVTLM(VTLM=EB95, npool=3, nlayer=length(depthLayers))

par(mfrow=c(2,1), mar=c(4,4,1,1))
plot(sol$C14[,1, target_yr], -depthLayers, type="l", col=1, xlim=range(sol$C14[,,target_yr]), xlab="D14C", ylab="Depth")
lines(sol$C14[,2, target_yr], -depthLayers, col=2)
lines(sol$C14[,3, target_yr], -depthLayers, col=3)

plot(sol$C[,1, target_yr], -depthLayers, type="l", col=1, xlim=c(0,max(sol$C[,,target_yr])), xlab="C content", ylab="Depth")
lines(sol$C[,2, target_yr], -depthLayers, col=2)
lines(sol$C[,3, target_yr], -depthLayers, col=3)
par(mfrow=c(1,1))

plot(rowSums(sol$C[,,target_yr]), -depthLayers, type="l", xlab="Total C", ylab="Depth", bty="n")

tau<-seq(0,100,by=1)
TT<-transitTime(A=EB95@mat@mat, u=EB95@inputFluxes@map, a=tau)
plot(tau, TT$transitTimeDensity, type="l")

Cdt<-getC(EB95)
C14dt<-getF14(EB95)
L14t<-collect_by_layer(Cdt,C14dt,nlayer=length(depthLayers),npool=3)
P14t<-collect_by_pool(Cdt,C14dt,nlayer=length(depthLayers),npool=3)

plot(tail(L14t$F14C, 1), -depthLayers, type="l", xlab="Delta14C", ylab="Depth")

plot(tail(L14t$C, 1), tail(L14t$F14C, 1), type="l", xlab="C stock", ylab="D14C")


plot(AtmF14[,1:2], xlim=c(0,2020), ylim=c(-1000, 1000), type="l", lty=1)
matlines(years,P14t$F14C, lty=1, col=2:11) # Find a better color palette

