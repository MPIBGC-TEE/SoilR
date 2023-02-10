# Example about how to use the LVTM fuction

library(SoilR)

source("VTLM.R") # Run this like if your working directory contains this file. Otherwise source the LVTM function from wheterever it is and make it available in the Global Environment
source("collect_by_layer.R") # Idem
source("collect_by_pool.R") # Idem

# Step 1. Prepare what you need for a SoilR pool model
years<-seq(1941.5,2019,by=0.5)
LitterInput<-700 
k1<-1/2
k2<-1/30
a21<-0.5*k1 # 50% of C decomposed from pool 1 moves to pool 2
a12<-0.1*k2 # 10% of C decomposed from pool 2 moves to pool 1
initialC<-c(0,0) # You can put any number here. They will be ignored because steady-state will be assumed
initialF14<-c(0,0) # Same here. For the depth simulation the age of the steady-state carbon will be used to set initial F14C
AtmF14<-Hua2021$NHZone2[,1:2] # Atmospheric radiocarbon curve for Northern Hemisphere zone 2 in Delta14C

#Step 2. Create SoilR pool model
PoolModel=TwopFeedbackModel14(t=years,ks=c(k1, k2),C0=initialC, 
                       F0_Delta14C=initialF14,In=LitterInput, a21=a21,a12=a12,inputFc=AtmF14)

# Step 3. Create Vertical Transport model
depthLayers<-c(1:10)
rootInputs<-rep(0,times=length(depthLayers))
downwardTransferRate<-0.05
upwardTransferRate<-0

TwopDepth<-VTLM(Model=PoolModel, lyrs=depthLayers, latIn = rootInputs, d=downwardTransferRate, u=upwardTransferRate)

# Step 4. Solve the vertical transport model and aggregate by pool and layer
Cdt<-getC(TwopDepth)
C14dt<-getF14(TwopDepth)

L14t<-collect_by_layer(Cdt,C14dt,nlayer=10,npool=2)
P14t<-collect_by_pool(Cdt,C14dt,nlayer=10,npool=2)

# Step 5. Plot the results
plot(AtmF14, type="l", lty=1)
matlines(TwopDepth@times,L14t$F14C, lty=1, col=2:11) # Find a better color palette

matplot(TwopDepth@times, L14t$C, type="l", lty=1, col=2:11)

plot(tail(L14t$C, 1), -depthLayers, xlab="C stock", ylab="Depth")
plot(tail(L14t$F14C, 1), -depthLayers, xlab="Delta14C", ylab="Depth")

plot(tail(L14t$C, 1), tail(L14t$F14C, 1), xlab="C stock", ylab="Delta14C")

plot(AtmF14, type="l", lty=1)
matlines(TwopDepth@times,P14t$F14C, lty=1, col=2:3)
legend("topright", c("Fast pool", "Slow pool"), lty=1, col=2:3)
