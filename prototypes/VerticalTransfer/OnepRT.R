# Test for reproducing a one pool model with transport combining SoilR and ReacTran
library(ReacTran)
library(SoilR)

# 1.
################################################################################
# Example model from ReacTran function tran.1D

# First order consumption of organic carbon (OC)
OC.model <- function (t = 0, OC, pars = NULL) {
  
  tran <- tran.1D(C = OC, flux.up = F.OC, D = Db.grid,
                  v = v.grid, VF = svf.grid, dx = grid)$dC
  reac <- - k*OC
  return(list(dCdt = tran + reac))
}

# Grid definition
L <- 10   # depth of sediment domain [cm]
N <- 100  # number of grid layers
grid <- setup.grid.1D(x.up = 0, L = L, N = N)

# Parameter values
F.OC    <- 25    # input flux organic carbon [micromol cm-2 yr-1]
por     <- 0.8   # porosity
Db      <- 10    # mixing coefficient sediment [cm2 yr-1]
v       <- 1     # advective velocity [cm yr-1]
v.grid   <- setup.prop.1D(value = v, grid = grid)
k       <- 1     # decay constant organic carbon [yr-1]

# Volume fractions 
svf.grid <- setup.prop.1D(value = (1-por), grid = grid)
Db.grid  <- setup.prop.1D(value = Db, grid = grid)

# Initial conditions + simulation OC
yini <- rep(0, length.out = N) 
OC   <- steady.1D(y = yini, func = OC.model, nspec = 1)

plot(OC, grid = grid$x.mid, xyswap = TRUE, main = "C concentration", 
     ylab = "depth [cm]", xlab = "", mfrow = NULL)
################################################################################

# 2.
################################################################################
# Implementation of a function that uses SoilR for the reaction term

OnepRT<-function(t=0,C0, pars=NULL){
  In=F.OC
  tran<-tran.1D(C = C0, flux.up = In, D = Db.grid,
                  v = v.grid, VF = svf.grid, dx = grid)$dC
  
  soilr.model<-Model(t=0,A=diag(-k,nrow=length(C0)),ivList = C0,inputFluxes = rep(0, length(C0)))
  reacFunc<-SoilR:::getRightHandSideOfODE(soilr.model)
  reac<-as.numeric(reacFunc(C0,t))
  
  return(list(dCdt = tran + reac))
}

################################################################################

# 3.
################################################################################
# Test that the two solutions are identical

SoilROPM<-OnepRT(t=0,C0=OC$y)
RTf<-OC.model(OC=OC$y)

plot(RTf$dCdt, SoilROPM$dCdt)

OC_RT   <- steady.1D(y = yini, func = OC.model, nspec = 1)
OC_SR   <- steady.1D(y = yini, func = OnepRT, nspec = 1)

plot(OC_RT$y, OC_SR$y)

################################################################################

# 4.
################################################################################
# Build a SoilR model with identical steady-state solution

SS<-tran.1D(C = OC_SR$y, flux.up = F.OC, D = Db.grid,
        v = v.grid, VF = svf.grid, dx = grid, full.output = TRUE)

B=-1*diag(SS$dC/OC_SR$y, nrow=length(SS$dC))
u=matrix(SS$flux[-100], ncol=1)

xss<--1*solve(B)%*%u

plot(xss, OC_SR$y) 
abline(0,1)
# Plot shows that there's not good agreement. The reconstructed model doesn't correspond well with the original model. This needs more work.
# Nevertheless, assuming one has a good reconstruction, one can go ahead and compute transit times and radiocarbon. 

tau<-seq(0,100,by=0.1)
TT<-transitTime(A=B, u,a=tau)

plot(tau,TT$transitTimeDensity, type="l")

Fatm<-BoundFc(map=Graven2017[,1:2], lag=0, format="Delta14C")

RM<-Model_14(t=Graven2017[,1],A=B,ivList = OC_SR$y,initialValF = ConstFc(rep(0,length(OC_SR$y)),"Delta14C"), 
             inputFluxes = as.numeric(u), inputFc = Fatm)
C14<-getF14C(RM)

plot(Graven2017[,1:2], type="l", col=4)
lines(Graven2017[,1],C14)
