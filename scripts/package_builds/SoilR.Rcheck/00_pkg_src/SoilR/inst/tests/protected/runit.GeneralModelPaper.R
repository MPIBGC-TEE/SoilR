test.GeneralModelPaper <- function(){
	#####################################################################################
####                      Reproducibility code for                               ####
# A general mathematical framework for representing soil organic matter dynamics    #
#####################################################################################
###############################################       Prepared by C. A. Sierra
###################################################################### February 2015

# The following packages are required. 
library(SoilR)
library(lattice)

#####################################################################################
# Analysis of eigenvalues for models with feedback structure

#Two-pool case
lambda=function(x1,x2){
  A=-1*diag(c(x1,x2))
  A[2,1]<-runif(1,0,1)*x1
  A[1,2]<-runif(1,0,1)*x2
  
  cs=colSums(A)
  if(any(cs>0)) stop(paste("Wrong matrix; A =", A))
  eigen(A,only.values=TRUE)$values
}

n=2000 # Number of models
l2=t(mapply(x1=runif(n,0,3),x2=runif(n,0,3),FUN=lambda))

#Three-pool case
lambda3=function(k1,k2,k3){
  alpha21=runif(1,0,1)
  alpha31=runif(1,0,(1-alpha21))
  alpha12=runif(1,0,1)
  alpha32=runif(1,0,(1-alpha12))
  alpha13=runif(1,0,1)
  alpha23=runif(1,0,(1-alpha13))

  M=-1*diag(c(k1,k2,k3))
  M[2,1]=alpha21*k1
  M[3,1]=alpha31*k1
  M[1,2]=alpha12*k2
  M[3,2]=alpha32*k2
  M[1,3]=alpha13*k3
  M[2,3]=alpha23*k3
  
  cs=colSums(M)
  if(any(cs>0)) {
    print("Wrong matrix")
    print(M)
    stop()
  }
  return(eigen(M,only.values=TRUE)$values)
}

l3=t(mapply(k1=runif(n,0,3),k2=runif(n,0,3),k3=runif(n,0,3),FUN=lambda3))

#Four-pool case
lambda4=function(k1,k2,k3,k4){
  alpha21=runif(1,0,1)
  alpha31=runif(1,0,(1-alpha21))
  alpha41=runif(1,0,(1-(alpha21+alpha31)))
  alpha12=runif(1,0,1)
  alpha32=runif(1,0,(1-alpha12))
  alpha42=runif(1,0,(1-(alpha12+alpha32)))
  alpha13=runif(1,0,1)
  alpha23=runif(1,0,(1-alpha13))
  alpha43=runif(1,0,(1-(alpha13+alpha23)))
  alpha14=runif(1,0,1)
  alpha24=runif(1,0,(1-(alpha14)))
  alpha34=runif(1,0,(1-(alpha14+alpha24)))
  
  M=-1*diag(c(k1,k2,k3,k4))
  M[2,1]=alpha21*k1
  M[3,1]=alpha31*k1
  M[4,1]=alpha41*k1
  M[1,2]=alpha12*k2
  M[3,2]=alpha32*k2
  M[4,2]=alpha42*k2
  M[1,3]=alpha13*k3
  M[2,3]=alpha23*k3
  M[4,3]=alpha43*k3
  M[1,4]=alpha14*k4
  M[2,4]=alpha24*k4
  M[3,4]=alpha34*k4
  
  cs=colSums(M)
  if(any(cs>0)) {
    print("Wrong matrix")
    print(M)
    stop()
  }
  return(eigen(M,only.values=TRUE)$values)
  
}

l4=t(mapply(k1=runif(n,0,3),k2=runif(n,0,3),k3=runif(n,0,3),k4=runif(n,0,3),FUN=lambda4))

#Figure 2
plot(l3,pch=20,xlab="Re",ylab="Im",ylim=c(-5,5))
points(l4,pch=20,col=4)
points(as.vector(l2),rep(0,length(l2)),pch=".",col=2,cex=2)
abline(0,1,lty=3)
abline(0,-1,lty=3)
legend("topright",c("dim(A) = 2","dim(A) = 3","dim(A) = 4"),pch=c(20,19,19),col=c(2,1,4),bty="n")

## Oscillations: damping ratios
zeta3=sqrt(1/(1+(Im(l3)/Re(l3))^2))
zeta4=sqrt(1/(1+(Im(l4)/Re(l4))^2))

#Figure 3
par(mfrow=c(2,1))
hist(zeta3[,1],main="dim(A) = 3",xlab=expression(zeta))
hist(zeta4[,1],main="dim(A) = 4",xlab=expression(zeta))
par(mfrow=c(1,1))

#####################################################################################
# Oscillations in nonlinear models

# Two pool model
n=1000
eps=runif(n,0,1) #0.6
ub=runif(n,0,10) #4.38
Fnpp=345

Ts=runif(n,0,35) #15
V=0.000008*exp(5.47+0.063*Ts)*(24*365)
xvs=runif(n,1,10)
Vs=V*xvs

K=10*exp(3.19+0.007*Ts)*1000
xks=runif(n,0.1,1)
Ks=K*xks

A=(Fnpp/((eps^(-1))-1)*ub)*(Vs/Ks)*(1-(ub/(eps*Vs)))^2
im=c(0.5*sqrt(((4*(1-eps)*ub)/A)-1))
Ev1=-A*complex(real=c(0.5,0.5), imaginary=c(im,-im))
zeta=(-1*Re(Ev1))/sqrt(Re(Ev1)^2 + Im(Ev1)^2)  

## Three-pool model
xvl=runif(n,1,10)
xkl=runif(n,0.1,1)
Vl=V*xvl
Kl=K*xkl

B1=(Fnpp/((eps^(-1))-1)*ub)*(Vl/Kl)*(1-((ub*((eps^(-1)-1)))/(Vl)))^2
B2=(Fnpp/((eps^(-1))-1)*ub)*(Vs/Ks)*(1-(ub/(Vs)))^2
im2=c(0.5*sqrt(((4*(1-eps)*ub)/B1)-1))
Ev2=complex(real=c(-B1/2,-B1/2),imaginary=c(-B1*im2,-B1*(-im2)))
zeta2=(-1*Re(Ev2))/sqrt(Re(Ev2)^2 + Im(Ev2)^2)  

# Figure 4
plot(Ev1,pch=20,xlab="Re",ylab="Im",ylim=c(-10,10),col=4)
points(Ev2,col=2,pch=20)
abline(v=0,lty=2)
abline(h=0,lty=2)
abline(0,1,lty=2)
abline(0,-1,lty=2)
legend(x=-4,y=-8,c("Two-pool M-M", "Three-pool M-M"),pch=20,col=c(4,2),bty="n")

#Figure 5
par(mfrow=c(2,1))
hist(zeta,xlab=expression(zeta),main="Two-pool M-M")
hist(zeta2,xlab=expression(zeta),main="Three-pool M-M")
par(mfrow=c(1,1))

# Figure 6
par(mfrow=c(2,2),mar=c(5,4,2,1))
plot(eps,zeta[1:1000],xlab=expression(alpha),ylab=expression(zeta),pch=20)
plot(ub,zeta[1:1000],ylab=expression(zeta),xlab=expression(k[b]),pch=20)
plot(Ks,zeta[1:1000],ylab=expression(zeta),xlab=expression(K[M]),pch=20)
plot(Vs,zeta[1:1000],xlab=expression(k[s]),ylab=expression(zeta),pch=20)
par(mfrow=c(1,1))

#####################################################################################
#Warming-acclimation

times=seq(0,100,0.1)

#Linear model
T0=matrix(c(-1,0.5,0,0.1,-1,0.2,0,0.1,-1),3,3)
T1=T0*0.5; diag(T1)<-(-1)

K0=diag(c(0.9,0.5,0.1))
K1=K0*2

A1=T0%*%K0 #Control
A2=T0%*%K1 #Decay rates double (+k)
A3=T1%*%K0 #Transfer coeff. halve (-alpha)
A4=T1%*%K1 #Decay rates double and transfer coeff. halve (+k, -alpha)

I=c(345,0,0)

#Steady-state pool sizes
C1=solve(-A1,I)
C2=solve(-A2,I)
C3=solve(-A3,I)
C4=solve(-A4,I)

W1=ThreepFeedbackModel(t=times,ks=diag(A1),a21=A1[2,1],a12=A1[1,2],a32=A1[3,2],a23=A1[2,3],C0=C1,In=I[1])
W2=ThreepFeedbackModel(t=times,ks=diag(A2),a21=A2[2,1],a12=A2[1,2],a32=A2[3,2],a23=A2[2,3],C0=C1,In=I[1])
W3=ThreepFeedbackModel(t=times,ks=diag(A3),a21=A3[2,1],a12=A3[1,2],a32=A3[3,2],a23=A3[2,3],C0=C1,In=I[1])
W4=ThreepFeedbackModel(t=times,ks=diag(A4),a21=A4[2,1],a12=A4[1,2],a32=A4[3,2],a23=A4[2,3],C0=C1,In=I[1])

Rt1=getReleaseFlux(W1)
Rt2=getReleaseFlux(W2)
Rt3=getReleaseFlux(W3)
Rt4=getReleaseFlux(W4)

#Nonlinear case
yrs=seq(0,1000,by=1)

MMctrl=TwopMMmodel(t=yrs,ival=c(39550.4939750971,315.068493150685) ,kb=4.38, ks=42.82, Km=269774.1, ADD=345, r=0.2)
Rtctrl=getReleaseFlux(MMctrl)

MMpk=TwopMMmodel(t=yrs,ival=c(39550.4939750971,315.068493150685) ,kb=4.38*2, ks=42.82*2, Km=269774.1, ADD=345, r=0.2)
Rtpk=getReleaseFlux(MMpk)

MMma=TwopMMmodel(t=yrs,ival=c(39550.4939750971,315.068493150685) ,kb=4.38, ks=42.82, Km=269774.1, ADD=345, r=0.3)
Rtma=getReleaseFlux(MMma)

MMpkma=TwopMMmodel(t=yrs,ival=c(39550.4939750971,315.068493150685) ,kb=4.38*2, ks=42.82*2, Km=269774.1, ADD=345, r=0.3)
Rtpkma=getReleaseFlux(MMpkma)

#Figure 7
par(mfrow=c(2,1),mar=c(4,5,1,2))
plot(times,rowSums(Rt1),type="l",ylim=c(0,1000),ylab=expression(paste("Respiration (g C ",m^-2, " ", yr^-1,")")),xlab="Years",xlim=c(0,20))
lines(times,rowSums(Rt2),col=2)
lines(times,rowSums(Rt3),col=3)
lines(times,rowSums(Rt4),col=4)
legend("topright",c("Control","+Decay", "-CUE",
                    "+Decay, -CUE"),lty=1,col=1:4,bty="n")

plot(yrs,rowSums(Rtctrl),type="l",xlab="Years",ylab=expression(paste("Respiration (g C ",m^-2, " ",yr^-1,")")),ylim=c(0,3500))
lines(yrs,rowSums(Rtpk),col=2)
lines(yrs,rowSums(Rtma),col=3)
lines(yrs,rowSums(Rtpkma),col=4)
# legend("topright",c("Control",expression(+k[i]), expression(-alpha[ij]),
#                     expression(paste(+k[i],", ",-alpha[ij]))),lty=1,col=1:4,bty="n")
par(mfrow=c(1,1))

################################################################################
# BIBO and ISS

yr=seq(0,500,1/12)
In=data.frame(time=yr,In=rnorm(length(yr),10,1))
ks=c(1/2,1/5,1/10)
C0=c(10,10,10)
set.seed(3.5) #Set random number generator for reproducibility
Temp=rnorm(length(yr),25,5)
xi=data.frame(time=yr,xi=fT.Q10(Temp,Q10=1.4))

M=ThreepFeedbackModel(t=yr,ks=ks,a21=0.3*ks[1],a12=0.1*ks[2],a32=0.4*ks[2],a23=0.3*ks[3],C0=C0,In=In,xi=xi)
Rt=getReleaseFlux(M)
Ct=getC(M)

#Figure 7
par(mfrow=c(2,1),mar=c(0,5,0.5,1))
Cmax=max(rowSums(Ct))
Cmin=min(rowSums(Ct))
plot(In,ylim=c(0,Cmax),xlim=c(0,20), type="n",
     ylab="C stock [mass]",xlab="",xaxt="n")
rect(min(In[,2]),Cmin,max(In[,2]),Cmax,density=NA,col="gray")
points(In[,2],rowSums(Ct),pch=20)
legend("topright","a",bty="n")

par(mar=c(4,5,0,1))
Rmax=max(rowSums(Rt))
Rmin=min(rowSums(Rt))
plot(In,ylim=c(0,Rmax),xlim=c(0,20), type="n",
     ylab=expression(paste("Release flux [mass ", time^-1,"]")),xlab=expression(paste("Input flux [mass ", time^-1,"]")))
rect(min(In[,2]),Rmin,max(In[,2]),Rmax,density=NA,col="gray")
points(In[,2],rowSums(Rt),pch=20)
legend("topright","b",bty="n")
par(mfrow=c(1,1))

set.seed(seed=NULL) #Returns random number generator to random seed

############################
# ISS

tm=seq(0,100)
u=seq(0,100)
x0=20
g=expand.grid(time=tm,inputs=u)
g$stock=x0*exp(-0.06*g$time)+(g$inputs)^0.7

#Figure 8
wireframe(stock~time*inputs,data=g,drape=TRUE,colorkey=FALSE,shade=TRUE,xlab="Time",ylab="max(Inputs)",zlab="C stock")

#####################################################################################
}
