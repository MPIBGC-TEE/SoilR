# Linear approach

library(SoilR)

times=seq(0,20,by=0.1)
ks=c(k1=0.8,k2=0.00605)
alpha21<-0.2; alpha12<-0.5
C0=c(C10=5,C20=5)

ABG_In<-5

Feedback=TwopFeedbackModel(t=times,ks=ks,a21=alpha21*ks[1],a12=alpha12*ks[2],C0=C0, In=ABG_In)

A1<-diag(-ks,2,2)
A1[2,1]<-alpha21*ks[1]
A1[1,2]<-alpha12*ks[2]

lyrs<-3
Blyr<-diag(1,lyrs,lyrs)

BM<-Blyr%x%A1

d<-0.05 #downward transport rate
u<-0.01 #upward transport rate

H<-matrix(c(-d, 0, 0,
           d, -d, 0,
           0, d, -d), 3,3, byrow=TRUE)

h<-matrix(c(1,0,
            0,1), 2,2,byrow=TRUE)

H%x%h

G<-matrix(c(0, u, 0,
            0, -u, u,
            0, 0, -u),3,3, byrow=TRUE)
g<-matrix(c(1,0,
            0,1),2,2, byrow=TRUE)

G%x%g

V<-(H%x%h)+(G%x%g)

M<-BM+V
In1<-c(ABG_In*0.8, ABG_In*0.2, rep(0,4))

xss<-solve(-M)%*%In1

Fatm<-BoundFc(map=Graven2017[,1:2], lag=0, format="Delta14C")

DepthModel<-Model_14(t=Graven2017[,1],A=M,ivList=as.numeric(xss), initialValF=ConstFc(rep(0,6),"Delta14C"), 
                     inputFluxes=In1, inputFc = Fatm)

C14t<-getF14C(DepthModel)
P14t<-getF14(DepthModel)
Ct<-getC(DepthModel)

tau=seq(0,200)
SA<-systemAge(A=M, u=In1, a=tau)

plot(Graven2017[,1:2], type="l", col=4)
lines(Graven2017[,1], C14t)

matplot(Graven2017[,1], Ct, type="l", lty=1, col=1:6)

collect_by_layer<-matrix(c(1,0,0,
                  1,0,0,
                  0,1,0,
                  0,1,0,
                  0,0,1,
                  0,0,1),nrow=6, ncol=3)

collect_by_pool<-matrix(c(1,0,
                          0,1,
                          1,0,
                          0,1,
                          1,0,
                          0,1), nrow=6, ncol=2, byrow = TRUE)

collect_by_layer<-function(Ct,P14t, nlayer, npool){
  AFM=AbsoluteFractionModern_from_Delta14C(P14t)
  collect<-diag(nrow=nlayer,ncol=nlayer)%x%matrix(c(1,1),nrow=npool,ncol=1) 
  Cl<-Ct%*%collect
  AFM_Ct<-AFM*Ct
  F14t_l<-(AFM_Ct%*%collect)/Cl
  return(list(F14C=Delta14C_from_AbsoluteFractionModern(F14t_l), C=Cl))
}

collect_by_pool<-function(Ct,P14t, nlayer, npool){
  AFM=AbsoluteFractionModern_from_Delta14C(P14t)
  collect<-matrix(1, ncol=1, nrow=nlayer)%x%diag(1,nrow=npool,ncol=npool)
  Cp<-Ct%*%collect
  AFM_Ct<-AFM*Ct
  F14t_p<-(AFM_Ct%*%collect)/Cp
  return(list(F14C=Delta14C_from_AbsoluteFractionModern(F14t_p), C=Cp))
}

L14t<-collect_by_layer(Ct,P14t,nlayer=3,npool=2)
matplot(Graven2017[,1],L14t$F14C, type="l", lty=1, col=2:4)
legend("topleft",c("Layer 1", "Layer 2", "Layer 3"), lty=1, col=2:4, bty="n")
lines(Graven2017[,1:2])

PP14t<-collect_by_pool(Ct,P14t,nlayer=3,npool=2)
matplot(Graven2017[,1],PP14t$F14C, type="l", lty=1, col=2:3)
legend("topleft",c("Pool 1", "Pool 2"), lty=1, col=2:3, bty="n")
lines(Graven2017[,1:2])

plot(as.numeric(tail(L14t$C, 1)), as.numeric(tail(L14t$F14C, 1)))

plot(c(1:3), as.numeric(tail(L14t$F14C, 1)))

