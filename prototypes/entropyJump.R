#
# vim:set ff=unix expandtab ts=2 sw=2:
entropyJump<-structure(
  function #Entropy rate per jump of a compartmental system
  ### Computes the entropy rate per jump and the entropy of the inifinite path
  (B,  ##<< A compartmental linear square matrix with cycling rates in the diagonal and transfer rates in the off-diagonal.
   u   ##<< A vector of mass inputs to the compartmental system
  )
  {
    
    beta=u/sum(u)
    zeta=-1*colSums(B)
    d=nrow(B)
    Q=matrix(0,nrow=d+1, ncol=d+1)
    Q[(1:d),(1:d)]=B; Q[(1:d),(d+1)]=beta
    Q[(d+1),(1:d)]=zeta; Q[(d+1), (d+1)]=-1
    DQ=diag(-1*diag(Q))
    P=(Q%*%DQ)+diag(1,nrow=d+1)
    x=-1*solve(B)%*%u
    y=rbind(x,sum(u))
    Pi=(1/sum(DQ%*%y))*DQ%*%y
    N=(sum(DQ%*%y)/sum(u))-1
    lambda=-1*diag(B)
    
    theta=sum(Pi*apply(P,MARGIN = 1, FUN=function(x){sum(x*log(x), na.rm=TRUE)}))
    
    thetaP=sum(Pi[1:d]*(1-log(lambda)))+theta
    return(list(stationaryDist=Pi,Jumps=N+1,entropyRate=theta, entropyRatePath=thetaP))
    
    ### A list with 4 objects: the stationary distribution of visited compartments (pi), the mean number of jumps per cycle, 
    ### the entropy rate per jump, and the entropy rate of the infinite path.
    ##seealso<< \code{\link{systemAge}}
  }
  ,
  ex=function(){
    # Century
    ksC=c(k.STR=0.094,k.MET=0.35,k.ACT=0.14,k.SLW=0.0038,k.PAS=0.00013)
    clayC=0.2; silt=0.45; LN=0.5; Ls=0.1; In=0.1
    Txtr=clayC+silt
    fTxtr=1-0.75*Txtr
    Es=0.85-0.68*Txtr
    
    alpha31=0.45; alpha32=0.55; alpha34=0.42; alpha35=0.45
    alpha41=0.3; alpha53=0.004; alpha43=1-Es-alpha53; alpha54=0.03
    
    AC=-1*diag(abs(ksC))
    AC[1,1]=AC[1,1]*exp(-3*Ls)
    AC[3,3]=AC[3,3]*fTxtr
    AC[3,1]=alpha31*abs(AC[1,1])
    AC[3,2]=alpha32*abs(AC[2,2])
    AC[3,4]=alpha34*abs(AC[4,4])
    AC[3,5]=alpha35*abs(AC[5,5])
    AC[4,1]=alpha41*abs(AC[1,1])
    AC[4,3]=alpha43*abs(AC[3,3])
    AC[5,3]=alpha53*abs(AC[3,3])
    AC[5,4]=alpha54*abs(AC[4,4])
    
    Fm=0.85-0.18*LN; Fs=1-Fm
    CI=matrix(nrow=5,ncol=1,c(In*Fm,In*Fs,0,0,0))
    
    entropyRate(B=AC, u=CI)
    
    xi=seq(0.1,2,by=0.1)
    
    lxi=lapply(xi,FUN=function(x){entropyRate(B=x*AC, u=CI)})
    
    xiEJ=sapply(lxi, FUN=function(x){x$entropyRate})
    xiEP=sapply(lxi, FUN=function(x){x$entropyRatePath})
    
    plot(xi, xiEJ, type="o")
    abline(v=1, lty=2)
    
    plot(xi, xiEP, type="o")
    abline(v=1, lty=2)
    
    #####
    Temp=seq(0,40,by=0.5)
    
    ET=lapply(Temp,FUN=function(x){entropyRate(B=fT.Century1(x)*AC, u=CI)})
    EJ_Temp=sapply(ET, FUN=function(x){x$entropyRate})
    EP_Temp=sapply(ET, FUN=function(x){x$entropyRatePath})
    
    plot(Temp, EJ_Temp, type="o")
    
    plot(Temp, EP_Temp, type="o")
    
    plot(Temp, fT.Century1(Temp), type="o")
    
    
      }
)
