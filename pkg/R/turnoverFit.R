#' Estimation of the turnover time from a radiocarbon sample.
#' 
#' This function finds two possible values of turnover time from 
#' radiocarbon sample assuming a one pool model with carbon at equilibrium.
#' 
#' This algorithm takes an observed radiocarbon value and 
#' runs \code{\link{OnepModel14}}, calculates the squared difference
#' between predictions and observations, and uses \code{\link{optimize}} to
#' find the minimum difference. 
#' 
#' @param obsC14 a scalar with the observed radiocarbon value in Delta14C
#' @param obsyr a scalar with the year in which the sample was taken.
#' @param yr0 The year at which simulations will start.
#' @param Fatm an atmospheric radiocarbon curve as data.frame. First column 
#' must be time.
#' @param plot logical. Should the function produce a plot?
#' @param by numeric. The increment of the sequence of years used in the
#' simulations.
#' @return A numeric vector with two values of the turnover time that minimize the
#' difference between the prediction of a one pool model and the observed
#' radiocarbon value.
turnoverFit=structure(
     function 
     (obsC14, 
      obsyr, 
      yr0, 
      Fatm, 
      plot=TRUE, 
      by=0.5 
      )
     {
          if(length(obsC14) != 1) stop("obsC14 must be a numeric value of length 1")
          if(length(obsyr) != 1) stop("obsyr must be a numeric value of length 1")
          if(obsyr > tail(Fatm[,1],1)) stop("The observed C14 datum must be from a year within the atmospheric radiocarbon curve.")
          years=seq(yr0,tail(Fatm[,1],1),by=by)
          C14cost=function(k){
               tmp=OnepModel14(t=years,k=k,C0=1,F0_Delta14C=k/(k+0.0001209681),
                               In=k,inputFc=Fatm)
               C14t=getF14(tmp)
               predC14=C14t[which(years==round(obsyr,1))]
               res=(obsC14-predC14)^2
               return(res)
          }
          kest1=optimize(C14cost,interval=c(1/30,1))$minimum
          kest2=optimize(C14cost,interval=c(1/50000,1/30))$minimum
          if(plot==TRUE){
               pred1=OnepModel14(t=years,k=kest1,C0=1,F0_Delta14C=kest1/(kest1+0.0001209681),
                                In=kest1,inputFc=Fatm)
               pred2=OnepModel14(t=years,k=kest2,C0=1,F0_Delta14C=kest2/(kest2+0.0001209681),
                                 In=kest2,inputFc=Fatm)
               C14test1=getF14(pred1)
               C14test2=getF14(pred2)
               par(mfrow=c(2,1),mar=c(4,5,1,1))
               plot(Fatm,type="l", xlab="Year AD",ylab=expression(paste(Delta^14,"C ","(per mille)")), ylim=c(min(0,obsC14),900))
               points(obsyr,obsC14,pch=19)
               lines(years,C14test1,col=2)
               lines(years,C14test2,col=4)
               legend("topleft",c("Atmospheric 14C","Model prediction 1","Model prediction 2","observation"),
                  lty=c(1,1,1,NA),pch=c(NA,NA,NA,19),col=c(1,2,4,1),bty="n")
               vop=Vectorize(C14cost)
               curve(vop,0,1,xlab="k",ylab="Squared residuals")
               abline(v=kest1,lty=2)
               abline(v=kest2,lty=2)
               par(mfrow=c(1,1))
          }
          return(c(tau1=1/kest1,tau2=1/kest2))
     }
     ,
     ex=function(){
     }
)
