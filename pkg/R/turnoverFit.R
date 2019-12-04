#' Estimation of the turnover time from a soil radiocarbon sample.
#' 
#' This function finds the best possible value of turnover time from a soil
#' radiocarbon sample assuming a one pool model and annual litter inputs.
#' 
#' This algorithm takes the observed values and a given amount of litter
#' inputs, runs \code{\link{OnepModel14}}, calculates the squared difference
#' between predictions and observations, and uses \code{\link{optimize}} to
#' find the minimum difference. If the turnover time is relatively short (< 50
#' yrs), it is safe to assume C0=0 because the soil will reach steady state
#' within the simulation time. However, for longer turnover times it is
#' recommended to use a value of C0 close to the steady state value.
#' 
#' @param obsC14 a scalar with the observed radiocarbon value in Delta14C of
#' the soil sample.
#' @param obsyr a scalar with the year in which the soil sample was taken.
#' @param In a scalar or data.frame with the annual amount of litter inputs to
#' the soil.
#' @param C0 a scalar with the initial amount of carbon stored at the beginning
#' of the simulation.
#' @param yr0 The year at which simulations will start.
#' @param Zone the hemispheric zone of atmospheric radiocarbon. Possible values
#' are NHZone1: northern hemisphere zone 1, NHZone2: northern hemisphere zone
#' 2, NHZone3: northern hemisphere zone 3, SHZone12: southern hemisphere zones
#' 1 and 2, SHZone3: southern hemisphere zone 3. See \code{\link{Hua2013}} for
#' additional information.
#' @param plot logical. Should the function produce a plot?
#' @param by numeric. The increment of the sequence of years used in the
#' simulations.
#' @return A scalar with the value of the turnover time that minimizes the
#' difference between the prediction of a one pool model and the observed
#' radiocarbon value.
turnoverFit=structure(
     function 
     (obsC14, 
      obsyr, 
      In, 
      C0=0, 
      yr0=1900, 
      Zone="NHZone2", 
      plot=TRUE, 
      by=0.5 
      )
     {
          if(length(obsC14) != 1) stop("obsC14 must be a numeric value of length 1")
          if(length(obsyr) != 1) stop("obsyr must be a numeric value of length 1")
          if(length(C0) != 1) stop("C0 must be a numeric value of length 1")
          if(Zone=="NHZone1") inputFc=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$NHZone1,time.scale="AD")
          if(Zone=="NHZone2") inputFc=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$NHZone2,time.scale="AD")
          if(Zone=="NHZone3") inputFc=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$NHZone3,time.scale="AD")
          if(Zone=="SHZone12") inputFc=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$SHZone12,time.scale="AD")
          if(Zone=="SHZone3") inputFc=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$SHZone3,time.scale="AD")
          if(obsyr > tail(inputFc,1)$Year.AD) stop("The observed C14 datum must be from a year within the atmospheric radiocarbon period of the Hua et al (2012) dataset.")
          years=seq(yr0,tail(inputFc,1)$Year.AD,by=by)
          C14cost=function(k){
               tmp=OnepModel14(t=years,k=k,C0=C0,F0_Delta14C=inputFc[which(inputFc[,1]==yr0),2],
                               In=In,inputFc=inputFc)
               C14t=getF14(tmp)
               predC14=C14t[which(years==round(obsyr,1))]
               res=(obsC14-predC14)^2
               return(res)
          }
          kest1=optimize(C14cost,interval=c(1/20,1))$minimum
          kest2=optimize(C14cost,interval=c(1/5000,1/30))$minimum
          if(plot==TRUE){
               pred1=OnepModel14(t=years,k=kest1,C0=C0,F0_Delta14C=inputFc[which(inputFc[,1]==yr0),2],
                                In=In,inputFc=inputFc)
               pred2=OnepModel14(t=years,k=kest2,C0=C0,F0_Delta14C=inputFc[which(inputFc[,1]==yr0),2],
                                 In=In,inputFc=inputFc)
               C14test1=getF14(pred1)
               C14test2=getF14(pred2)
               par(mfrow=c(2,1),mar=c(4,5,1,1))
               plot(inputFc[,1:2],type="l",xlim=c(1900,2010), xlab="Year AD",ylab=expression(paste(Delta^14,"C ","(\u2030)")))
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
          return(list(tau1=1/kest1,tau2=1/kest2))
     }
     ,
     ex=function(){
     }
)
