#' Binding of pre- and post-bomb Delta14C curves
#' 
#' This function takes a pre- and a post-bomb curve, binds them together, and
#' reports the results back either in years BP or AD.
#' 
#' 
#' @param prebomb A pre-bomb radiocarbon dataset. They could be either
#' \code{\link{IntCal09}} or \code{\link{IntCal13}}.
#' @param postbomb A post-bomb radiocarbon dataset. They could be any of the
#' datasets in \code{\link{Hua2013}}.
#' @param time.scale A character indicating whether to report the results in
#' years before present \code{BP} or anno domini \code{AD}.
#' @return A \code{data.frame} with 3 columns: years in AD or BP, the
#' atmospheric Delta14C value, the standard deviation of the Delta14C value.
bind.C14curves<-structure(
     function 
     (prebomb, 
      postbomb, 
      time.scale 
      )
     {
     if(time.scale != "BP" & time.scale != "AD") stop("time.scale must be either BP or AD")
     if(time.scale == "BP"){
          pre=prebomb[,c(1,4,5)] 
          names(pre)<-c("Year.BP","Delta14C","Sigma")
          post=postbomb[,1:3]
          post[,1]=(post[,1]-1950)*(-1)
          names(post)<-c("Year.BP","Delta14C","Sigma")
          all=rbind(pre,post)
     }
     if(time.scale=="AD"){
          pre=prebomb[,c(1,4,5)] 
          pre[,1]=1950-pre[,1]
          names(pre)<-c("Year.AD","Delta14C","Sigma")
          post=postbomb[,1:3]
          names(post)<-c("Year.AD","Delta14C","Sigma")
          all=rbind(pre,post)
     }
     return(all)
}
,
     ex=function(){
          bp=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$NHZone1,time.scale="BP")
          plot(bp[,1:2],type="l")
          plot(bp[,1:2],type="l",xlim=c(-100,100))
          ad=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$NHZone1,time.scale="AD")
          plot(ad[,1:2],type="l")
          plot(ad[,1:2],type="l",xlim=c(0,2010))
          abline(v=1950,lty=2)
}
)
