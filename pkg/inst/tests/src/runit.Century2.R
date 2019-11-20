test.Example<-function(){
  t=seq(0,52*200,1) #200 years  
  LNcorn=0.17/0.004 # Values for corn clover reported in Parton et al. 1987
  Ex<-CenturyModel(t,LN=0.5,Ls=0.1,In=0.1)
  Ct=getC(Ex)
  Rt=getReleaseFlux(Ex)
  
  matplot(t,Ct,type="l", col=1:5,lty=1,ylim=c(0,max(Ct)*2.5),
  ylab=expression(paste("Carbon stores (kg C", ha^-1,")")),xlab="Time (weeks)") 
  lines(t,rowSums(Ct),lwd=2)
  legend("topright", c("Structural litter","Metabolic litter",
  "Active SOM","Slow SOM","Passive SOM","Total Carbon"),
  lty=1,lwd=c(rep(1,5),2),col=c(1:5,1),bty="n")
  
  matplot(t,Rt,type="l",lty=1,ylim=c(0,max(Rt)*3),ylab="Respiration (kg C ha-1 week-1)",xlab="Time") 
  lines(t,rowSums(Rt),lwd=2) 
  legend("topright", c("Structural litter","Metabolic litter",
  "Active SOM","Slow SOM","Passive SOM","Total Respiration"),
  lty=1,lwd=c(rep(1,5),2),col=c(1:5,1),bty="n")
}
