#!/usr/bin/Rscript
requireNamespace('pkgload')
pkgload::load_all('../pkg',export_all=FALSE)
#pkgload::load_all('../pkg')

hours=seq(0,800,0.1)
bcmodel=AWBmodel(t=hours)
Cpools=getC(bcmodel)
###Time solution
## fixme mm:
## the next line causes trouble on Rforge Windows patched build
## matplot(hours,Cpools,type="l",ylab="Concentrations",xlab="Hours",lty=1,ylim=c(0,max(Cpools)*1.2))
###State-space diagram
#plot(as.data.frame(Cpools))
