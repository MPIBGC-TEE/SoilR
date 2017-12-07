#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
checkWarning <- function(expr,silent=TRUE) {
    checkTrue(
    inherits(
       tryCatch(
         eval(expr, envir = parent.frame()), 
         silent = silent,
         warning=function(w){w}
       ),
       "warning"
    ),
        "Warning not generated as expected\n")
}             


plotAndCheck=function(fileName,lexp,env){
  pdf(file=fileName)
  eval(lexp,env)
  dev.off()
  # fixme mm:
  # the following command does not work on all platforms (it relies on a 
  # commandline tool qpdf
  res=system(command=paste("qpdf --check ",fileName,sep=""))
  checkEquals(attr(res,"status"),NULL)
}
