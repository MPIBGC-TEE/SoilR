#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
soilrRepoBase <- Sys.getenv("soilrRepoBase") 
ss=file.path(soilrRepoBase,"scripts")
sr=file.path(soilrRepoBase,"RPackages","SoilR","pkg")
#sR=file.path(sr,"R")
#sC=file.path(sr,"..","C++")
#sm=file.path(sr,"man")
#si=file.path(sr,"inst")
#sd=file.path(si,"doc")
#sT=file.path(si,"tests")
ignoreListPath<-  file.path(ss,"ignoredWords") 
persDictPath <-  file.path(ss,"acceptedWords.rds") 
saveRDS(readLines(ignoreListPath),persDictPath)
inject <- function(targetFunc){
  res <- targetFunc(
    dir=sr,
    control=list(
      "--master=en_US", 
      "--add-extra-dicts=en_GB"
    )
    ,
    dictionaries=persDictPath
  )
  res
}
funcList=c(aspell_package_R_files,aspell_package_Rd_files,aspell_package_vignettes)
for (fun in funcList){
  res=inject(fun)
  print(res)
}
