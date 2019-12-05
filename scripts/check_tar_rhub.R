#!/usr/bin/Rscript
requireNamespace('getopt')
script.path<- getopt::get_Rscript_filename()
script.dir<- dirname(script.path)
source(file.path(script.dir,'helperFunctions.R'))


pkgDir <- file.path(script.dir,'..','pkg')
pkgBuildDir <-file.path(script.dir, 'package_builds')

pkgTarName <- paste0(pkgload::pkg_name(pkgDir),'_',pkgload::pkg_version(pkgDir),'.tar.gz')
#unlink(pkgBuildDir,recursive=TRUE)
if (!file.exists(pkgBuildDir){
  dir.create(pkgBuildDir,recursive=TRUE)
}

system2("R",args=c('CMD','build','--compact-vignettes=both'))
kgTarName <- paste0(pkgload::pkg_name(pkgDir),'_',pkgload::pkg_version(pkgDir),'.tar.gz')
system2("R",args=c('CMD','build','--as-cran',pkgTarName))
pkgbuild::build(pkgDir,args='--compact-vignettes=both') 
#build(pkgDir,path=pkgBuildDir)
#pkgTarName=list.files(pkgBuildDir)[[1]]

#check_tar_rhub(
#  file.path(
#    pkgBuildDir,
#    pkgTarName
#  )
#)
