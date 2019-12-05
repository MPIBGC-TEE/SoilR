#!/usr/bin/Rscript
requireNamespace('devtools')
script.path<- getopt::get_Rscript_filename()
script.dir<- dirname(script.path)
source(file.path(script.dir,'helperFunctions.R'))
pkgDir <- file.path(script.dir,'..','pkg')

#pkgBuildDir <-file.path(script.dir, 'package_builds')

#unlink(pkgBuildDir,recursive=TRUE)
#dir.create(pkgBuildDir,recursive=TRUE)
#
#build(pkgDir,path=pkgBuildDir)

#pkgTarName=list.files(pkgBuildDir)[[1]]
#devtools::check_built(file.path(pkgBuildDir,pkgTarName))

check(
  pkg=pkgDir,
)
