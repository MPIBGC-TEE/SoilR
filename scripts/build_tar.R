#!/usr/bin/Rscript
requireNamespace('devtools')
script.path<- getopt::get_Rscript_filename()
script.dir<- dirname(script.path)
source(file.path(script.dir,'helperFunctions.R'))
pkgDir <- file.path(script.dir,'..','pkg')

pkgBuildDir <-file.path(script.dir, 'package_builds')

if (!file.exists(pkgBuildDir)){dir.create(pkgBuildDir,recursive=TRUE)}
#
p <- build_tar(pkgDir=pkgDir,dest_path=pkgBuildDir)
print(p)
