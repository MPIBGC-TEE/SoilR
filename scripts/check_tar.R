#!/usr/bin/Rscript
requireNamespace('devtools')
script.path<- getopt::get_Rscript_filename()
script.dir<- dirname(script.path)
source(file.path(script.dir,'helperFunctions.R'))
pkgDir <- file.path(script.dir,'..','pkg')

#pkgload::pkg_version(pkgDir) does not get the last '-' right and puts a point there
versionsString=pkgload::pkg_desc(pkgDir)$get_field('Version')
pkgTarName <- paste0(pkgload::pkg_name(pkgDir),'_',versionsString,'.tar.gz')
pkgBuildDir <-file.path(script.dir, 'package_builds')

if (!file.exists(pkgBuildDir)){dir.create(pkgBuildDir,recursive=TRUE)}
p <- file.path(script.dir,pkgBuildDir,pkgTarName)
#p <- build_tar(pkgDir=pkgDir,dest_path=pkgBuildDir)

# Note that for the rhub::local_check_linux commands You need a working docker installation 
# and be part of the docker group (to run containers without sudo) 
r_cmd_check_args=c('--as-cran')
chk <- devtools::check_built(p)
chk <- rhub::check_on_windows(path=p)
chk <- rhub::local_check_linux(path=p, image = "rhub/fedora-gcc-devel")
chk <- rhub::local_check_linux(path=p, image = "rhub/ubuntu-gcc-devel")
chk <- rhub::local_check_linux(path=p, image = "rhub/fedora-clang-devel")
