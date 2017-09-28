#!/usr/bin/Rscript
require(devtools)
#install('~/linkeddocs/pkg')
require(linkeddocs)
pkgDir='~/SoilR-exp/pkg'
package.skeleton.dx_3(pkgDir)
install(pkgDir,args=c('--html'))
#check(pkgDir,document=FALSE,build_args = '--no-build-vignettes')
system(sprintf('firefox %s/SoilR/html/SoilR_package.html &',.libPaths()[1]))
#system(sprintf('firefox %s/SoilR/html/00Index.html &',.libPaths()[1]))


