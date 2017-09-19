#!/usr/bin/Rscript
require(devtools)
#install('~/linkeddocs/pkg')
require(linkeddocs)
pkgDir='~/SoilR-exp/pkg'
package.skeleton.dx_3(pkgDir)
install(pkgDir,args=c('--html'))
#system('xdg-open ~/R/x86_64-pc-linux-gnu-library/3.3/SoilR//html/00Index.html')
#check(pkgDir,document=FALSE,build_args = '--no-build-vignettes')
#system(sprintf('firefox %s/SoilR/html/SoilR_package.html &',.libPaths()[1]))


