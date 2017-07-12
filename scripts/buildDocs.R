#!/usr/bin/Rscript
require(devtools)
install('~/linkeddocs/pkg')
require(linkeddocs)
pkgDir='~/SoilR-exp/pkg'
package.skeleton.dx_3(pkgDir)
install(pkgDir,args=c('--html'))
#check(pkgDir,document=FALSE,build_args = '--no-build-vignettes')

