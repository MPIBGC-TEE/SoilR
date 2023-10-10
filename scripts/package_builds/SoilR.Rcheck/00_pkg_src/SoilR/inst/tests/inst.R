#!/usr/bin/Rscript
require('devtools')
pkgDir<-'../../'
if (is.element('SoilR',installed.packages())){
  devtools::uninstall(pkgDir)
}
devtools::install(pkgDir)
#devtools::install('../../../../debugHelpers/pkg')
