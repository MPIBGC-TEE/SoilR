require('devtools')
pkgDir<-'../../'
devtools::uninstall(pkgDir)
devtools::install(pkgDir)
#devtools::install('../../../../debugHelpers/pkg')
