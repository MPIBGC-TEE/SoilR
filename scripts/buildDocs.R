#!/usr/bin/Rscript
require(devtools)
#install('~/linkeddocs/pkg')
require(linkeddocs)
pkgDir='~/SoilR-exp/pkg'
package.skeleton.dx_3(pkgDir)
install(pkgDir,args=c('--html'))
#check(pkgDir,document=FALSE,build_args = '--no-build-vignettes')
browserBin <- 'firefox'
# check if the browser is running 
res <- system2(command='pgrep',args=browserBin,stdout=TRUE)
# if the command returns with an exit status !=0 res
# will have a status attribute
BrowserNotRunning <- is.element("status",names(attributes(res)))
# start a new Browser tab only if it is not running
if(BrowserNotRunning){
	system(sprintf('%s %s/SoilR/html/SoilR_package.html &',browserBin,.libPaths()[1]))
}
#system(sprintf('firefox %s/SoilR/html/00Index.html &',.libPaths()[1]))
list.files(full.names=TRUE,sprintf('%s/SoilR/html/',.libPaths()[1]))


