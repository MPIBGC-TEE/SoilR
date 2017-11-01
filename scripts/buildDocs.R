#!/usr/bin/env Rscript
#·vim:set·ff=unix ts=2·sw=2:

# find relative path to this script from the current wd
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
git.hubs.docs.dir <- file.path(script.basename,'..','docs')

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
SoilR.lib.dir.html <- sprintf('%s/SoilR/html/',.libPaths()[1])
succrm <- lapply(
	list.files(git.hubs.docs.dir),
	function(fp){file.remove(fp)}
)
succall <- lapply(
	list.files(full.names=TRUE,SoilR.lib.dir.html)
	,
	function(fp){file.copy(from=fp,to=git.hubs.docs.dir,overwrite=TRUE)}
)
# github alway starts with the file index.html
succ <- file.copy(
	from=file.path(SoilR.lib.dir.html,'SoilR_package.html'),
	to=file.path(git.hubs.docs.dir,'index.html'),
	overwrite=TRUE
)
if(BrowserNotRunning){
	system(sprintf('%s %s/index.html &',browserBin,git.hubs.docs.dir))
	#system(sprintf('firefox %s/SoilR/html/00Index.html &',.libPaths()[1]))
}


