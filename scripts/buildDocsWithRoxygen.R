#!/usr/bin/env Rscript
#·vim:set·ff=unix ts=2·sw=2:
require(devtools)
require(pkgload)
pkgload::load_all("~/roxygen2_mm",export_all=FALSE)

requireNamespace('roxygen2')
  tryCatch(
    roxygen2::SoilR_approved(),
    error=function(e){
      cat("
      Until the pull request to roxygen2 have been accepted 
      you need a special branch of roxygen2 to document SoilR. 
      Please install from gitby:     

      devtools::install_github('mamueller/roxygen2')
      ")
      e
  }
)

# find relative path to this script from the current wd
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
git.hubs.docs.dir <- file.path(script.basename,'..','docs')

pkgDir='~/SoilR-exp/pkg'
#roxygen2::roxygenize(pkgDir,roclets=c('remove_autotag_roclet'))
#roxygen2::roxygenize(pkgDir,roclets=c('auto_comment_roclet','rd'))
#roxygen2::roxygenize(pkgDir,roclets=c('update_auto_comment_roclet','rd'))
roxygen2::roxygenize(pkgDir,roclets=c('rd'))
#roxygen2::roxygenize(pkgDir,roclets=c('inheritance_graph_roclet'))
#devtools::check(
#  pkgDir,
#  document=FALSE,
#  build_args = '--no-build-vignettes',
#  args = '--ignore-vignettes'
#)
#devtools::install(pkgDir,args=c('--html'))
#
#p='pkg.pdf'
#if(file.exists(p)){file.remove(p)}
#devtools::check(pkgDir,document=FALSE)
#system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(pkgDir)))
#browserBin <- 'firefox'
#
## check if the browser is running 
#res <- system2(command='pgrep',args=browserBin,stdout=TRUE)
#
## if the command returns with an exit status !=0 res
## will have a status attribute
#BrowserNotRunning <- is.element("status",names(attributes(res)))
## start a new Browser tab only if it is not running
#SoilR.lib.dir.html <- sprintf('%s/SoilR/html/',.libPaths()[1])
#succrm <- lapply(
#	list.files(git.hubs.docs.dir),
#	function(fp){file.remove(fp)}
#)
#succall <- lapply(
#	list.files(full.names=TRUE,SoilR.lib.dir.html)
#	,
#	function(fp){file.copy(from=fp,to=git.hubs.docs.dir,overwrite=TRUE)}
#)
## github alway starts with the file index.html
#succ <- file.copy(
#	from=file.path(SoilR.lib.dir.html,'SoilR_package.html'),
#	to=file.path(git.hubs.docs.dir,'index.html'),
#	overwrite=TRUE
#)
##if(BrowserNotRunning){
#	system(sprintf('%s %s/index.html &',browserBin,git.hubs.docs.dir))
##}


