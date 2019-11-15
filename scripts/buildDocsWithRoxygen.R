#!/usr/bin/env Rscript
#·vim:set·ff=unix ts=2·sw=2:
requireNamespace('pkgload')
requireNamespace('devtools')
requireNamespace('getopt')
pkgload::load_all("~/debugHelpers/pkg",export_all=FALSE)
pkgload::load_all("~/roxygen2_mm",export_all=FALSE)

#########################################
build_docs<-function(pkgDir){
  #build_rd(pkgDir,roclets=c('remove_autotag_roclet'))
  #build-rd(pkgDir,roclets=c('auto_comment_roclet','rd'))
  #build-rd(pkgDir,roclets=c('update_auto_comment_roclet','rd'))
  #build-rd(pkgDir,roclets=c('inheritance_graph_roclet'))
  build_rd(pkgDir)

  check_rd(pkgDir)
  install_pkg_with_html(pkgDir)
  update_github_dosc(pkgDir)
  browse_index(pkgDir)
}
#########################################
build_rd<-function(pkgDir,roclets=c('inheritance_graph_roclet','rd')){
  # find relative path to this script from the current wd
  initial.options <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  
  pkgDir='~/SoilR-exp/pkg'
  pp('pkgDir')
  
  
  requireNamespace('roxygen2')
    tryCatch(
      roxygen2::SoilR_approved(),
      error=function(e){
        cat("
        Until the pull request to roxygen2 have been accepted 
        you need a special branch of roxygen2 to document SoilR. 
        Please install from github by:     
  
        devtools::install_github('mamueller/roxygen2')
        ")
        e
    }
  )
  roxygen2::roxygenize(pkgDir,roclets)
}

#########################################
check_rd<-function(pkgDir){
devtools::check(
  pkgDir,
  document=FALSE,
  build_args = '--no-build-vignettes',
  args = '--ignore-vignettes'
)
}  

#########################################
install_pkg_with_html<-function(pkgDir){
  devtools::install(pkgDir,args=c('--html'))
}

#########################################
update_github_dosc<-function(pkgDir){
  git.hubs.docs.dir <- file.path(pkgDir,'..','docs')
  #
  #p='pkg.pdf'
  #if(file.exists(p)){file.remove(p)}
  #devtools::check(pkgDir,document=FALSE)
  #system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(pkgDir)))
  browserBin <- 'firefox'
  #
  SoilR.lib.dir.html <- sprintf('%s/SoilR/html/',.libPaths()[1])
  browser()
  succrm <- lapply(
  	list.files(git.hubs.docs.dir),
  	function(fp){file.remove(file.path(git.hubs.docs.dir,fp))}
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
}

#########################################
browse_index<-function(pkgDir){
  system(sprintf('%s %s/index.html &',browserBin,git.hubs.docs.dir))
}




############################################ 
# find out if we are started via commandline 
# or from a R-session
if(is.null(sys.calls()[[sys.nframe()-1]])){
  script.path<- getopt::get_Rscript_filename()
  script.dir<- dirname(script.path)
  pkgDir <- file.path(script.dir,'..','pkg')
  build_docs(pkgDir)
}
