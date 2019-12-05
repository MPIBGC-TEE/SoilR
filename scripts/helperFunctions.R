requireNamespace('knitr')
requireNamespace('pkgload')
requireNamespace('devtools')
requireNamespace('getopt')
#requireNamespace('git2r')
#pkgload::load_all("~/debugHelpers/pkg",export_all=FALSE)
#pkgload::load_all("~/roxygen2_mm",export_all=FALSE)
devtools::install_github('mamueller/roxygen2')
requireNamespace('roxygen2')

#########################################
update_and_check_rd_and_vignettes<-function(pkgDir){
  build_rd(pkgDir)
  check(pkgDir)
}
#########################################
build_and_check_rd<-function(pkgDir){
  build_rd(pkgDir)
  check_rd(pkgDir)
}
#########################################
show_docs<-function(pkgDir){
  #build_rd(pkgDir,roclets=c('remove_autotag_roclet'))
  #build-rd(pkgDir,roclets=c('auto_comment_roclet','rd'))
  #build-rd(pkgDir,roclets=c('update_auto_comment_roclet','rd'))
  #build-rd(pkgDir,roclets=c('inheritance_graph_roclet'))
  build_rd(pkgDir,roclet='rd')

  check_rd(pkgDir)
  install_pkg_with_html(pkgDir)
  update_github_dosc(pkgDir)
  browse_index(pkgDir)
}
#########################################
build <- function(pkgDir,path,args='--compact-vignettes=both',manual=TRUE){
  devtools::build(
    pkgDir,
    path=path,
    args=args,
  )
}
#########################################
build_rd<-function(pkgDir,roclets=c('inheritance_graph_roclet','rd')){
  # find relative path to this script from the current wd
  initial.options <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  
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
check<-function(pkgDir,document=FALSE,build_args='--compact-vignettes=both'){
  ##system2("R",args=c('CMD','build','--compact-vignettes=both'))
  #pkgTarName <- paste0(pkgload::pkg_name(pkgDir),'_',pkgload::pkg_version(pkgDir),'.tar.gz')
  ##system2("R",args=c('CMD','build','--as-cran',pkgTarName))
  #pkgbuild::build(pkgDir,args='--compact-vignettes=both') 
  #devtools::check_built(pkgTarName)
  # use a shortcut
   devtools::check(
     pkgDir,
     document=document,
     build_args=build_args,
     error_on='note'
   )
}  

#########################################
check_rhub<-function(pkgDir,build_args='--compact-vignettes=both'){
   devtools::check_rhub(
     pkgDir,
     build_args=build_args
   )
}  
#########################################
check_win_release<-function(pkgDir,args='--compact-vignettes=both'){
   devtools::check_win_release(
     pkgDir,
     args=args
   )
}  

#########################################
release<-function(pkgDir){
  # we use this shortcut
  # we always use check(pkgDir) before)
  res=devtools::release(
    pkgDir,
    check=FALSE
  )
}  
#########################################
install_pkg_with_html<-function(pkgDir){
  devtools::install(pkgDir,args=c('--html'))
}

#########################################
git.hubs.docs.dir<-function(pkgDir){
  file.path(pkgDir,'..','docs')
}

#########################################
git.hubs.docs.index<-function(pkgDir){
  git.hubs.docs.dir <-git.hubs.docs.dir(pkgDir) 
  file.path(git.hubs.docs.dir,'index.html')
}
#########################################
update_github_dosc<-function(pkgDir){
  git.hubs.docs.dir <-git.hubs.docs.dir(pkgDir) 
  #
  #p='pkg.pdf'
  #if(file.exists(p)){file.remove(p)}
  #devtools::check(pkgDir,document=FALSE)
  #system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(pkgDir)))
  #
  SoilR.lib.dir.html <- sprintf('%s/SoilR/html/',.libPaths()[1])
  stopifnot(
    all(
      as.logical(
        lapply(
        	list.files(git.hubs.docs.dir),
        	function(fp){file.remove(file.path(git.hubs.docs.dir,fp))}
        )
      )
    )
  )
  stopifnot(
    all(
      as.logical(
        lapply(
  	      list.files(full.names=TRUE,SoilR.lib.dir.html)
  	      ,
  	      function(fp){file.copy(from=fp,to=git.hubs.docs.dir,overwrite=TRUE)}
        )
      )
    )
  )
  
  #browser()
  # github alway starts with the file index.html
  stopifnot(
    file.copy(
  	  from=file.path(SoilR.lib.dir.html,'SoilR-package.html'),
  	  to=git.hubs.docs.index(pkgDir),
  	  overwrite=TRUE
    )
  )
}

#########################################
browse_index<-function(pkgDir){
  git.hubs.docs.dir <-git.hubs.docs.dir(pkgDir) 
  browserBin <- 'firefox'
  system(
    sprintf(
      '%s %s &',
       browserBin,
       git.hubs.docs.index(pkgDir)
    )
  )
}

#########################################
#' build a single vignette
#' This is usefull if you just want to look at it.
build_vignette<- function(vignette_src_path , buildDir='.',clean=TRUE){
  pkgDir <- dirname(dirname(vignette_src_path))
  on.exit(pkgload::unload(pkgDir))
  
  pkgload::load_all(pkgDir)
  t=tools::file_path_sans_ext(vignette_src_path)
  texFileName=file.path(buildDir,paste(t,'tex',sep='.'))
  pdfFileName=file.path(buildDir,paste(t,'pdf',sep='.'))
  if (file.exists(texFileName)){unlink(texFileName)}
  knitr::knit(
    vignette_src_path,
    output=texFileName
  )
  tools::texi2pdf(texFileName,clean=clean)
  tools::compactPDF( pdfFileName, gs_quality = "ebook")
  
}
