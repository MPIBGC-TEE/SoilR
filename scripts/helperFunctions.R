requireNamespace('knitr')
requireNamespace('pkgload')
requireNamespace('devtools')
requireNamespace('rhub')
requireNamespace('getopt')
#requireNamespace('git2r')
#pkgload::load_all("~/debugHelpers/pkg",export_all=FALSE)
pkgload::load_all("~/roxygen2_mm",export_all=FALSE)
#devtools::install_github('mamueller/roxygen2')
requireNamespace('roxygen2')


#########################################
update_and_check_rd_and_vignettes<-function(pkgDir){
  build_rd(pkgDir)
  check(pkgDir)
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
build_devtools <- function(pkgDir,path,args='--compact-vignettes=both',manual=TRUE){
  devtools::build(
    pkgDir,
    path=path,
    args=args,
    manual=manual # very important since rhub will complain otherwise
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
build_tar <- function(pkgDir,dest_path){
  pkgTarName <- paste0(pkgload::pkg_name(pkgDir),'_',pkgload::pkg_version(pkgDir),'.tar.gz')
  #pkgbuild::build(path=pkgDir,dest_path=dest_path,args='--compact-vignettes=both') #does not build pdf of Rd files
  devtools::build(
    pkg=pkgDir,
    path=dest_path,
    args='--compact-vignettes=both',
  )
}
#########################################
check_devtools<-function(pkgDir,document=FALSE,build_args='--compact-vignettes=both'){
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
# this is a slightly changed copy of devtools::check_rhub
# that differs in two points 
# 1.) it sets a default flag for vignette compaction, which (when unset) leads to a "NOTE" in the cranchecks
#     recommending to compact the vignettes ...
# 2.) It makes sure that the pdfs
#     for the help files are built before the package
#     is uploaded to r_hub which complains in some cases
#     with "Package has help file(s) containing install/render-stage \Sexpr{} expressions but no prebuilt PDF manual."
# 3.) it validates the email address mamueller@bgc-jena.mpg.de 
#     On the building machine (e.g.travis)
#     I had to do this only once but we need to add the file everytime 
check_devtools_rhub  <- function (
  pkg = ".", 
  platforms = NULL, 
  email = NULL, 
  interactive = TRUE, 
  build_args ='--compact-vignettes=both' , # 1.)
  ...
) 
{
  devtools:::check_suggested("rhub")
    pkg <- devtools::as.package(pkg)
    built_path <- devtools::build(
      pkg$path, 
      tempdir(), 
      quiet = !interactive, 
      args = build_args,
      manual=TRUE  # 2.)
    )
    on.exit(unlink(built_path), add = TRUE)
    #check_dots_used()
    rhub::validate_email(email='mamueller@bgc-jena.mpg.de',token='8b48b62f582c4a7b9684a507d64704ac')
    status <- rhub::check_for_cran(path = built_path, email = email, 
        platforms = platforms, show_status = interactive, ...)
    if (!interactive) {
        message("R-hub check for package ", sQuote(pkg$package), 
            " submitted.")
        status
    }
    else {
        status
    }
}
#########################################
check_tar_rhub<-function(path){
   rhub::check_for_cran(
     path=path
   )
}  
#########################################
check_win_releases<-function(pkgDir,args='--compact-vignettes=both'){
   devtools::check_win_release(
     pkgDir,
     args=args
   )
   devtools::check_win_devel(
     pkgDir,
     args=args
   )
   devtools::check_win_oldrelease(
     pkgDir,
     args=args
   )
}  

#########################################
release<-function(pkgDir){
  # we use this shortcut
  # we always use check(pkgDir) before)
  res=devtools::release(
    pkg=pkgDir,
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
#' build a SINGLE vignette (as opposed to devtools::build_vignettes(
#' This is usefull if you just want to look at it.
#' while you are working with the vignette and the code simultaniuously
#' It will load the package from source and reflect every change in the code 
#' immidiately.
#' This avoids confusion that sometimes arises from using the installed version of the package
#' rather than the most recent version while working on the vignette

build_vignette<- function(vignette_src_path , buildDir='.',clean=TRUE){
  pkgDir <- dirname(dirname(vignette_src_path)) 
  old <- getwd()
  on.exit(
    {
      setwd(old);
      pkgload::unload(pkgload::pkg_name(pkgDir))
    }
  ) 
  
  pkgload::load_all(pkgDir,export_all=FALSE)# load SoilR src
  t <- basename(tools::file_path_sans_ext(vignette_src_path))
  texFileName  <- paste(t,'tex',sep='.')
  texFilePath=file.path(buildDir,texFileName)
  if (file.exists(texFilePath)){unlink(texFilePath)}
  knitr::knit(
    vignette_src_path,
    output=texFilePath
  )
  setwd(buildDir)
  tools::texi2pdf(texFileName,clean=clean)
  pdfFilePath=file.path(paste(t,'pdf',sep='.'))
  browser()
  tools::compactPDF(pdfFilePath, gs_quality = "ebook")
}
