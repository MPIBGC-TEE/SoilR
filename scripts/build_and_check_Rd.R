#!/usr/bin/env Rscript
# This is the tool to work on the roxygen comments that create the rd and html files.
#
# It builds the *.rd and *.html documentation but without the vignettes
# which makes it much faster. 
# it also automatically updates the html-files for the github page 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  # if we are called via Rscript we figure out where we are
  requireNamespace('getopt')
  script.path<- getopt::get_Rscript_filename()
  script.dir<- dirname(script.path)
}else{
  # if the script is sourced from an R/Rstudio session we 
  # assume that we are in the same directory as the script. 
  script.dir='.' 
  # To change this either set the scrip.dir or your working directory: 
  #
  # script.dir <- file.path(relative/path/to/this/script)
  #
  # setwd(relative/path/to/this/script)  
}

source(file.path(script.dir,'helperFunctions.R'))
pkgDir <- file.path(script.dir,'..','pkg')

#build_rd(pkgDir,roclets=c('remove_autotag_roclet'))
#build-rd(pkgDir,roclets=c('auto_comment_roclet','rd'))
#build-rd(pkgDir,roclets=c('update_auto_comment_roclet','rd'))
#build-rd(pkgDir,roclets=c('inheritance_graph_roclet'))
#build_rd(pkgDir,roclets=c('inheritance_graph_roclet','rd'))
#build_rd(pkgDir,roclets=c('auto_comment_roclet','rd'))
#build_rd(pkgDir,roclets=c('update_auto_comment_roclet','rd'))
build_rd(pkgDir)
check_rd(pkgDir)
show_docs(pkgDir)
