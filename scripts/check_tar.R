#!/usr/bin/Rscript
# This script uses kniter to create a markdown document
# reporting the status of several checks which are encoded in the 
# Rmd file
if(is.null(sys.calls()[[sys.nframe()-1]])){
  # if we are called via Rscript we figure out where we are
  requireNamespace('getopt')
  script.path<- getopt::get_Rscript_filename()
  script.dir<- dirname(script.path)
}else{
  # if the script is sourced from an R/Rstudio session we 
  # assume that we are in the same directory as the script 
  script.dir='.' 
  # To change this either set the scrip.dir or your working directory: 
  #
  # script.dir <- file.path(relative/path/to/this/script)
  #
  # setwd(relative/path/to/this/script)  
}
pkgDir <- file.path(script.dir,'..','pkg')
versionString <- pkgload::pkg_version(pkgDir) 
trunk <- paste0(pkgload::pkg_name(pkgDir),'_',versionString)
pkgTarName <- paste0(trunk,'.tar.gz')
reportName <- paste0('knitr_check_report_',trunk,'.md')
directReportName <- paste0('direct_check_report_',trunk,'.md')
pkgBuildDir <-file.path(script.dir, 'package_builds')

p <- file.path(pkgBuildDir,pkgTarName)
chk <- rhub::check_with_rdevel(p,show_status=TRUE)
browser()
mdstr <- capture.output(chk$cran_summary())
writeLines(text=mdstr,con=file.path(pkgBuildDir,directReportName))
#knitr::knit(
#  input=file.path(script.dir,'cran-comments.Rmd'),
#  output=file.path(pkgBuildDir,reportName)
#)

