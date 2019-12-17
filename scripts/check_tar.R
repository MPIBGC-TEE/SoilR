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

requireNamespace('parallel')
requireNamespace('rhub')
pkgDir <- file.path(script.dir,'..','pkg')
versionString <- pkgload::pkg_version(pkgDir) 
trunk <- paste0(pkgload::pkg_name(pkgDir),'_',versionString)
pkgTarName <- paste0(trunk,'.tar.gz')
reportName <- paste0('knitr_check_report_',trunk,'.md')
directReportName <- paste0('direct_check_report_',trunk,'.md')
pkgBuildDir <-file.path(script.dir, 'package_builds')

p <- file.path(pkgBuildDir,pkgTarName)


# helper function to perform the actual checks and convert
# the output to a string 
# (the toStrFun function has to be an argument since the different checking functions
# return different result types that can not be converted to string in the
# same way
call2str <- function(call,toStrFunc){
  res <- tryCatch(
    error=function(e){e} # propagate errors upwards
    ,eval(call)
  )
  if(inherits(res,'error')){
    #errors have a sensible toString implementation
    # so we just call it
    str_res=toString(res) 
  }else{
    # since the different checkfunction deliver different 
    # results we apply a checkfunction specific unwrapper...
    str_res=toStrFunc(res)
  }
  str_res
}
# define functions to unwrap the testresults to a string
rhub_remote_2str <- function(chk){ capture.output(chk$cran_summary())}

rhub_local_2str <- function(chk){
    c(chk$image,capture.output(chk$check_result))
  }

# now compute the acutal resultstrings
chk_local <- call2str(
  as.call(list(devtools::check_built,path=p))
  ,function(chk){capture.output(chk)}
)

chk_rhub_remote_rdevel <- call2str(
  as.call(list(rhub::check_with_rdevel, path=p , show_status=TRUE))
  ,rhub_remote_2str
)  
chk_rhub_remote_windows <- call2str(
  as.call(list(rhub::check_on_windows, path=p ,show_status=TRUE))
  ,rhub_remote_2str
)

chk_rhub_local_debian_gcc_devel   <- call2str(
  as.call(list(rhub::local_check_linux, path=p ,image = "rhub/debian-gcc-devel"))
  ,rhub_local_2str
)
chk_rhub_local_fedora_clang_devel <- call2str(
  as.call(list(rhub::local_check_linux, path=p ,image ="rhub/fedora-clang-devel"))
  ,rhub_local_2str
)
browser()
text=c(
  "# local installation 
  Ubuntu 18.04 LTS, R 3.6.1 
  "
  ,chk_local
  ,"# remote Rhub checks"
  ,
  c( chk_rhub_remote_rdevel ,chk_rhub_remote_windows)
  ,'# local Rhub containers'
  ,
  c(chk_rhub_local_debian_gcc_devel, chk_rhub_local_fedora_clang_devel)
)

writeLines(
  text=text
  ,con=file.path(pkgBuildDir,directReportName)
)

