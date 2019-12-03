#!/usr/bin/env Rscript
#·vim:set·ff=unix ts=2·sw=2:
############################################ 
# find out if we are started via commandline 
# or from a R-session
if(is.null(sys.calls()[[sys.nframe()-1]])){
  script.path<- getopt::get_Rscript_filename()
  script.dir<- dirname(script.path)
  source(file.path(script.dir,'helperFunctions.R'))
  pkgDir <- file.path(script.dir,'..','pkg')
  build_and_check_rd(pkgDir)
  show_docs(pkgDir)
}
