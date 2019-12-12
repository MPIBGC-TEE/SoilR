#!/usr/bin/env Rscript
############################################ 
# Build the *.rd and *.html documentation but without the vignettes
# which makes it much faster and the ideal tool to work on the roxygen comments 

# it also automatically updates the html-files for the github page 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  script.path<- getopt::get_Rscript_filename()
  script.dir<- dirname(script.path)
  source(file.path(script.dir,'helperFunctions.R'))
  pkgDir <- file.path(script.dir,'..','pkg')
  build_rd(pkgDir)
  check_rd(pkgDir)
  show_docs(pkgDir)
}
