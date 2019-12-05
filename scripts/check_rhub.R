#!/usr/bin/Rscript
requireNamespace('devtools')
script.path<- getopt::get_Rscript_filename()
script.dir<- dirname(script.path)
source(file.path(script.dir,'helperFunctions.R'))


# rhub expects the pdf version of the help files
# and will complain (a NOTE in the cran checks) if they are not there
# unfortunately the direct shortcut commands like p
# R CMD build produces them by default 
pkgDir <- file.path(script.dir,'..','pkg')
check_devtools_rhub(
  pkg=pkgDir
)
