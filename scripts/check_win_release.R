#!/usr/bin/Rscript
requireNamespace('devtools')
script.path<- getopt::get_Rscript_filename()
script.dir<- dirname(script.path)
source(file.path(script.dir,'helperFunctions.R'))
pkgDir <- file.path(script.dir,'..','pkg')
check_win_release(
  pkg=pkgDir
)
