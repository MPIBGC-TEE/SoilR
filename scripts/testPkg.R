#!/usr/bin/Rscript
requireNamespace('devtools')
script.path<- getopt::get_Rscript_filename()
script.dir<- dirname(script.path)
pkgDir <- file.path(script.dir,'..','pkg')
devtools::check(
  pkg=pkgDir,
  document=FALSE,
)
