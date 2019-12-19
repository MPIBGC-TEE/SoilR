#!/usr/bin/Rscript

if(is.null(sys.calls()[[sys.nframe()-1]])){
  # if we are called wiht Rscript find out where we are
  script.path<- getopt::get_Rscript_filename()
  script.dir<- dirname(script.path)
} else{
  # If we are in an R/Rstudio session 
  # we have to tell the script where to find the helper functions 
  # and for simplicity assume that we are in the directory where the script live
  script.dir <- '.'
}
source(file.path(script.dir,'..','helperFunctions.R'))
build_vignette(
  vignette_src_path='../../pkg/vignettes/ParameterEstimation-knitr.Rnw',
  #vignette_src_path='../../pkg/vignettes/GeneralModel-knitr.Rnw',
  buildDir='.'
)
