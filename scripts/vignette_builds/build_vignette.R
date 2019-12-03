#!/usr/bin/Rscript

if(is.null(sys.calls()[[sys.nframe()-1]])){
  script.path<- getopt::get_Rscript_filename()
  script.dir<- dirname(script.path)
  source(file.path(script.dir,'..','helperFunctions.R'))

  build_vignette(
    vignette_src_path='../../pkg/vignettes/ParameterEstimation-knitr.Rnw',
    buildDir='.'
  )
}
