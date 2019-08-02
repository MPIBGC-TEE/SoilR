#!/usr/bin/Rscript
require('SoilR')
require('pkgload')
pkgDir=file.path('..','..','pkg')

#trunks=c('GeneralModel-knitr')
trunks=c('ParameterEstimation-knitr','GeneralModel-knitr')
#trunks=c('ParameterEstimation-knitr','GeneralModel-knitr')

#pkgload::load_all(pkgDir)
require('knitr')
for (t in trunks){
  texfileName=file.path(paste(t,'tex',sep='.'))
  knitr::knit(
    file.path(
      pkgDir
      ,'vignettes'
      ,paste(t,'Rnw',sep=".")
    )
    ,output=texfileName
  )
  expr=tools::texi2pdf(texfileName)
  expr=tools::texi2pdf(texfileName)
}
