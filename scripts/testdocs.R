#!/usr/bin/Rscript
requireNamespace('pkgload')
requireNamespace('devtools')
pkgload::load_all("~/roxygen2_mm")
pkgDir='~/SoilR-exp/pkg'
#roxygen2::roxygenize(pkgDir,roclets=c('autotag_roclet','rd'))
roxygen2::roxygenize(pkgDir,roclets=c('rd'))
#devtools::check(
#  pkg=pkgDir,
#  document=FALSE,
#  build_args='--no-build-vignettes'
#)
