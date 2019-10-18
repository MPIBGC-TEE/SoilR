#!/usr/bin/Rscript
requireNamespace('pkgload')
pkgload::load_all("~/roxygen2_mm")
pkgDir='~/SoilR-exp/pkg'
roxygen2::roxygenize(pkgDir,roclets=c('autotag_roclet','rd'))
