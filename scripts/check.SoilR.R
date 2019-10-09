#!/usr/bin/Rscript
requireNamespace('devtools')
devtools::check(pkg="../pkg",build_args=c("--no-build-vignettes"))
