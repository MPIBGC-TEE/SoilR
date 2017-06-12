#!/usr/bin/Rscript

fromCran <- c( 'devtools', 'argparse', 'stringr')
lapply(fromCran,install.packages,repos='https://cran.uni-muenster.de')

fromGitHub <- c( 'R6Unit')
require(devtools)
lapply(fromGitHub,function(name){devtools::install_github(sprintf("mamueller/%s/pkg",name))})

# install the package itself from source  
devtools::install(file.path('..','pkg'))
