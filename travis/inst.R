#!/usr/bin/Rscript

fromCran <- c(
  'devtools',
	'testthat',
	'argparse',
	'stringr',
	'RUnit',
	'getopt',
  'knitr',
  'rmarkdown',
  'FME'
)
lapply(fromCran,install.packages,repos='https://cran.uni-muenster.de')

#fromGitHub <- c( 'R6Unit','debugHelpers')
#require(devtools)
#lapply(fromGitHub,function(name){devtools::install_github(sprintf("mamueller/%s/pkg",name))})

# install the package itself from source  
devtools::install(file.path('pkg'))
