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
  'rhub',
  'FME',
  'igraph',
  'expm',
  'sets'
)
lapply(fromCran,install.packages,repos='https://cran.uni-muenster.de')
