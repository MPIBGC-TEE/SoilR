#!/usr/bin/Rscript
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
require(devtools)
install('../pkg')
