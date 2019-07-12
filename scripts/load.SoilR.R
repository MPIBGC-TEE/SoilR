#!/usr/bin/Rscript
deps<-c('pkgload')
for (d in deps){
	if (!is.element(d,installed.packages())) {
		install.packages(d,repos='https://cran.uni-muenster.de')
	}
}
for (d in deps){
	require(d,character.only=TRUE)
}
load_all('../pkg')
