#!/usr/bin/Rscript
# please run from this directory since the relative paths are hardcoded
# we update both READMEs simultaniously from the same *.Rmd file
# but with different relative paths to their figures (which are in pkg/man/figures
# This is necessary since otherwise github will not find the figures in one of the cases
srcText<- readLines(file.path('pkg_readme_template.Rmd'))
knitr::opts_chunk$set(fig.path='man/figures/README-')
knitr::knit(text=srcText,output=file.path('..','pkg','README.md'))

knitr::opts_chunk$set(fig.path='pkg/man/figures/README-')
knitr::knit(text=srcText,output=file.path('..','README.md'))
