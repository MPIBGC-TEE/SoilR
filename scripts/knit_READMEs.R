#!/usr/bin/Rscript
# please run from this directory since the relative paths are hardcoded
# we update both READMEs simultaniously from the same *.Rmd file
# but with different relative paths to their figures (which are in pkg/man/figures
# This is necessary since otherwise github will not find the figures in one of the cases

# before we start we make sure that we use the newest version so that the Readmes are up to date
devtools::install('../pkg')
srcText<- readLines(file.path('pkg_readme_template.Rmd'))
here <- getwd()

setwd(file.path('..','pkg'))
knitr::opts_chunk$set(fig.path='man/figures/README-')
knitr::knit(text=srcText,output='README.md')
setwd(here)

setwd('..')
knitr::opts_chunk$set(fig.path='pkg/man/figures/README-')
knitr::knit(text=srcText,output='README.md')
setwd(here)
