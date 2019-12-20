# Test environments
## on travis-ci:\\
  Ubuntu 16.04.6 LTS () \\
  ## R CMD check results\\
  0 ERRORs | 0 WARNINGs | 0 NOTES.

## local installation 
   Ubuntu 18.04 LTS, R 3.6.1 
  
  Error: R CMD check found WARNINGs

## remote Rhub checks
### R-hub debian-gcc-devel (r-devel)
R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

### R-hub windows-x86_64-release (r-release)
R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## local Rhub containers
### rhub/debian-gcc-devel
0 errors ✔ | 0 warnings ✔ | 0 notes ✔


### rhub/fedora-clang-devel
❯ checking re-building of vignette outputs ... WARNING
  Error(s) in re-building vignettes:
  --- re-building ‘GeneralModel-knitr.Rnw’ using knitr
0 errors ✔ | 1 warning ✖ | 0 notes ✔

There are two remarks:
1. The error that causes the warning is due to a missing style file in the latex installation
   To fix it I would rather change the docker file for the container
   than the package.
2. If we use the remote rhub test 'as-cran' the fedora-clang-rdevel container fails with an error that seems due 
   to some dependency that uses the c compiler, possibly deSolve.
   The reason why we ran this fedora-clang-rdevel version locally, was to reproduce this error to fix it.
   But this is obviously not possible as the result reproducably shows 0 errors. 
   I actually even installed rdevel locally on my ubuntu but also failed to reproduce the error (which is consistent with
   the passing rdevel checks on other gcc based systems).
   I hope that you consider this as enough "good will". I am just not able to break the package anywhere where I have access ;-). 
