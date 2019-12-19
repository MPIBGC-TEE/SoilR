# Scripts for package maintenance 

The scripts in the directory [scripts](https://github.com/MPIBGC-TEE/SoilR-exp/tree/master/scripts) 
have descriptive names and are meant as abreviations and memory aids for common tasks.
They are usaullay built to be executed by `Rscript` but you can of course use them from a R/Rstudio session.
In the latter case it is easier to change into the script directory.
Some commonly used functions are in.
```
./helperFunctions.R 
```
The scripts and functions are not intendet as blackboxes but mainly as reminders about some necessarry flags and `SoilR` specific locations for calls to more general tools from `devtools, knitr, tools` or `rhub`.
They have some comments and are meant to be read as well as executed.
All they can do, can equally well be achieved from a R or Rstudio session.
By using the scripts as an executable documentation we continiuously check these instructions and keep them up to date.

## Working on the help files
We use roxygen to generate the Rd files from the comments.
To update the rd files and the html files for the github documentation:
```
./build_and_check_Rd.R
```

## Working on the vignettes
To work on the vignettes there is a small scirpt you can invoke with the relative 
path to the vignette you want to build.
```
./build_vignette.R relative/path/to/the/vignette.Rnw
```



## Check the package as cran:
``` 
./testPkg.R 
``` 
 Will run local checks wiht devtools::check, 
 note that it will not build the updated docs, so run `build_and_check_Rd.R` 
 first, if you want the latest version of the docs.

## Build and check a downloadable package ```SoilR.*tar.gz```
 
* ```
  ./build_tar.R 
  ```
  Will create the downloadable version in [package_builds](https://github.com/MPIBGC-TEE/SoilR-exp/tree/master/scripts/package_builds) 

* ``` 
  ./check_tar.R  
  ```
  Will perform local and remote checks with several rhub docker containers and windows installations and create a markdown file reporting the results in the same directory.  
  
* If the tests pass you still have to add the built package and its report to the git repo, if you want to make it available for download.
