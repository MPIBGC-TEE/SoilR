---
output: github_document
---
<!-- README.md is generated from README.Rmd. by scripts/knit_READMEs.R which builds the cran and the github readme which share the same figures but must use different relative paths to them -->


[![Travis-CI Build Status](https://travis-ci.org/MPIBGC-TEE/SoilR-exp.svg?branch=master)](https://travis-ci.org/MPIBGC-TEE/SoilR-exp)
[![CRAN status](https://www.r-pkg.org/badges/version/SoilR)](https://CRAN.R-project.org/package=SoilR)
# SoilR: Models of Soil Organic Matter Decomposition
This is the development version of SoilR. It is a rapidly evolving version that may be less
stable than the official stable version in [CRAN](https://cran.r-project.org/web/packages/SoilR/index.html).

## Documentation
* The latest build of the package documentation can be found [here:](https://mpibgc-tee.github.io/SoilR-exp/).
* To find examples you can also look at the [workflows](http://www.bgc-jena.mpg.de/TEE/software/soilr/).

<!--
## Example
This is a working example which demostrates some of the new functionality.


```r
require('SoilR',quietly =TRUE)
smod <- WangThreePoolNonAutonomous_sym() 
# (look at the source code of WangThreePoolNonAutonomous_sym )
plotPoolGraph(smod)
```

<img src="pkg/man/figures/README-unnamed-chunk-2-1.svg" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="100%" />

```r
state_variable_names(smod)
#> [1] "C_l" "C_b" "C_s"
# define initial values for the state variables
iv=c(C_l=1000,C_b=5000,C_s=1000)
times<-seq(from=1,to=1000,by=10)
modrun=Model_by_PoolNames( smod=smod ,times=times ,initialValues=iv)
sol <- getSolution(modrun)
# Let's see what we have computed
colnames(sol)
#>  [1] "time"                               "C_l"                               
#>  [3] "C_b"                                "C_s"                               
#>  [5] "accumulated_influx.C_l"             "accumulated_influx.C_s"            
#>  [7] "accumulated_internal_flux.C_l->C_b" "accumulated_internal_flux.C_s->C_b"
#>  [9] "accumulated_internal_flux.C_b->C_s" "accumulated_outflux.C_l"           
#> [11] "accumulated_outflux.C_s"            "influxes.C_l"                      
#> [13] "influxes.C_s"                       "internal_fluxes.C_l->C_b"          
#> [15] "internal_fluxes.C_s->C_b"           "internal_fluxes.C_b->C_s"          
#> [17] "out_fluxes.C_l"                     "out_fluxes.C_s"
# shortcut overview plot for all phase plane projections and time lines
# of the pool contents
 plot(data.frame(times=times,sol[,c('C_l','C_s','C_b')]))
```

<img src="pkg/man/figures/README-unnamed-chunk-2-2.svg" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="100%" />

```r
# plot fluxes as functions of time

in_fluxes <- sol[,grep('influxes',colnames(sol))]
plot( times, sol[,'influxes.C_l'] ,type='l'
  ,ylim=c(min(in_fluxes),max(in_fluxes))
)
lines( times, sol[,'influxes.C_l'] ,type='l'
  ,ylim=c(min(in_fluxes),max(in_fluxes))
)
```

<img src="pkg/man/figures/README-unnamed-chunk-2-3.svg" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="100%" />

```r
internal_fluxes <- sol[,grep('internal_fluxes',colnames(sol))]
plot(
  times, sol[,'internal_fluxes.C_l->C_b'] ,type='l'
  ,ylim=c(min(internal_fluxes),max(internal_fluxes))
)
lines(times,sol[,'internal_fluxes.C_b->C_s'],col='dark red')
lines(times,sol[,'internal_fluxes.C_s->C_b'],col='blue')
```

<img src="pkg/man/figures/README-unnamed-chunk-2-4.svg" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="100%" />

```r
# plot the stocks as functions of time
stocks <- sol[,c('C_l','C_s','C_b')]
plot(times,sol[,'C_l'],type='l',ylim=c(min(stocks),max(stocks)))
lines(times,sol[,'C_s'])
lines(times,sol[,'C_b'])
```

<img src="pkg/man/figures/README-unnamed-chunk-2-5.svg" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="100%" />
-->

## Installation
There are several options.
1. To install the latest bleeding edge commit without cloning the repository, 
you can use the `devtools`package.
If it is not installed yet type:

```r
install.packages("devtools")
```

After installing `devtools`, you only need to run

```r
devtools::install_github('MPIBGC-TEE/SoilR-exp/pkg')
```

2. Download package file (this might be slightly older) 
  go to the [package_builds](https://github.com/MPIBGC-TEE/SoilR-exp/tree/master/scripts/package_builds) directory
  and download the latest SoilR_\*.tar.gz file
  and install it with 
  ```r
  devtools::install_local("/path/to/the/downloaded/SoilR\*.tar.gz",repos=NULL)
  ```
  If you do not want to or cannot use ```devtools``` you can install the dependencies by copying the contents of the script
  [install_dependencies.R](https://github.com/MPIBGC-TEE/SoilR-exp/tree/master/scripts/install_dependencies.R)

3. Clone the repository [Source](#source)
  possibly change files and install the package 
  either 
  * directly with devtools
    ```r
    devtools::install('path/to/SoilR-exp/pkg')
    ```
  * or after building it with 
    ```sh
    R CMD build path/to/SoilR-exp/pkg
    ```

## Source code and contributing


1. You can download the source code of `SoilR` by cloning this repository
<a name='source'></a>
```
git clone https://github.com/MPIBGC-TEE/SoilR-exp.git
```
2. If you are already contributing and want to update a vignette or correct a typo in the documentation 
look at the [tools for package maintenance](scripts/README.md).

3. If you want to contribute new functions or repair bugs, create  a pull request.


## References
To learn more about `SoilR` check the references below, and consult the
[workflows](http://www.bgc-jena.mpg.de/TEE/software/soilr/)
* Sierra, C. A., Müller, M., & Trumbore, S. E. (2014). Modeling radiocarbon dynamics in soils: SoilR version 1.1. Geoscientific Model Development, 7(5), 1919–1931. [doi:10.5194/gmd-7-1919-2014](https://www.geosci-model-dev.net/7/1919/2014/)
* Sierra, C. A., Müller, M., & Trumbore, S. E. (2012). Models of soil organic matter decomposition: the SoilR package, version 1.0. Geosci. Model Dev., 5(4), 1045–1060. [doi:10.5194/gmd-5-1045-2012](https://www.geosci-model-dev.net/5/1045/2012/)

