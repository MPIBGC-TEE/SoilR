# SoilR: Models of Soil Organic Matter Decomposition
This is the development version of SoilR. It is a rapidly evolving version that may be less
stable than the official stable version in [CRAN](https://cran.r-project.org/web/packages/SoilR/index.html).

## Documentation
[latest build](https://mpibgc-tee.github.io/SoilR-exp/)

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
1. Download package file (this might be slightly older) 
  go to the [builds](https://github.com/MPIBGC-TEE/SoilR-exp/tree/master/builds) directory
  and download the latest SoilR_\*.tar.gz file
  and install it with 
  ```r
  install.packages("/path/to/the/downloaded/SoilR\*.tar.gz",repos=NULL)
  ```
1. Clone the rpository [Source](#source)
  possibly change files and install the package 
  either 
  * with directly with devtools
    ```r
    devtools::install('path/to/SoilR-exp/pkg')
    ```
  * or after building it with 
    ```sh
    R CMD build path/to/SoilR-exp/pkg
    ```

## Source code and contributing
<a name='source'></a>
You can download the source code of `SoilR` by cloning this repository

```
git clone https://github.com/MPIBGC-TEE/SoilR-exp.git
```

If you want to contribute new functions or repair bugs, send us a pull request.

## References
To learn more about `SoilR` check the references below, and consult the
[workflows](http://www.bgc-jena.mpg.de/TEE/software/soilr/)
* Sierra, C. A., Müller, M., & Trumbore, S. E. (2014). Modeling radiocarbon dynamics in soils: SoilR version 1.1. Geoscientific Model Development, 7(5), 1919–1931. [doi:10.5194/gmd-7-1919-2014](https://www.geosci-model-dev.net/7/1919/2014/)
* Sierra, C. A., Müller, M., & Trumbore, S. E. (2012). Models of soil organic matter decomposition: the SoilR package, version 1.0. Geosci. Model Dev., 5(4), 1045–1060. [doi:10.5194/gmd-5-1045-2012](https://www.geosci-model-dev.net/5/1045/2012/)
