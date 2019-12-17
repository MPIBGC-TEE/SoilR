#!/bin/bash
# the container hash can be obtained by rounning the following code in R
# install.packages("remotes")
# remotes::install_github("r-hub/rhub")
# chk <- rhub::local_check_linux('/home/mm/SoilR-exp/pkg', image = "rhub/fedora-clang-devel")
containerKey='17bafaee-e259-4b49-9739-13e7fdd9cb32-2' # will change
docker container start ${containerKey} 
docker exec -it  ${containerKey} 
