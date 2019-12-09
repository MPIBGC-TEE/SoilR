#!/bin/bash
# the container hash can be obtained by rounning the following code in R
# pkg_path <- "/home/maelle/Documents/R-hub/test-packages/note"
# chk <- local_check_linux(pkg_path, image = "rhub/fedora-clang-devel")
docker container start  25a91c0b-0d8c-4f65-9ea7-ccf280954926-2
docker exec -it  25a91c0b-0d8c-4f65-9ea7-ccf280954926-2 bash 
