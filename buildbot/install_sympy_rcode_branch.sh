# althouhg the following packages are mentioned in setup.py of bgc_md as dependencies
# the install int the order determined by setuptools seems to fail 
which python3
pip3 install --upgrade pip
pip3 install concurrencytest
pip3 install numpy
pip3 install matplotlib
pip3 install mpi4py
pip3 install scipy

cd sympy
python3 setup.py develop
cd ..

