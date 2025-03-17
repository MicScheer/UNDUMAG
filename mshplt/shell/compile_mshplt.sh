#!/bin/bash

#+PATCH,//MSHPLT/SHELL
#+DECK,compile_mshplt,T=SHELL.

# Run this script in the mshplt directory

cd single_file

rm -f mshplt.o

gfortran -c -O3 -cpp \
-finit-local-zero \
-fdec \
-Wno-align-commons \
-fno-automatic \
-fcheck=bounds \
-ffixed-line-length-none \
-funroll-loops \
-o mshplt.o mshplt.f

cd ../example

gfortran -O3 -cpp \
-finit-local-zero \
-fdec \
-Wno-align-commons \
-fno-automatic \
-fcheck=bounds \
-ffixed-line-length-none \
-funroll-loops \
-o ../bin/mshplt_main_example_3d.exe mshplt_main_example_3d.f \
../single_file/mshplt.o

cd ..

