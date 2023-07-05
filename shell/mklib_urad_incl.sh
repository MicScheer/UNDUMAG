#!/bin/sh

# +PATCH,//UNDUMAG/SHELL
# +DECK,mklib_urad_incl_debug,t=shell.

OLDPWD=$PWD

cd $UNDUMAG_INCL/urad

echo
echo
echo Compiling "$UNDUMAG_INCL/urad/*.f"
echo

gfortran -c -w -O2 -cpp \
-ffpe-summary=invalid,zero,overflow \
-fdec -fd-lines-as-comments \
-Wno-align-commons -fno-automatic -ffixed-line-length-none \
-finit-local-zero \
-funroll-loops \
*.f

echo
echo Making $UNDUMAG_INCL/lib/liburad.a
echo

ar rc $UNDUMAG_INCL/lib/liburad.a *.o
ranlib $UNDUMAG_INCL/lib/liburad.a

cd $OLDPWD
