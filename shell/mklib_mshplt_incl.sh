#!/bin/sh

# +PATCH,//WAVE/SHELL
# +DECK,mklib_mshplt_incl,T=SHELL.

OLDPWD=$PWD

cd $UNDUMAG_INCL/mshplt

echo
echo
echo Compiling "$UNDUMAG_INCL/mshplt/*.f"
echo

gfortran -c -w -O2 -cpp \
-ffpe-summary=invalid,zero,overflow \
-fdec -fd-lines-as-comments \
-Wno-align-commons -fno-automatic -ffixed-line-length-none \
-finit-local-zero \
-funroll-loops \
*.f

echo
echo Making $UNDUMAG_INCL/lib/libmshplt.a
echo

ar rc $UNDUMAG_INCL/lib/libmshplt.a *.o
ranlib $UNDUMAG_INCL/lib/libmshplt.a

cd $OLDPWD
