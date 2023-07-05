# +PATCH,//UNDUMAG/SHELL
# +DECK,compile_undumag_incl,T=SHELL.

echo " "
echo " "
echo " "

cd $UNDUMAG_INCL

cd main

rm -f ../bin/undumag.exe

gfortran -O2 -cpp \
-fd-lines-as-comments \
-Wno-align-commons \
-fopenmp \
-fcheck=bounds \
-ffixed-line-length-none -finit-local-zero  \
-funroll-loops \
-o ../bin/undumag.exe \
undumag_main.f \
../lib/libundu.a \
../lib/libundu_modules.a \
../lib/liburad.a \
../lib/libutil.a \
../lib/libmshcern.a \
../lib/libmshplt.a \

cd $UNDUMAG_INCL

