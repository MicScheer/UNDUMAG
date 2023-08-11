# +PATCH,//UNDUMAG/SHELL
# +DECK,compile_undumag_incl_debug,T=SHELL.

echo " "
echo " "
echo " "

cd $UNDUMAG_INCL

cd main

rm -f ../bin/undumag_debug.exe

gfortran -g -cpp \
-fd-lines-as-comments \
-Wno-align-commons \
-fopenmp \
-fcheck=bounds \
-ffixed-line-length-none -finit-local-zero  \
-funroll-loops \
-o ../bin/undumag_debug.exe \
undumag_main.f \
../lib/libundu_debug.a \
../lib/libundu_modules_debug.a \
../lib/liburad_debug.a \
../lib/libutil_debug.a \
../lib/libmshcern_debug.a \
../lib/libmshplt_debug.a \

cd $UNDUMAG_INCL

