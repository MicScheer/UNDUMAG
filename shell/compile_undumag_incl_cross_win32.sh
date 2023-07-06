# +PATCH,//UNDUMAG/SHELL
# +DECK,compile_undumag_incl_cross_win32,T=SHELL.

echo " "
echo " "
echo " "

cd $UNDUMAG_INCL_WIN

#rm -f ../bin/undumag_win32.exe

cd main
rm -f *.mod
cd mod

x86_64-w64-mingw32-gfortran-win32 -c -O2 -cpp \
-fd-lines-as-comments \
-Wno-align-commons \
-fopenmp \
-fcheck=bounds \
-ffixed-line-length-none -finit-local-zero  \
-funroll-loops \
*.f

mv *.mod ..

cd ..

x86_64-w64-mingw32-gfortran-win32 -O2 -cpp -static \
-fd-lines-as-comments \
-Wno-align-commons \
-fopenmp \
-fcheck=bounds \
-ffixed-line-length-none -finit-local-zero  \
-funroll-loops \
-o ../bin/undumag_win32.exe \
undumag_main.f \
../lib/libundu.a \
../lib/libundu_modules.a \
../lib/liburad.a \
../lib/libutil.a \
../lib/libmshcern.a \
../lib/libmshplt.a \

cd $UNDUMAG_INCL_WIN

