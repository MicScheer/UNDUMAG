
# +PATCH,//UNDUMAG/SHELL
# +DECK,mklib_undu_incl_debug,t=shell.

cd $UNDUMAG_INCL

cd for

rm -f *.mod

cd mod

gfortran -c -g  \
-fbacktrace \
-fcheck=bounds \
-fd-lines-as-comments \
-fopenmp \
-ffixed-line-length-none -finit-local-zero  \
*.f

mv *.mod ..

echo
echo "Creating $UNDUMAG_INCL/lib/libundu_modules_debug.a"
echo

ar rc $UNDUMAG_INCL/lib/libundu_modules_debug.a *.o
ranlib $UNDUMAG_INCL/lib/libundu_modules_debug.a

cd ..

echo
echo "Creating $UNDUMAG_INCL/lib/libundu_debug.a"
echo

gfortran -c -g  \
-fbacktrace \
-fcheck=bounds \
-fd-lines-as-comments \
-fopenmp \
-ffixed-line-length-none -finit-local-zero  \
*.f

ar rc $UNDUMAG_INCL/lib/libundu_debug.a *.o
ranlib $UNDUMAG_INCL/lib/libundu_debug.a

