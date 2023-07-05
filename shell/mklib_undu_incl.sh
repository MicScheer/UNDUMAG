
# +PATCH,//UNDUMAG/SHELL
# +DECK,mklib_undu_incl,T=SHELL.

cd $UNDUMAG_INCL

cd for

rm -f *.mod

cd mod

gfortran -c -O2  \
-fbacktrace \
-fcheck=bounds \
-fd-lines-as-comments \
-fopenmp \
-ffixed-line-length-none -finit-local-zero  \
*.f

mv *.mod ..

echo
echo "Creating $UNDUMAG_INCL/lib/libundu_modules.a"
echo

ar rc $UNDUMAG_INCL/lib/libundu_modules.a *.o
ranlib $UNDUMAG_INCL/lib/libundu_modules.a

cd ..

echo
echo "Creating $UNDUMAG_INCL/lib/libundu.a"
echo

gfortran -c -O2  \
-fbacktrace \
-fcheck=bounds \
-fd-lines-as-comments \
-fopenmp \
-ffixed-line-length-none -finit-local-zero  \
*.f

ar rc $UNDUMAG_INCL/lib/libundu.a *.o
ranlib $UNDUMAG_INCL/lib/libundu.a

