
# +PATCH,//UNDUMAG/SHELL
# +DECK,mklib_util_incl,T=SHELL.


cd $UNDUMAG_INCL

cd util

if test -e mod/*.f; then

  echo
  echo "Creating $UNDUMAG_INCL/lib/libutil_modules.a"
  echo

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

  ar rc $UNDUMAG_INCL/lib/libutil_modules.a *.o
  ranlib $UNDUMAG_INCL/lib/libutil_modules.a

  cd ..

fi

echo
echo "Creating $UNDUMAG_INCL/lib/libutil.a"
echo

gfortran -c -O2  \
-fbacktrace \
-fcheck=bounds \
-fd-lines-as-comments \
-fopenmp \
-ffixed-line-length-none -finit-local-zero  \
*.f

ar rc $UNDUMAG_INCL/lib/libutil.a *.o
ranlib $UNDUMAG_INCL/lib/libutil.a
