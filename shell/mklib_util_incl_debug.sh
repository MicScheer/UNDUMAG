
# +PATCH,//UNDUMAG/SHELL
# +DECK,mklib_util_incl_debug,t=shell.


cd $UNDUMAG_INCL

cd util

if test -e mod/*.f; then

  echo
  echo "Creating $UNDUMAG_INCL/lib/libutil_modules_debug.a"
  echo

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

  ar rc $UNDUMAG_INCL/lib/libutil_modules_debug.a *.o
  ranlib $UNDUMAG_INCL/lib/libutil_modules_debug.a

  cd ..

fi

echo
echo "Creating $UNDUMAG_INCL/lib/libutil_debug.a"
echo

gfortran -c -g  \
-fbacktrace \
-fcheck=bounds \
-fd-lines-as-comments \
-fopenmp \
-ffixed-line-length-none -finit-local-zero  \
*.f

ar rc $UNDUMAG_INCL/lib/libutil_debug.a *.o
ranlib $UNDUMAG_INCL/lib/libutil_debug.a
