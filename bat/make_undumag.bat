

cd mshcern
del *.o
gfortran -w -std=legacy -c -O2 -cpp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops *.f
ar rc ..\lib\libmshcern.a *.o
cd ..

cd mshplt
del *.mod
del *.o

  cd mod
  gfortran -std=legacy -c -O2 -cpp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons  -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops *.f
  ar rc ..\..\lib\libmshplt_modules.a *.o
  move *.mod ..
  cd ..

gfortran -std=legacy -c -O2 -cpp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons  -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops *.f
ar rc ..\lib\libmshplt.a *.o
cd ..

cd util
del *.mod
del *.o

  cd mod
  gfortran -std=legacy -c -O2 -cpp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons  -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops *.f
  ar rc ..\..\lib\libutil_module.a *.o
  move *.mod ..
  cd ..

gfortran -std=legacy -c -O2 -cpp -fcheck=all -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops *.f
ar rc ..\lib\libutil.a *.o
cd ..

cd urad
gfortran -std=legacy -c -O2 -cpp -fcheck=all -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops *.f
ar rc ..\lib\liburad.a *.o
cd ..

cd for
del *.mod
del *.o

  cd mod
  gfortran -std=legacy -c -O2 -cpp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons  -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops *.f
  ar rc ..\..\lib\libundu_modules.a *.o
  move *.mod ..
  cd ..

gfortran -std=legacy -c -O2 -cpp -finit-local-zero -fcheck=all -fopenmp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons -ffixed-line-length-none -funroll-loops *.f
ar rc ..\lib\libundu.a *.o
cd ..

del main\*.mod
copy for\bpolyederf90m.mod main
copy for\commandlinef90m.mod main
copy for\undumagf90m.mod main

cd main
gfortran -O2 -cpp -fd-lines-as-comments -Wno-align-commons -fopenmp -ffixed-line-length-none -finit-local-zero  -funroll-loops ^
-o ..\bin/undumag.exe undumag_main.f ^
..\lib\libundu.a ..\lib\libundu_modules.a ..\lib\liburad.a ..\lib\libutil.a ..\lib\libmshcern.a ..\lib\libmshplt.a ..\lib\libmshplt_modules.a

cd ..

dir bin
