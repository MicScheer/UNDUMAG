Install UNDUMAG:

  Having cloned UNDUMAG from Github:

  export UNDU_INCL=Where_you_have_the_clone_of_UNDUMAG
  export LD_LIBRARY_PATH=$UNDU_INCL/dynlib:$LD_LIBRARY_PATH
  (It is recommended to put these exports into you login-script)

  cd $UNDU_INCL

  python3 python/make_undumag.py

  (
  or
  python3 python/make_undumag_intel.py
  or
  python3 python/make_undumag_intel_debug.py
  )

Run UNDUMAG:


 cd stage
 ../bin/undumag.exe

GUI:

 cd stage
 python3 -i ../python/undugui.py

