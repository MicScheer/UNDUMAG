
# +PATCH,//UNDUMAG/PYTHON
# +DECK,undumag_make,T=PYTHON.

import os
import sys
import platform
import glob

def Quit(*args, delay=0):
  #reakpoint()
  nargs =  len(args)

  text = ''
  for i in range(nargs):
    text += str(args[i]) + " "
  #endif

  if delay > 0:

    if len(text):
      print("\n",text, "\nWaiting",delay," seconds before kill")
      #time.sleep(delay)
    else:
      print("\nWaiting",delay," seconds before kill")
      #time.sleep(delay)
    #endif len(text):

    if platform.system() == 'Windows':
      os.system("sleep " + str(delay) + " && taskkill /F /PID " + str(os.getpid()) + " &")
    else:
      os.system("sleep " + str(delay) + " && kill " + str(os.getpid()) + " &")
    #endif platform.system() == 'Windows'

  elif delay < 0:
    return
  else:
    print("\n",text)
    if platform.system() == 'Windows':
      os.system("taskkill /F /PID " + str(os.getpid()))
    else:
      os.system("kill " + str(os.getpid()))
    #endif platform.system() == 'Windows'

#enddef Quit(text = '', delay=0)

global Iverbose,Idry,Idebug,UI

args=sys.argv; nargs = len(args)

try:
  UI = os.environ['UNDUMAG_INCL'] + "/"
except:
  UI = ''
  path = args[0].split("/")
  l = len(path)
  if l == 1:
    path = os.getcwd().split("/")
    path.append(args[0])
  elif l == 2:
    path = os.getcwd().split("/")
    pp = args[0].split("/")
    path.append(pp[0])
    path.append(pp[1])
  #endif
  for i in range(len(path)-2):
    UI += path[i] + "/"
  #endfor
  print("\n*** Warning: Shell variable UNDUMAG_INCL not defined ***")
  print("*** Assuming: ",UI," ***")
  os.system('sleep 3')
#endtry

Iverbose = 0
Idebug = 0
Idry = 0

if nargs > 1:
  try:
    Iverbose = int(args[1])
  except:
    n = '\n'
    print(n)
    print("Usage: python3 " + UI + args[0] + " [verbose level]",n)
    print("To force total recompilation delete ",n,UI + "bin/undumag.exe",n)
    Quit()
  #end try
#endif

if nargs > 2: Idebug = int(args[2])

global Undu_tree,Scomp_bounds,Scomp_omp,Scomp,Texe,Tlib,Scomp_nowarn

Scomp = "gfortran -std=legacy -c -O2 -cpp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops "
Scomp_nowarn = "gfortran -w -std=legacy -c -O2 -cpp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops "
Scomp_bounds = "gfortran -std=legacy -c -O2 -cpp -fcheck=bounds -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons -fno-automatic -ffixed-line-length-none -finit-local-zero -funroll-loops "
Scomp_omp = "gfortran -std=legacy -c -O2 -cpp -finit-local-zero -fcheck=bounds -fopenmp -fbacktrace -ffpe-summary=invalid,zero,overflow -fdec -fd-lines-as-comments -Wno-align-commons -ffixed-line-length-none -funroll-loops "

def get_undu_tree():

  global UI,Undu_tree,Iverbose,Idry,Idebug,Texe,Tlib

  try:
    Texe = os.stat(UI + '/bin/undumag.exe').st_mtime_ns
  except:
    Texe = 0
  #endtry

  top = glob.glob(UI+"/*")

  Undu_tree = []
  #reakpoint()

  for topd in top:

    dd = topd.split("/")[-1]

    if dd == 'cmz' or dd == 'doc' or dd == 'check_system' or dd == 'bin' \
    or dd == 'python' or dd == 'main' or dd == 'lib' or dd == 'shell' \
    or dd == 'clc' or dd == 'nam': continue

    t = os.stat(topd).st_mtime_ns

    modf = glob.glob(topd+"/mod/*.f")

    modfor = []
    for ff in modf:
      f = ff.split("/")[-1]
      tf = os.stat(ff).st_mtime_ns
      modfor.append([f,tf])
    #endfor

    modm = glob.glob(topd+"/*.mod")
    modmod = []
    for ff in modm:
      f = ff.split("/")[-1]
      tf = os.stat(ff).st_mtime_ns
      modmod.append([f,tf])
    #endfor

    cm = glob.glob(topd+"/*.cmn")
    cmn = []
    for ff in cm:
      f = ff.split("/")[-1]
      tf = os.stat(ff).st_mtime_ns
      cmn.append([f,tf])
    #endfor

    ff = glob.glob(topd+"/*.f")
    fort = []
    for fff in ff:
      f = fff.split("/")[-1]
      tf = os.stat(fff).st_mtime_ns
      fort.append([f,tf])
    #endfor

    Undu_tree.append([topd,t,modfor,modmod,cmn,fort])

  #endfor get_undu_tree

#enddef get_undu_tree

def undu_update():

  global UI,Undu_tree,Texe,Scomp_bounds,Scomp_omp,Scomp,Iverbose,Idry,Idebug,Scomp_nowarn

  kmain = 0

  get_undu_tree()

  for td in Undu_tree:

    dd = td[0]
    ds = dd + "/"
    dsm = dd + "/mod/"
    t = td[1]
    modfor = td[2]
    cmn = td[4]
    fort = td[5]

    scomp = Scomp

    lib = ''
    libm = ''
    ranl = 0
    ranlm = 0
    slibm = ''
    slib = ''

    ddd = dd.split("/")[-1]

    if Iverbose >= 0: print("\nProcessing",dd)
    #reakpoint()

    if ddd == 'mshcern':
      lib = UI + 'lib/libmshcern.a'
      libm = UI + 'lib/libmshcern_module.a'
      scomp = Scomp_nowarn
    elif ddd == 'mshplt':
      lib = UI + 'lib/libmshplt.a'
      libm = UI + 'lib/libmshplt_modules.a'
    elif ddd == 'for':
      lib = UI + 'lib/libundu.a'
      libm = UI + 'lib/libundu_modules.a'
      scomp = Scomp_omp
    elif ddd == 'urad':
      lib = UI + 'lib/liburad.a'
      libm = UI + 'lib/liburad_module.a'
      scomp = Scomp_bounds  # uradcfft does boundary tricks
    elif ddd == 'util':
      lib = UI + 'lib/libutil.a'
      libm = UI + 'lib/libutil_module.a'
      scomp = Scomp_bounds
    #endif

    try:
      Tlib = os.stat(lib).st_mtime_ns
      if Tlib > Texe: kmain = 1
    except:
      pass
    #endtry
    try:
      Tlib = os.stat(libm).st_mtime_ns
      if Tlib > Texe: kmain = 1
    except:
      pass
    #endtry

    scompmod = "cd " + dd + "/mod && " + scomp
    scomp = "cd " + dd + " && " + scomp

    #if ddd == 'for': Iverbose=1

    for f in modfor: # Compile modules

      ff = f[0]
      t = f[1]

      if t < Texe: continue

      if Iverbose > 0: print(ff)

      fo = ff[:-1] + "o"
      fm = ff[:-1] + "mod"

      Flines = open(ds+"mod/"+ff,'r')

      while True:
        l = Flines.readline()
        if not l: break
        sl = l.split()
        if len(sl) == 0: continue
        key = sl[0].lower()
        if key== 'module':
          m = sl[1].lower()
          break
        #endif
      #end while
      Flines.close()

      if Iverbose > 0: print("\nModule:",m)

      #if m == 'displacement': #reakpoint()

      scom = scompmod + "-o " + fo + " " + ff
      if Iverbose > 0: print("\n",scom,"\n")
      if Idry == 0: os.system(scom)

      #if Iverbose > 0:
      #print(ff)
      #istat = os.system("ls -la " + UI + "undumag_greeter.f")
      #breakpoint()
      #endif

      scom = 'mv ' + dsm + m + ".mod " + dd
      if Iverbose > 0: print("\n",scom,"\n")
      if Idry == 0: os.system(scom)

      slibm += " " + dsm + fo
      ranlm = 1

      # Search use of module in *.cmn

      for ft in cmn:

        f = ft[0]
        t = ft[1]

        if t < Texe: continue

        Flines = open(ds+f,'r')
        while True:
          l = Flines.readline()
          if Idebug > 1: print(l)
          if not l: break
          if len(l) < 10: break
          sl = l.split()
          if len(sl) > 1:
            key = sl[0].lower()
            if key== 'use':
              if sl[1].lower() == m:
                scom = 'touch ' + ds+f
                if Iverbose > 0: print("\n",scom,"\n")
                if Idry == 0: os.system(scom)
                break
              #endif
            #endif
          #endif
        #end while
        Flines.close()

      #endfor

      # Search use of module in *.f
      for ft in fort:

        f = ft[0]
        t = ft[1]

        if Idebug > 1: print(f)

        Flines = open(ds+f,'r')
        while True:
          l = Flines.readline()
          if not l: break
          if len(l) < 10: break
          sl = l.split()
          if len(sl) > 1:
            key = sl[0].lower()
            if key== 'implicit':
              if sl[1].lower() == 'none': break
            elif key== 'use':
              if sl[1].lower() == m:
                scom = 'touch ' + ds+f
                if Iverbose > 0: print("\n",scom,"\n")
                if Idry == 0: os.system(scom)
                break
              #endif
            #endif
          #endif
        #end while
        Flines.close()

      #endfor

    #endfor modfor

    if ranlm:
      scom = 'ar rc ' + libm + " " + slibm
      if Iverbose > 0: print("\n",scom,"\n")
      if Idry == 0: os.system(scom)
      scom = 'ranlib ' + libm
      if Iverbose > 0: print("\n",scom,"\n")
      if Idry == 0: os.system(scom)
      ranlm = 0
      slibm = ''
      kmain = 1
    #endif

    # Check *.cmn

    for ft in cmn:

      f = ft[0]
      t = os.stat(ds+f).st_mtime_ns

      if t < Texe: continue

      fcmn = f.split("/")[-1]

      for fft in fort:

        Flines = open(ds+fft[0],'r')
        while True:
          l = Flines.readline()
          if not l: break
          if len(l) < 10: break
          sl = l.split()
          if sl[0][0] == '*' or sl[0][0] == '!' or len(sl) < 6: continue
          key = sl[0].lower()
          if key== 'include':
            #reakpoint()
            if sl[1].lower() == "'" + fcmn + "'" or sl[1].lower() == '"' + fcmn + '"':
              scom = 'touch ' + ds+fft[0]
              if Iverbose > 0: print("\n",scom,"\n")
              if Idry == 0: os.system(scom)
              break
            #endif
          #endif
        #end while
        Flines.close()
      #endfor fort

    #endfor cmn

    # Compile *.f if neccessary

    for ft in fort:

      f = ft[0]
      t = os.stat(ds+f).st_mtime_ns

      #if Iverbose > 0:
      #  print(f,t,t-Texe)
      #  if f == 'undumag_iron_residuals.f': breakpoint()
      #endif

      istat = os.system("ls -la " + UI + "undumag_greeter.f 2>/dev/null")
      if not istat:
        print(ff)
        breakpoint()
      #endif

      if t < Texe: continue

      fo = f[:-1] + "o"

      scom = scomp + "-o " + fo + " " + f
      if Iverbose > 0: print("\n",scom,"\n")
      if Idry == 0: os.system(scom)

      slib += " " + ds + fo
      ranl = 1

    #endfor

    if ranl:
      scom = 'ar rc ' + lib + " " + slib
      if Iverbose > 0: print("\n",scom,"\n")
      if Idry == 0: os.system(scom)
      scom = 'ranlib ' + lib
      if Iverbose > 0: print("\n",scom,"\n")
      if Idry == 0: os.system(scom)
      kmain = 1
      ranl = 0
      slib = ''
    #endif

  #endfor dir

  if kmain:
    scom = UI + "shell/compile_undumag_incl.sh"
    if Iverbose > 0: print("\n",scom,"\n")
    if Idry == 0: os.system(scom)
    if Iverbose >=0: print("\n--- " + UI  + "bin/undumag.exe updated ---\n")
  else:
    if Iverbose >=0: print("\n--- No need to update " + UI  + "bin/undumag.exe ---\n")
  #endif

#enddef undu_update

undu_update()
