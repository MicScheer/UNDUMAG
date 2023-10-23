# +PATCH,//UNDUMAG/PYTHON
# +DECK,undu_ipylogon,T=PYTHON.

print("--- undumag undu_ipylogon.py ---\n")

import sys
args=sys.argv
arg1 = args[1]
nargs = len(args)

import undumag_plot as u
from undumag_plot import *

optconsole()
set_console_title("unduPython")

ntuples = 1
global Nhead,Ntup,Nind,Nntup

if arg1 == "last":
  try:
    Farg = open("ipylogon.arg","r")
    argl = Farg.readlines()
    Farg.close()
    if len(argl):
      sys.argv = [sys.argv[0]]
      for arg in argl: sys.argv.append(arg.strip())
    #endif
  except: pass
#endif

args=sys.argv
arg1 = args[1]
nargs = len(args)

if not arg1 == "last" and nargs > 0:
  Farg = open("ipylogon.arg","w")
  for i in range(1,nargs): Farg.write(args[i] + "\n")
  Farg.close()
#endif

seed(0)

if nargs > 1:

    if arg1 == "none":
      pass
    elif arg1 == "default":
      pass

    elif arg1 == "load":

      read_facets_bounding_box()
      MainFacets = read_main_faces()
      faces,voxels = read_faces('undumag_facets.fct',cs='xzy')
      undu_read_map()
      undu_geo() # other arguments are evaluated in undu_geo

    elif arg1[:3] == "mag" or arg1 == 'M':

      select = []
      if nargs > 2:
        for ia in range(2,len(args)): select.append(args[ia])
      #endif

      undu_plot_mag_magnetization(select,msize=5.)

    elif arg1[:3] == "seg" or arg1 == "S":

        optconsole()
        set_console_title("unduPython")
        select = []
        if nargs > 2:
          for ia in range(2,len(args)): select.append(args[ia])
        #endif
        undu_plot_mag_3d(select,alpha=1.0)
        getconsole()

    elif arg1 == "onax":

        optconsole()
        set_console_title("unduPython")

        optnstat()

        undu_nbybz()

        nuon = nget('nuon')

        bymax = nuon.by.max()

#        if fexist("unduradia.map"):
#          nrmap = ncread("nrmap","x:y:z:bx:by:bz:b:hx:hy:hz:h:mx:my:mz:m","unduradia.map")
#          nplmls(nrmap,"y:bz")
#          nplmcs(nrmap,"y:bx")
#          bzmaxrad = nrmap.bz.max()
#          text(0.8,0.8,"RADIA <-> UNDUMAG BvMax:" + '\n'+ pg5(bzmaxrad/bymax-1))
#        #endif

    elif arg1 == "beff":

        optconsole()
        set_console_title("unduPython")
        undu_b()
        undu_nbybz_eff()

    elif arg1 == "geo":

        optconsole()
        set_console_title("unduPython")
        undu_geo() # other arguments are evaluated in undu_geo
        getconsole()

    elif arg1 == "map":

        optconsole()
        set_console_title("unduPython")

        undu_read_map()

        if not nexist("nmags"):
          nmags = ncread("nmags","imoth:mag:icol:iplan:icorn:x:y:z:bx:by:bz:imat:cmag:cmoth:ispole","undumag.mag")
        #endif

        if not nexist("nvox"):
          nvox = ncread("nvox","cnam:cmoth:icol:modu:kmag:lmag:ivox:icop:x:y:z:bxi:byi:bzi:bxe:bye:bze:ispole","undumag_voxel.lis")
        #endif

        ninfo("nmap")

        npl("nmap","x:By")
        txyz("","x/mm","By/T")

        getconsole()

    elif arg1 == "radia":

      if fexist("unduradia_mag.map"):
        nmag = ncread("nmag","x:y:z:bx:by:bz:b:hx:hy:hz:h:mx:my:mz:m","unduradia_mag.map")
      #endif

      if fexist("unduradia_pol.map"):
        npol = ncread("npol","x:y:z:bx:by:bz:b:hx:hy:hz:h:mx:my:mz:m","unduradia_pol.map")
      #endif

      if fexist("unduradia.map"):
        nrmap = ncread("nrmap","x:y:z:bx:by:bz:b:hx:hy:hz:h:mx:my:mz:m","unduradia.map")
      #endif

      #npl(nrmap,"y:bz")
      nlist()

      npl(nmag,"y:x:z")

    elif arg1 == "intmap":

        optconsole()
        set_console_title("unduPython")

        if not nexist("nmapint"):
          if fexist("undumag_integral.map"):
            vlis = 'xi:xe:y:z:byint1:bzint1:byint2:bzint2'
            nmapint = ncread("nmapint",vlis,"undumag_integral.map")
          #endif
        #endif

        if not nexist("nmapintinf"):
          vlis = 'y:z:byint1:bzint1:byint1dip:bzint1dip'
          nmapintinf = ncread("nmapintinf",vlis,"undumag_integrals_inf.map")
        #endif not UnduMapRead

        if not nexist("nmapintref"):
          vlis = 'y:z:byint1:bzint1:byint1dip:bzint1dip'
          Fref="undumag_integrals_inf.ref"
          if fexist(Fref):
            nmapintref = ncread("nmapintref",vlis,Fref)
          else:
            nmapintref = ncre("nmapintref","nmapintref",vlis)
          #endif
        #endif not UnduMapRead

        if not nexist("nmags"):
          nmags = ncread("nmags","imoth:mag:icol:iplan:icorn:x:y:z:bx:by:bz:imat:cmag:cmoth:ispole","undumag.mag")
        #endif
        if not nexist("nvox"):
          nvox = ncread("nvox","cnam:cmoth:icol:modu:kmag:lmag:ivox:icop:x:y:z:bxi:byi:bzi:bxe:bye:bze:ispole","undumag_voxel.lis")
        #endif

        npll(nmapintinf,"z:byint1","y==0")
        npllbs(nmapintinf,"z:bzint1","y==0")

    elif arg1 == "coil":

        optconsole()
        set_console_title("unduPython")
        if nargs > 4: ncoil = undu_plot_coil(args[2],args[3],args[4])
        elif nargs > 3: ncoil = undu_plot_coil(args[2],args[3])
        elif nargs > 2: ncoil = undu_plot_coil(args[2])
        else: ncoil = undu_plot_coil()
        getconsole()

    elif arg1 == "ucoilsside":
        import ucoils_plot
        from ucoils_plot import *
        ucoils_plot('side')
    elif arg1 == "ucoilstop":
        import ucoils_plot
        from ucoils_plot import *
        ucoils_plot('top')
    elif arg1 == "ucoilsbeam":
        import ucoils_plot
        from ucoils_plot import *
        ucoils_plot('beam')

#endif nargs > 1

if ntuples:
  for n in range(len(Nhead)):
    snam = Nhead[n][1]
    #print(snam)
    exec(snam + ' = nget("' + snam + '")')
  #endfor
#endif

wans()
