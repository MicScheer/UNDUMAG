#!/usr/bin/env python

##############################################################################
#
#      Copyright 2013 Helmholtz-Zentrum Berlin (HZB)
#      Hahn-Meitner-Platz 1
#      D-14109 Berlin
#      Germany
#
#      Author Michael Scheer, Michael.Scheer@Helmholtz-Berlin.de
#
# -----------------------------------------------------------------------
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy (wave_gpl.txt) of the GNU General Public
#    License along with this program.
#    If not, see <http://www.gnu.org/licenses/>.
#
#    Dieses Programm ist Freie Software: Sie koennen es unter den Bedingungen
#    der GNU General Public License, wie von der Free Software Foundation,
#    Version 3 der Lizenz oder (nach Ihrer Option) jeder spaeteren
#    veroeffentlichten Version, weiterverbreiten und/oder modifizieren.
#
#    Dieses Programm wird in der Hoffnung, dass es nuetzlich sein wird, aber
#    OHNE JEDE GEWAEHRLEISTUNG, bereitgestellt; sogar ohne die implizite
#    Gewaehrleistung der MARKTFAEHIGKEIT oder EIGNUNG FueR EINEN BESTIMMTEN ZWECK.
#    Siehe die GNU General Public License fuer weitere Details.
#
#    Sie sollten eine Kopie (wave_gpl.txt) der GNU General Public License
#    zusammen mit diesem Programm erhalten haben. Wenn nicht,
#    siehe <http://www.gnu.org/licenses/>.
#
##############################################################################

# +PATCH,//WAVES/PYTHON
# +DECK,waves,T=PYTHON.

#!/usr/bin/env python

# +PATCH,//WAVES/PYTHON
# +KEEP,imports,T=PYTHON.

import subprocess, glob, os, sys, time, re, fileinput, shutil, platform, copy

import matplotlib as mpl
mpl.use('TkAgg')

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits import mplot3d
from matplotlib import cm #color maps

from tkinter import *
from tkinter.font import Font
import tkinter as tk
from tkinter import ttk

from math import *

import numpy as np
import pandas as pd

from scipy import stats
from scipy import special
from scipy import interpolate
from scipy.stats import *
from scipy.interpolate import interp1d
from scipy.spatial import ConvexHull, convex_hull_plot_2d

from PIL import Image

from copy import *
###################################################

# +PATCH,//WAVES/PYTHON
# +KEEP,mshutil,T=PYTHON.


global Klold, Nold, Xa1old, Xanold

Klold = 1
Nold = -99
Xa1old = -9999.
Xanold = -9999.

import numpy as np

def pe2(x):
  try: return '{:.2e}'.format(float(x))
  except: return x
def pe3(x):
  try: return '{:.3e}'.format(float(x))
  except: return x
def pe4(x):
  try: return '{:.4e}'.format(float(x))
  except: return x
def pe5(x):
  try: return '{:.5e}'.format(float(x))
  except: return x
def pe6(x):
  try: return '{:.6e}'.format(float(x))
  except: return x
def pe7(x):
  try: return '{:.7e}'.format(float(x))
  except: return x
def pe8(x):
  try: return '{:.8e}'.format(float(x))
  except: return x
def pe9(x):
  try: return '{:.9e}'.format(float(x))
  except: return x
def pe10(x):
  try: return '{:.10e}'.format(float(x))
  except: return x

def pg2(x):
  try: return '{:.2g}'.format(float(x))
  except: return x
def pg3(x):
  try: return '{:.3g}'.format(float(x))
  except: return x
def pg4(x):
  try: return '{:.4g}'.format(float(x))
  except: return x
def pg5(x):
  return '{:.5g}'.format(float(x))
#  try: return '{:.5g}'.format(float(x))
#  except: return x
def pg6(x):
  try: return '{:.6g}'.format(float(x))
  except: return x
def pg7(x):
  try: return '{:.7g}'.format(float(x))
  except: return x
def pg8(x):
  try: return '{:.8g}'.format(float(x))
  except: return x
def pg9(x):
  try: return '{:.9g}'.format(float(x))
  except: return x
def pg10(x):
  try: return '{:.10g}'.format(float(x))
  except: return x

def readfloat(s,default=-9999):
  print(s)
  ans = input()
  if ans == '': return default
  else: return float(ans)
#enddef

def readint(s,default=-9999):
  print(s)
  ans = input()
  if ans == '': return default
  else: return int(float(ans))
#enddef

def printnl(line):
  print("\n",line,"\n")
#enddef printnl()

def set_console_title(console='Python'):
#+seq,mshimportsind.
# +PATCH,//WAVES/PYTHON
# +KEEP,statusglobind,T=PYTHON.
  global Istatus, WarningText, ErrorText, Gdebug

  # Histograms and Ntuples
  global H1h, H1hh, H2h, H2hh, H1, H2, H1head, H2head, H1HLast, Nhead, Ntup, \
  Nctup, Nh1, Nh2, Nntup, Nnctup, Hdir, Ndir, Kdir, Cdir, Fdir, \
  H1Last, H2Last, NLast, H1h, H2h, N, Nct, Ind, IndLast, \
  Nmin, Nmax, Nmean, Nrms, Nxopt, Nyopt, Nlook, Nsum, \
  Tdf, Tfig, Tax, Tax3d, Tax2d , H1ind, H2ind, Ncind, \
  H1ILast, NiLast, H1I, H2I, H2ILast, Ni, NctI, Nind, Nsel, Nlines, Ncolon, \
  FitPar, FitFit, FitSig, FitChi2ndf, FitNdf, FitChi2Prob,Figman,TnpFloat64,Tnpcmpl128
#+KEEP,plotglobind,T=PYTHON.
#*CMZ :          28/09/2019  14.39.13  by  Michael Scheer
  global MPLmain, MPLmaster, Nfigs,Figgeom, Figgeom2, FiggeomR, FiggeomL, XtermGeo, Figs,Fig,Ax,\
  Fig1,Ax1,Fig6,Ax6,Fig2,Ax2,Fig7,Ax7,Fig3,Ax3,Fig8,Ax8, Figgeoms, \
  Fig4,Ax4,Fig9,Ax9,Fig5,Ax5,Fig10,Ax10,\
  Screewidth, Screenheight, ScaleSizeX, ScaleSizeY, \
  FirstConsole, Console, Igetconsole,Klegend, Fwidth, Fheight, Fxoff, Fyoff, \
  Kfig, Kax, Ihist,Iprof, Imarker, Ierr, Isurf, Iinter, Isame, Itight, IsameGlobal, Iline, CMap, Cmap, Tcmap, Surfcolor, Cmaps, \
  Iplotopt, Ispline, Kecho, Kdump,Kpdf, Ndump,Npdf, Legend, \
  Kplots,Nwins, Zones, Kzone, Nxzone, Nyzone, Zone, Axes, Icmap, \
  Mode3d,Mode3D, Mode2d,Mode2D, CanButId, CanButIds, \
  MarkerSize, MarkerType, MarkerColor, \
  Markersize, Markertype, Markercolor, \
  Fillstyle, FillStyle, \
  Textcolor, WaveFilePrefix,WaveDump, \
  LineStyle, LineWidth, LineColor, \
  Linestyle, Linewidth, Linecolor, \
  Author, \
  Tightpad, Xtightpad,Ytightpad, ColorbarPad,\
  LeftMargin,RightMargin,TopMargin,BottomMargin, Xspace, Yspace, \
  Histcolor, Histedgecolor, Histbarwidth, Kdate, Kfit, Kstat, YTitle, YGTitle,x_of_xlab,y_of_xlab,x_of_ylab,y_of_ylab, Ygtitle, \
  Icont3d, Iboxes, Inoempty, Iclosed,Itrisurf, Iscatter, Iscat3d, Ifill1d, TitPad, Xtitle, Ytitle, \
  Gtit,Xtit,Ytit,Ztit,Ttit,Ptit,Colors, Surfcolors,Linestyles, Markertypes, \
  LexpX,LexpY,LexpRot,LexpPow,\
  GtitFontSize,Titfontsize,Atitfontsize,Axislabelsize,Textfontsize,Datefontsize,\
  Statfontsize, Axislabeldist, Axislabeldist3d, Axisdist, Axisdist3d, \
  XFit, YFit, Xfit, Yfit,Ystat, YStat, \
  GtitFontSize,TitFontSize,AtitFontSize,AxisLabelSize,TextFontSize,DateFontSize,\
  StatFontSize, AxisLabelDist, AxisLabelDist3d, AxisTitleDist, AxisTitleDist3d, \
  AtitFontSize3d, Atitfontsize3d, NXtick,NXtick3d, Nxtick,Nxtick3d, Ktitles,  Dummy,\
  ZoomXmin,ZoomXmax, ZoomYmin, ZoomYmax,ZoomZmin,ZoomZmax,\
  Tdate, TdateOv, Trun, TrunOv, Icallfromoverview,\
  LogX,LogY, LogZ, NxBinMax, Khdeleted, Waveplot, \
  Mrun, Mcomment, Mdate, ROFx, Rofy, Hull2D,Hull3DList,THull3D,Hull3D, Kgrid, KxAxis,KyAxis,KzAxis,Kbox, \
  FillColor,WisLinux,Ishow,Sepp,Backslash
#+PATCH,//WAVES/PYTHON
#+KEEP,vecglobind,T=PYTHON.

  global VsortX, VsortY, VoptX, VoptY, VsplX, VsplY, Vspl1, Vspl2, VsplI, \
  VsplCoef, Nspline,Ninter, Nfitxy, Nfitint, Vxint, Vyint, SplineMode, \
  VxyzX,VxyzY,VxyzZ,Tnpa,Tnone,VxyzE

#+KEEP,nxyzglobind,T=PYTHON.
#*CMZ :          29/09/2019  11.11.01  by  Michael Scheer
  global N1, N2, N3, N4, N5, N6, N7,N8,N9,Nv, Nx, Nxy, Nxyz


  Console = console

  if platform.system() == 'Linux':
    sys.stdout.write("\x1b]2;" + console + "\x07")
  elif platform.system() == 'Windows':
    #ctypes.windll.kernel32.SetConsoleTitleW(console)
    os.system("title "+console)
  #endif

#enddef set_console_title()

def util_spline_coef_periodic(x,y):

  """
c--- calculates spline coefficients for periodic function
c--- the interval must be closed, i.e. x(n)-x(1)=periodlength and y(n)=y(1)

c--   input:

c-       n: number of x,y-values, must be at least five
c-       x: array of x-values
c-       y: array of y-values

c--   ouput:

c-       ypp:   spline-coefficients
c-     ifail:   error status

c--   workingspace: aa(n),bb(n),cc(n),c(n),cn(n)
  """
  n = len(x)

  ypp =  np.zeros_like(x)

  aa = np.zeros_like(x)
  bb = np.zeros_like(x)
  cc = np.zeros_like(x)
  c  = np.zeros_like(x)
  cn = np.zeros_like(x)

  if n < 5:
    ifail=-1
    return ifail, ypp
  #endif

  ymax=0.0
  ya = abs(y)
  ymax = ya.max()

  n -= 1 # now n is last index

  if abs(y[n]-y[0])/ymax > 1.0e-9:
    ifail=-2
    return ifail, yp
  else:
    y[0]=(y[n]+y[0])/2.0
    y[n]=y[0]
  #endif

  #cn=0.0 #letzte Spalte der Matrix

  n1=n-1
  n2=n-2

  aa[0]=(x[n]-x[n1])/6.e0
  bb[0]=(x[1]-x[0]+(x[n]-x[n1]))/3.e0
  cc[0]=(x[1]-x[0])/6.e0
  c[0]=(y[1]-y[0])/(x[1]-x[0])-(y[n]-y[n1])/(x[n]-x[n1])

  for j in range(1,n):
    #do j=2,n1
    aa[j]=(x[j]-x[j-1])/6.0
    bb[j]=(x[j+1]-x[j-1])/3.0
    cc[j]=(x[j+1]-x[j])/6.0
    c[j]=(y[j+1]-y[j])/(x[j+1]-x[j])-(y[j]-y[j-1])/(x[j]-x[j-1])
    #print(j,aa[j],bb[j],cc[j],c[j])
    #print(j,x[j],y[j])
  #enddo #j

  # Auf Dreiecksmatrix bringen

  # Oberste Zeile

  cc[0]=cc[0]/bb[0]
  aa[0]=aa[0]/bb[0]
  c[0] = c[0]/bb[0]
  bb[0]=1.0

  # 2. Zeile, d.h. die erste regulaere, cn[j] ist die letzte Spalte der Matrix

  bb[1]=bb[1]/aa[1]
  cc[1]=cc[1]/aa[1]
  c[1] = c[1]/aa[1]
  aa[1]=1.0

  aa[1]=0.0
  bb[1]=bb[1]-cc[0]
  cn[1]=-aa[0]
  c[1] = c[1]-c[0]

  cc[1]=cc[1]/bb[1]
  cn[1]=cn[1]/bb[1]
  c[1] = c[1]/bb[1]

  bb[1]=1.0

  # Nun die hoeheren bis n-3

  for j in range(2,n-3):
    #do j=3,n-3

    bb[j]=bb[j]/aa[j]
    cc[j]=cc[j]/aa[j]
    c[j] = c[j]/aa[j]

    aa[j]=0.0
    bb[j]=bb[j]-cc[j-1]
    cn[j]=-cn[j-1]
    c[j] = c[j]-c[j-1]

    cc[j]=cc[j]/bb[j]
    cn[j]=cn[j]/bb[j]
    c[j] = c[j]/bb[j]

    bb[j]=1.0

  #enddo

  # vorletzte zeile

  bb[n2]=bb[n2]/aa[n2]
  cc[n2]=cc[n2]/aa[n2]
  c[n2] = c[n2]/aa[n2]
  aa[n2]=1.0

  aa[n2]=0.0
  bb[n2]=bb[n2]-cc[n2-1]
  cc[n2]=cc[n2]-cn[n2-1]
  c[n2] = c[n2]-c[n2-1]

  cc[n2]=cc[n2]/bb[n2]
  c[n2] = c[n2]/bb[n2]

  bb[n2]=1.0

  # Letzte Zeile

  ypp[n2]=aa[n1]/cc[n1]
  ypp[n1]=bb[n1]/cc[n1]
  c[n1]=c[n1]/cc[n1]
  cc[n1]=1.0
  ypp[0]=cc[n1]

  # Oberste Zeile abziehen

  ypp[0]=0.0
  ypp[1]=-cc[0]
  ypp[n1]=ypp[n1]-aa[0]
  c[n1]=c[n1]-c[0]

  for j in range(1,n1):
    #do j=2,n2

    c[n1]=c[n1]/ypp[j]
    ypp=ypp/ypp[j]

    ypp[j]=ypp[j]-bb[j]
    ypp[j+1]=ypp[j+1]-cc[j]
    ypp[n1]=ypp[n1]-cn[j]
    c[n1]=c[n1]-c[j]

  #enddo

  c[n1]=c[n1]/ypp[n1]

  # Ernten

  ypp[n1]=c[n1]

  # Vorletzte Zeile

  bb[n2]=bb[n2]/cc[n2]
  c[n2]=c[n2]/cc[n2]
  cc[n2]=1.0

  c[n2]=c[n2]-c[n1]
  cc[n2]=0.0

  c[n2]=c[n2]/bb[n2]
  bb[n2]=1.0

  ypp[n2]=c[n2]

  # Letzte Spale nullen und normieren, letzte und vorletzte sind bereits fertig

  bb[0]=bb[0]/aa[0]
  cc[0]=cc[0]/aa[0]
  c[0]=c[0]/aa[0]
  aa[0]=1.0

  c[0]=c[0]-c[n1]
  aa[0]=0.0

  cc[0]=cc[0]/bb[0]
  c[0]=c[0]/bb[0]
  bb[0]=1.0


  # Regulaere Zeilen
  for j in range(1,n-3):
    #do j=2,n-3

    bb[j]=bb[j]/cn[j]
    cc[j]=cc[j]/cn[j]
    c[j]=c[j]/cn[j]
    cn[j]=1.0

    c[j]=c[j]-c[n1]
    cn[j]=0.0

    cc[j]=cc[j]/bb[j]
    c[j]=c[j]/bb[j]
    bb[j]=1.0

  #enddo

  j=n-2
  while j > 1:
    j -= 1
    #do j=n-3,2,-1
    ypp[j]=c[j]-cc[j]*ypp[j+1]
  #enddo

  ypp[0]=(c[0]-cc[0]*ypp[1]-aa[0]*ypp[n1])/bb[0]
  ypp[n]=ypp[0]

  ifail=0

  return ifail, ypp
#enddef util_spline_coef_periodic(x,y)

def util_spline_inter(xa,ya,y2a,x,mode):


  global Klold, Nold, Xa1old, Xanold


  """
C---  INTERPOLATES Y(X) VIA SPLINE

C--   INPUT:

C-       XA:   ARRAY OF X-VALUES
C-       YA:   ARRAY OF Y-VALUES
C-       YA2:  ARRAY SPLINE COEFFICIENTS
C-       X: Y(X) IS CALCULATED
C-       MODE: CONTROL FLAG:
C-             MODE.GE.0: USE VALUES OF LAST CALL TO START WITH
C-             MODE.LT.0: NEW INITIALIZATION

C--   OUTPUT: Y(X), DY/DX(X), D2Y/DX2(X)

  """
  y = None
  yp = None
  ypp = None

  n = len(xa) - 1

  eps=abs(xa[n]-xa[0])/1.0e10
  xx=x

  if xa[0] > xa[n]:
    print('*** Error in util_spline_inter: x-values must be in ascending order ***')
    Quit()
  #endif xa[0] > xa[n]:

  if xx < xa[0] and xx > xa[0]-eps:
    xx=xa[0]
  elif xx > xa[n] and xx < xa[n]+eps:
    xx=xa[n]
  #endif

  if xx < xa[0] or xx > xa[n]:
    print('xa[0], xa[n]:',xa[0], xa[n])
    print('x:',x)
    print('*** Error in util_spline_inter: x out of range ***')
    return y,yp,ypp
  #endif

  if mode < 0 or Klold >= n:
    klo=0
  elif Nold == n and  xa[0] == Xa1old and  xa[n] == Xanold and  xx > xa[Klold]:
    klo=Klold
  else:
    klo=0
  #endif

  if xx < xa[klo+1]:
    khi=klo+1
  else:
    khi=n
    while (khi-klo) > 1:
      k=int((khi+klo)/2)
      if xa[k] > xx:
        khi=k
      else:
        klo=k
      #endif
    #endwhile
  #endif

  h=xa[khi]-xa[klo]

  if h <= 0.0:
    print('*** error in util_spline_inter: bad input ***')
    return y,yp,ypp
  #endif

  a=(xa[khi]-xx)/h
  b=(xx-xa[klo])/h

  yl = ya[klo]
  yh = ya[khi]
  y2l = y2a[klo]
  y2h = y2a[khi]

  y = a*yl+b*yh+(a*(a+1.)*(a-1.)*y2l+b*(b+1.)*(b-1.)*y2h)*(h**2)/6.
  yp = (yh-yl)/h+((3.0*b*b-1.0)*y2h-(3.0*a*a-1.0)*y2l)/6.0*h
  ypp = y2l + (y2h-y2l)/h*(xx-xa[klo])

  Klold=klo
  Nold=n
  Xa1old=xa[0]
  Xanold=xa[n]

  return y,yp,ypp

#enddef util_spline_inter(xa,ya,y2a,x,y,mode)

def util_spline_coef(x,y,yp1=9999.,ypn=9999.):

#+seq,mshimportsind.
# +PATCH,//WAVES/PYTHON
# +KEEP,statusglobind,T=PYTHON.
  global Istatus, WarningText, ErrorText, Gdebug

  # Histograms and Ntuples
  global H1h, H1hh, H2h, H2hh, H1, H2, H1head, H2head, H1HLast, Nhead, Ntup, \
  Nctup, Nh1, Nh2, Nntup, Nnctup, Hdir, Ndir, Kdir, Cdir, Fdir, \
  H1Last, H2Last, NLast, H1h, H2h, N, Nct, Ind, IndLast, \
  Nmin, Nmax, Nmean, Nrms, Nxopt, Nyopt, Nlook, Nsum, \
  Tdf, Tfig, Tax, Tax3d, Tax2d , H1ind, H2ind, Ncind, \
  H1ILast, NiLast, H1I, H2I, H2ILast, Ni, NctI, Nind, Nsel, Nlines, Ncolon, \
  FitPar, FitFit, FitSig, FitChi2ndf, FitNdf, FitChi2Prob,Figman,TnpFloat64,Tnpcmpl128
#+KEEP,plotglobind,T=PYTHON.
#*CMZ :          28/09/2019  14.39.13  by  Michael Scheer
  global MPLmain, MPLmaster, Nfigs,Figgeom, Figgeom2, FiggeomR, FiggeomL, XtermGeo, Figs,Fig,Ax,\
  Fig1,Ax1,Fig6,Ax6,Fig2,Ax2,Fig7,Ax7,Fig3,Ax3,Fig8,Ax8, Figgeoms, \
  Fig4,Ax4,Fig9,Ax9,Fig5,Ax5,Fig10,Ax10,\
  Screewidth, Screenheight, ScaleSizeX, ScaleSizeY, \
  FirstConsole, Console, Igetconsole,Klegend, Fwidth, Fheight, Fxoff, Fyoff, \
  Kfig, Kax, Ihist,Iprof, Imarker, Ierr, Isurf, Iinter, Isame, Itight, IsameGlobal, Iline, CMap, Cmap, Tcmap, Surfcolor, Cmaps, \
  Iplotopt, Ispline, Kecho, Kdump,Kpdf, Ndump,Npdf, Legend, \
  Kplots,Nwins, Zones, Kzone, Nxzone, Nyzone, Zone, Axes, Icmap, \
  Mode3d,Mode3D, Mode2d,Mode2D, CanButId, CanButIds, \
  MarkerSize, MarkerType, MarkerColor, \
  Markersize, Markertype, Markercolor, \
  Fillstyle, FillStyle, \
  Textcolor, WaveFilePrefix,WaveDump, \
  LineStyle, LineWidth, LineColor, \
  Linestyle, Linewidth, Linecolor, \
  Author, \
  Tightpad, Xtightpad,Ytightpad, ColorbarPad,\
  LeftMargin,RightMargin,TopMargin,BottomMargin, Xspace, Yspace, \
  Histcolor, Histedgecolor, Histbarwidth, Kdate, Kfit, Kstat, YTitle, YGTitle,x_of_xlab,y_of_xlab,x_of_ylab,y_of_ylab, Ygtitle, \
  Icont3d, Iboxes, Inoempty, Iclosed,Itrisurf, Iscatter, Iscat3d, Ifill1d, TitPad, Xtitle, Ytitle, \
  Gtit,Xtit,Ytit,Ztit,Ttit,Ptit,Colors, Surfcolors,Linestyles, Markertypes, \
  LexpX,LexpY,LexpRot,LexpPow,\
  GtitFontSize,Titfontsize,Atitfontsize,Axislabelsize,Textfontsize,Datefontsize,\
  Statfontsize, Axislabeldist, Axislabeldist3d, Axisdist, Axisdist3d, \
  XFit, YFit, Xfit, Yfit,Ystat, YStat, \
  GtitFontSize,TitFontSize,AtitFontSize,AxisLabelSize,TextFontSize,DateFontSize,\
  StatFontSize, AxisLabelDist, AxisLabelDist3d, AxisTitleDist, AxisTitleDist3d, \
  AtitFontSize3d, Atitfontsize3d, NXtick,NXtick3d, Nxtick,Nxtick3d, Ktitles,  Dummy,\
  ZoomXmin,ZoomXmax, ZoomYmin, ZoomYmax,ZoomZmin,ZoomZmax,\
  Tdate, TdateOv, Trun, TrunOv, Icallfromoverview,\
  LogX,LogY, LogZ, NxBinMax, Khdeleted, Waveplot, \
  Mrun, Mcomment, Mdate, ROFx, Rofy, Hull2D,Hull3DList,THull3D,Hull3D, Kgrid, KxAxis,KyAxis,KzAxis,Kbox, \
  FillColor,WisLinux,Ishow,Sepp,Backslash
#+PATCH,//WAVES/PYTHON
#+KEEP,vecglobind,T=PYTHON.

  global VsortX, VsortY, VoptX, VoptY, VsplX, VsplY, Vspl1, Vspl2, VsplI, \
  VsplCoef, Nspline,Ninter, Nfitxy, Nfitint, Vxint, Vyint, SplineMode, \
  VxyzX,VxyzY,VxyzZ,Tnpa,Tnone,VxyzE

#+KEEP,nxyzglobind,T=PYTHON.
#*CMZ :          29/09/2019  11.11.01  by  Michael Scheer
  global N1, N2, N3, N4, N5, N6, N7,N8,N9,Nv, Nx, Nxy, Nxyz


  """
C--- calculates spline coefficients

C--   input:

C-       x: array of x-values
C-       y: array of y-values
C-       yp1:  second derivative at first x-value
C-       ypn:  second derivative at last x-value

C--   ouput:

C-       y2:   spline-coefficients

C--   workingspace: aa(n),bb(n),cc(n),c(n)
  """
  n = len(x)

  y2 = np.zeros_like(x)

  aa = np.zeros_like(x)
  bb = np.zeros_like(x)
  cc = np.zeros_like(x)
  c  = np.zeros_like(x)

  ifail = 0

  if n < 3 :
      if abs(yp1) == 9999.0 :
          y2[0]=0.0
      else:
          y2[0]=yp1
      #endif
      if abs(ypn) == 9999.0 :
          y2[n-1]=0.0
      else:
          y2[n-1]=ypn
      #endif
      return ifail, y2
  #endif

  if abs(yp1) == 9999.0 :
      xx=x[0:3]
      yy=y[0:3]
      a, yp, opt, kfail = util_parabel(xx,yy)
      if kfail == 0 or kfail == 2:
          y2[0]=2.0*a[2]
      else:
          y2[0]=0.0
      #endif
  else:
      y2[0]=yp1
  #endif

  if abs(ypn) == 9999.0 :
      xx=x[-3:]
      yy=y[-3:]
      a, yp, opt, kfail = util_parabel(xx,yy)
      if kfail == 0 or kfail == 2:
          y2[n-1]=2.0*a[2]
      else:
          y2[n-1]=0.0
      #endif
  else:
      y2[n-1]=ypn
  #endif

  c[0]=y2[0]
  c[n-1]=y2[n-1]

  bb[0]=1.0
  cc[0]=0.0
  cc[n-1]=1.0

  j=1
  while j < n-1:
    if x[j+1] == x[j] :
      print('*** error in util_spline_coef: intervall of zero length')
      print('j, x[j], x[j+1]:',j,x[j],x[j+1])
      return -1
    #endif
    aa[j]=(x[j]-x[j-1])/6.0
    bb[j]=(x[j+1]-x[j-1])/3.0
    cc[j]=(x[j+1]-x[j])/6.0
    c[j]=(y[j+1]-y[j])/(x[j+1]-x[j])-(y[j]-y[j-1])/(x[j]-x[j-1])
    j += 1
  #enddo !j

  j = 0
  while j < n - 2:
  #do j=2,n-1

    j += 1

    bb[j]=bb[j]-aa[j]*cc[j-1]
    c[j]= c[j]-aa[j]* c[j-1]
    #aa[j]=aa[j]-aa[j]*bb[j-1]

    cc[j]=cc[j]/bb[j]
    c[j]= c[j]/bb[j]
    bb[j]=1.0

  #enddo !j

  j = n - 1
  while j > 1:
  #do j=n-1,2,-1
    j -= 1
    y2[j]=c[j]-cc[j]*y2[j+1]
    if abs(y2[j]) < 1.0e-15: y2[j]=0.0
  #enddo
  #endwhile j > 2

  return ifail, y2

#enddef util_spline_coeff(x,y,yp1=9999.,ypn=9999.)

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

    set_console_title(os.getcwd())

    if platform.system() == 'Windows':
      stat = os.system("sleep " + str(delay) + " && taskkill /F /PID " + str(os.getpid()) + " &")
    else:
      stat = os.system("sleep " + str(delay) + " && kill " + str(os.getpid()) + " &")
    #endif platform.system() == 'Windows'

  elif delay < 0:
    return
  else:
    print("\n",text)
    set_console_title(os.getcwd())
    if platform.system() == 'Windows':
      stat = os.system("taskkill /F /PID " + str(os.getpid()))
    else:
      stat = os.system("kill " + str(os.getpid()))
    #endif platform.system() == 'Windows'

#enddef Quit(text = '', delay=0)

def exit(text = ''): Quit(text)

def Wexit(ew=''):
  if type(ew) == str:
    Quit(ew)
  else:
    Quit()

def qwait(delay=3):
  Quit("",delay)

def sleep(isec):
  time.sleep(isec)

def wait(isec=1000,text='waiting'):
  if text: print("--- ",text," for ",isec," seconds ---")
  time.sleep(isec)

def util_parabel(xin,yin):

  """
  C--- CALCULATES A(1),A(2),A(3), THE DERIVATIVES YP(X(1)),YP(X(2)),YP(X(3)),
  C    AND THE EXTREMUM (XOPT,A(XOPT)) OF PARABOLA Y=A1+A2*X+A3*X**2
  C    FROM COORDINATES OF THE THREE POINTS (X(1),Y(1)),(X(2),Y(2)),(X(3),Y(3))
  C
  """

  #reakpoint()
  a = [-9999.,-9999.,-9999.]
  yp = [-9999.,-9999.,-9999.]
  opt = [-9999.,-9999.]
  ifail = 0

# calculate f=a0+a1*(x-x0)+a2*(x-x0)**2
#  = a0 + a1*x - a0*x0 + a2*x**2 - 2*a2*x*x0 + a2*x0**2
#  = a0 + (a2*x0 -a0)*x0 + (a1 - 2*a2*x0 )*x+ a2*x**2

# change system: (x0,s0)->(0,0), i.e.
# calculate f=a1*dx+a2*dx**2
#  df/dx=a1+2*a2*dx_max =! 0, dx_max=-a1/2/a2

  #print("util_parabel:",xin,"\n",yin)
  xy = pd.DataFrame(columns=['x','y'])
  xy.x = xin
  xy.y = yin
  xy = xy.sort_values(by='x')

  x = xy.x
  y = xy.y

  x.index = range(len(x))
  y.index = range(len(y))

  x0=x[1]
  f0=y[1]

  fm=y[0]-f0
  fp=y[2]-f0

  dxm=x[0]-x0
  dxp=x[2]-x0

  # fm=a1*dxm+a2*dxm**2
  # fp=a1*dxp+a2*dxp**2

  # (dxm dxm2) (a1) = (y[0])
  # (dxp dxp2) (a2) = (y[2])

  dxm2=dxm*dxm
  dxp2=dxp*dxp

  det=dxm*dxp2-dxp*dxm2

  if det != 0.0:
    a1=(fm*dxp2-fp*dxm2)/det
    a2=(fp*dxm-fm*dxp)/det
  else:
    ifail=1
    return a,yp,opt,ifail
  #endif

  if a2 != 0.0:
    dxmax=-a1/(2.0*a2)
    dymax=(a1+a2*dxmax)*dxmax
    opt[0]=x0+dxmax
    opt[1]=f0+dymax
  #endif

  # calculate f=f0+a1*dx+a2*dx**2
  # = a1*x - a1*x0 + a2*x**2 + a2*x0**2 - 2*a2*x*x0
  #  f = f0 + (a2*x0 -a1)*x0 + (a1 - 2*a2*x0 )*x+ a2*x**2

  a22=2.0*a2

  a[0]=f0 + (a2*x0 -a1)*x0
  a[1]=a1 - a22*x0
  a[2]=a2

  # calculate yp=a1+2*a2*dx

  yp[0]=a1+a22*dxm
  yp[1]=a1
  yp[2]=a1+a22*dxp

  if opt[0] <= min(xin) or opt[0] >= max(xin): ifail = 2

  return a,yp,opt,ifail
#enddef util_parabel(xin,yin)
def util_eqn(a,b):

  x = []
  istat = 0

  npar = len(b)
  nrow = len(a)

  if nrow < 1:
    istat = -1
    return istat,x
  #endif nrow < 1

  ncolumn = len(a[0])
  if nrow != ncolumn:
    istat = -2
    return istat,x
  #endif nrow < 1

  if nrow != npar:
    istat = -3
    return istat,x
  #endif nrow < 1

  x = np.linalg.solve(a,b)
  return istat,x

#enddef util_eqn(a,b)

def util_linear_fit_data(datapoints,fitfun):

  istat = 0
  param = np.array(1)

  nffun = len(fitfun[0][0])
  nfpoi = len(fitfun)
  npar = len(fitfun[0])
  nfun = len(datapoints[0])
  npoi = len(datapoints)

 # print(npar,nfun,npoi)
 # print(fitfun)
 # print(len(fitfun))

  a=np.zeros([npar,npar], dtype = 'float')
  param=np.zeros([npar], dtype = 'float')

  if len(fitfun) != npoi:
    print("*** Error in util_linear_fit_data: Number of functions differ for datapoints and fit-functions")
    istat = 1
    return istat, param
  #endif

  if nffun != nfun:
    print("*** Error in util_linear_fit_data: Number of functions differ for datapoints and fit-functions")
    istat = 1
    return istat, param
  #endif

#  print(datapoints)

  for ipar in range(npar):
    for ifun in range(nfun):
      for ipoi in range(npoi):
        param[ipar] += datapoints[ipoi][ifun] * fitfun[ipoi][ipar][ifun]
        #print(ipoi,ipar,ifun, datapoints[ipoi][ifun],fitfun[ipoi][ipar][ifun])
      #endfor ipoi in range(npoi)
    #endfor ifun in range(nfun)
  #endfor ipar in range(npar)

  for jpar in range(npar):
    for ipar in range(npar):
      for ifun in range(nfun):
        for ipoi in range(npoi):
          a[ipar][jpar] += fitfun[ipoi][ipar][ifun] * fitfun[ipoi][jpar][ifun]
        #endfor ipoi in range(npoi)
      #endfor ifun in range(nfun)
    #endfor ipar in range(npar)
  #endfor jpar in range(npar)

  param = np.linalg.solve(a,param)

  return istat,param

#endif

def util_vnorm(v):
  vn = 0
  for i in range(len(v)): vn += v[i]*v[i]
  return np.sqrt(vn)
#enddef util_vnorm

def util_rotate(cen,vrot,phi,vin,eps=1.0e-10):

      istat=0
      vlen=util_vnorm(vrot)

      if vlen == 0.0:
        vout=vin
        istat=1
        return istat, vout
      #endif

      o = vrot/vlen

      s = np.sin(phi)
      c = np.cos(phi)

      c1 = 1.0 - c

      rm11 = o[0] * o[0] * c1 + c
      rm22 = o[1] * o[1] * c1 + c
      rm33 = o[2] * o[2] * c1 + c

      rm12 = o[0] * o[1] * c1 - o[2] * s
      rm13 = o[0] * o[2] * c1 + o[1] * s

      rm21 = o[0] * o[1] * c1 + o[2] * s
      rm23 = o[1] * o[2] * c1 - o[0] * s

      rm31 = o[0] * o[2] * c1 - o[1] * s
      rm32 = o[1] * o[2] * c1 + o[0] * s

      r = np.array(vin) - np.array(cen)

      vout1 = rm11 * r[0] + rm12 * r[1] + rm13 * r[2] + cen[0]
      vout2 = rm21 * r[0] + rm22 * r[1] + rm23 * r[2] + cen[1]
      vout3 = rm31 * r[0] + rm32 * r[1] + rm33 * r[2] + cen[2]

      rm = [[rm11,rm12,rm13],[rm21,rm22,rm23],[rm31,rm32,rm33]]
      vout = [vout1,vout2,vout3]

      if eps:
        for i in range(3):
          if abs(vout[i]) < eps: vout[i] = 0.0
          for j in range(3):
            if abs(rm[i][j]) < eps: rm[i][j] = 0.0
          #endfor
        #endfor
      #endif

      return istat,vout,rm
#def util_rotate(cen,vrot,phi,vin,vout,istat)

def evnm(eg): return wtoe1/eg
def nmev(wl): return wtoe1/wl

def fsize(cfile):
  return os.stat(cfile).st_size
#endif

def fexist(f):
  import os
  if os.path.exists(f): return True
  return False
#enddef

def fwrite(F,*args):

  nargs =  len(args)

  text = ''
  for i in range(nargs-1):
    text += str(args[i]) + " "
  #endif
  text += str(args[nargs-1]) + "\n"
  F.write(text)
#enddef

def util_determinante(a):
  return np.linalg.det(a)
#enddef

def util_solve(a,x):
  return np.linalg.solve(a,x)
#enddef

def util_break(s=''):
  print('Util_break:',s)
#enddef

global WavesMode
WavesMode = None

global Wdirs, Wfiles, Wfile, Wcode, Wrun \
,Webea ,Wcurr ,Wipin ,Wcir ,Wpiny ,Wpinx ,Wpinz ,Wpinw ,Wpinh ,Wpinr \
,Wmpiz ,Wmpiy ,Wmpir ,Wmpip ,Wicbr ,Wselx ,Wsely ,Wselz ,Wphax \
,Wsigz ,Wsigy ,Wsgzp ,Wsgyp ,Wespr ,Wif2p ,Wnfrq ,Wflow ,Wfhig \
,WflowExp, WfhigExp, WnfrqExp \
,Wispe ,Wispm ,Widip ,Wnlpo ,Wbw ,Wibun ,Wnbun ,Wneib ,Wiamp \
,Wielo ,Wifol ,Wiefo ,Wirun ,Widat ,Witim ,Wvers ,Wisto, Wbeta, Wibri, Koverview \
,Wnoby ,Wnobz ,Wwal1 ,Wwal2 ,Wxabs ,Wzab1 ,Wzab2, \
Kcode, Kebeam, Kcurr \
,Wesel,Wener,Wfd,Wiesel, Vfd, IsameCanvas, TextIn, LastPlot, Lastwin \
,FiggeoEph, Ioverview,WclipE,Kpreload
global IzCut,IyCut
#global Sepp

Kpreload = True

Wfiles = []
Wdirs = []

Wiesel = -1.
Wesel = -1.
Wener = []
Wfd = []

Vfd = None

TextIn = ''
LastPlot = []
Lastwin = ''
FiggeoEph = ""
Koverview = 0
Icalloverview = 0

WclipE = 1

IsameCanvas = 0
# Histograms and Ntuples
global H1h, H1hh, H2h, H2hh, H1, H2, H1head, H2head, H1HLast, Nhead, Ntup, \
Nctup, Nh1, Nh2, Nntup, Nnctup, Hdir, Ndir, Kdir, Cdir, Fdir, \
H1Last, H2Last, NLast, H1h, H2h, N, Nct, Ind, IndLast, \
Nmin, Nmax, Nmean, Nrms, Nsum, Nxopt, Nyopt, Nlook, \
Tdf, Tfig, Tax, Tax3d, Tax2d , H1ind, H2ind, Ncind, \
H1ILast, NiLast, H1I, H2I, H2ILast, Ni, NctI, Nind, Nsel, Nlines, Ncolon, \
FitPar, FitFit, FitSig, FitChi2ndf, FitNdf, FitChi2Prob,Figman,TnpFloat64,Tnpcmpl128

Tdf = type(pd.DataFrame())

Tfig = None
Figman = None
Tax2d = None
Tax3d = None

H1h = None
H1 = []
H1head = []
Hdir = []
Ndir = 0
Kdir = 0
Cdir = ''

H1HLast = None

H2h = None
H2 = []
H2head = []

Ntup = []
Nctup = []
Ncind = []
H1ind = []
H2ind = []
Nind = []
Nhead = []

Nh1 = 0
Nh2 = 0

Nntup = 0
Nnctup = 0

H1Last = None
H2Last = None

H1h = None
H2h = None
N = None

Ind = -1
IndLast = -1

H1I = -1
H2I = -1
H1ILast = -1
H2ILast = -1

N = None
Nsel = None
NLast = None

Ni = -1
NiLast = -1

Nlines = 0
Ncolon = 0

TnpFloat64 = type(np.exp(0.0))
Tnpcmpl128 = type(np.exp(complex(0.0,0.0)))

#+PATCH,//WAVES/PYTHON
#+KEEP,plotglobal,T=PYTHON.

import matplotlib as mpl

global MPLmain, MPLmaster, Nfigs,Figgeom, Figgeom2, FiggeomR, FiggeomL, \
XtermGeo, Figs,Fig,Ax, Figgeoms, \
Screewidth, Screenheight, ScaleSizeX, ScaleSizeY, \
Fig1,Ax1,Fig6,Ax6,Fig2,Ax2,Fig7,Ax7,Fig3,Ax3,Fig8,Ax8,\
Fig4,Ax4,Fig9,Ax9,Fig5,Ax5,Fig10,Ax10,\
FirstConsole, Console, Igetconsole,Klegend, Fwidth, Fheight, Fxoff, Fyoff, \
Kfig, Kax, Ihist,Iprof, Imarker, Ierr, Isurf, Iinter, Isame, Itight, IsameGlobal, Iline, CMap, Cmap, Tcmap, Surfcolor, \
Kplots, Nwins, Zones, Kzone, Nxzone, Nyzone, Zone, Axes, Legend, CanButId, CanButIds, \
Iplotopt, Ispline, Kecho, Kdump,Kpdf, Ndump,Npdf, Icmap, \
MarkerSize, MarkerType, MarkerColor, \
Markersize, Markertype, Markercolor, \
Fillstyle, FillStyle, WaveFilePrefix,WaveDump, \
Mode3d,Mode3D, Mode2d,Mode2D, \
Textcolor,Linestyle, Linewidth, Linecolor, Author, \
Histcolor, Histedgecolor, HistEdgeColor, Histbarwidth, Kdate, Kstat, Kfit, \
Icont3d, Iboxes, Inoempty, Iclosed, Itrisurf, Iscatter, Iscat3d, Ifill1d, TitPad, Xtitle, Ytitle, \
YTitle, YGTitle,x_of_xlab,y_of_xlab,x_of_ylab,y_of_ylab, Ygtitle, Tightpad, Xtightpad,Ytightpad, ColorbarPad,\
LeftMargin,RightMargin,TopMargin,BottomMargin, Xspace, Yspace, \
Gtit,Xtit,Ytit,Ztit,Ttit,Ptit,Surfcolors, Cmaps, Colors, Linestyles, Markertypes, \
LexpX,LexpY,LexpRot,LexpPow, Xstat, XStat, XFit, YFit, Xfit, Yfit, Ystat, YStat, \
GtitFontSize,Titfontsize,Atitfontsize,Axislabelsize,Textfontsize,Datefontsize,\
Statfontsize, Axislabeldist, Axislabeldist3d, Axisdist, Axisdist3d,\
GtitFontSize,TitFontSize,AtitFontSize,AxisLabelSize,TextFontSize,DateFontSize,\
StatFontSize, AxisLabelDist, AxisLabelDist3d, AxisTitleDist, AxisTitleDist3d,\
AtitFontSize3d, Atitfontsize3d, NXtick,NXtick3d, Nxtick,Nxtick3d, Ktitles, Dummy,\
ZoomXmin,ZoomXmax, ZoomYmin, ZoomYmax, ZoomZmin,ZoomZmax,Tdate, TdateOv, Trun, TrunOv, \
LogX, LogY, LogZ, NxbBinMax, Khdeleted,WisLinux, Waveplot, \
Mrun, Mcomment, Mdate, ROFx, ROFy, Hull2D,Hull3DList,THull3D,Hull3D, Kgrid,KyAxis,KxAxis,KzAxis,Kbox, \
FillColor,Ishow


FillColor = 'none'

NxBinMax = 0
Khdeleted =0

Tdate = None
TdateOv = None
Trun = None
TrunOv = None

WaveFilePrefix = 'wave_plot_'

ZoomXmin = -1.e30
ZoomXmax = 1.e30
ZoomYmin = -1.e30
ZoomYmax = 1.e30
ZoomZmin = -1.e30
ZoomZmax = 1.e30
LogX = 0
LogY = 0
LogZ = 0

Console = 'wavesPython'
FirstConsole = 1

Figgeom = ""
Figgeom2 = ""
FiggeomR = ""
FiggeomL = ""
XtermGeo = ""
Figgeoms = []

Fwidth= 500
Fheight = 300
Fxoff = 10
Fyoff = 10

Kdate = False
Kstat = False

Ktitles = 1
Kecho = 1
Kdump = True
Kpdf = True
Ndump = 0
Npdf = 0

Author = ''

Waveplot = None
Mrun = None
Mcomment = None
Mdate = None
ROFx = 0.03
ROFy = 0.95

fcfg = ''
if WavesMode == 'WAVES' or WavesMode == 'WPLOT' or WavesMode == 'WSHOP': fcfg = 'waveplot.cfg'
elif WavesMode == 'UNDUMAG': fcfg = 'undugui.cfg'
else:
  if fexist('waveplot.cfg'):
    WavesMode = 'WAVES'
    fcfg = 'waveplot.cfg'
  else:
    fcfg = 'ntupplot.cfg'
  #endif
#endif

YFit = 0.8
Yfit = 0.8
Xfit = 0.75
XFit = 0.75

YStat = 0.8
Ystat = 0.8
Xstat = 0.05
XStat = 0.05

if fcfg != '' and os.path.exists(fcfg):

  wcon = open(fcfg,"r")

  while True:
    line = wcon.readline()
    if not len(line): break
    line = line.split('!')
    line = line[0].strip()
    words = line.split(':')
    if words[0] == 'Kstat':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kstat = True
      else: Kstat = False
    if words[0] == 'Kfit':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kstat = True
      else: KFit = False
    elif words[0] == 'Kbeam':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kbeam = True
      else: Kbeam = False
    elif words[0] == 'Kcurr':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kcurr = True
      else: Kcurr = False
    elif words[0] == 'Kcode':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kcode = True
      else: Kcode = False
    elif words[0] == 'Kecho':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kecho = True
      else: Kecho = False
    elif words[0] == 'Kpreload':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kpreload = True
      else: Kpreload = False
    elif words[0] == 'Koverview':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Ioverview = True
      else: Ioverview = False
    elif words[0] == 'Kgrid':
      if words[1].upper().strip() == 'FALSE' or words[1].upper().strip() == 'NO' or words[1].strip() == '0': Kgrid = False
      else: Kgrid = True
    elif words[0] == 'Kdate':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kdate = True
      else: Kdate = False
    elif words[0] == 'Kdump':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kdate = True
      else: Kdump = False
    elif words[0] == 'Kpdf':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Kdate = True
      else: Kpdf = False
    elif words[0] == 'xFit': XFit = float(words[1])
    elif words[0] == 'yFit': YFit = float(words[1])
    elif words[0] == 'xStat': XStat = float(words[1])
    elif words[0] == 'yStat': YStat = float(words[1])
    elif words[0] == 'ROFx': ROFx = float(words[1])
    elif words[0] == 'ROFy': ROFy = float(words[1])
    elif words[0] == 'Kconsole':
      if words[1].upper().strip() == 'TRUE' or words[1].upper().strip() == 'YES' or words[1].strip() == '1': Igetconsole = True
      else: Igetconsole = False
    elif words[0] == 'Author':
      if len(words) > 1: Author = words[1].strip()
  #endwhile True

  wcon.close()

else:
  Ioverview = 1
  Author = 'USER'
  Kfit = False
  Kstat = False
  Kecho = False
  Kbeam = True
  Kcurr = True
  Kdate = True
  Kpdf = True
  Kdump = True
  #Krun = True

#endif os.path.exists(".waveplot_config")

if Author.lower().strip() == 'user':

  try: Author = os.environ['USER']
  except:
    try: Author = os.environ['USERNAME']
    except:
      try: Author = os.getcwd()
      except: pass

  if Author != '':
    Al = list(Author)
    Al[0] = Al[0].upper()
    Author = ''
    for c in Al: Author += c
#endif Author.lower().strip() == 'user'

Kplots = list(np.linspace(1,1000,1000)*0)
Nwins = 0
Fig = None
Ax = None
Figs = []
Nfigs = 0
IsameGlobal = 0
Ifill1d = 0

TitPad = 12

GtitFontSize = 'large'
TitFontSize = 'large'
AtitFontSize = 11
AtitFontSize3d = AtitFontSize
AxisLabelSize = 10
TextFontSize = 11
DateFontSize = 9
StatFontSize = -9
AxisTitleDist = 6. # distance of title from axis
AxisTitleDist3d = 5. # distance of title from axis
Axistitledist3d = 5. # distance of title from axis
AxisLabelDist3d = 1. # distance of tick-labels from axis
AxisLabelDist = 5. # distance of tick-labels from axis

NXtick = -9
NXtick3d = 9

Xtitle = 0.5
Ytitle = 1.03
YTitle = 1.03
YGtitle = 1.03
Ygtitle = 1.03

x_of_xlab = 0.5
y_of_xlab = -0.15

x_of_ylab = 0.5
y_of_ylab = 0.5

Itight = 0
Zones = []
Nxzone = 0
Nyzone = 0
Kzone = 0
Axes = []

Icmap = 1
CMap=None
CMap='winter'
CMap='hot'
CMap='autumn'
CMap='summer'
CMap='magma'
CMap='plasma'
CMap='RdPu'
CMap = 'copper'
CMap = 'rainbow'
CMap='jet'
Cmaps = ['none','autumn','copper','jet','hot','magma','plasma','rainbow','RdPu','summer','viridis','winter']
Cmap = CMap
Tcmap = mpl.colorbar.Colorbar

Mode3D = 'surf'
Mode2D = '!'

Surfcolor='salmon'
Surfcolor = 'red'
Surfcolor = 'navajowhite'
Surfcolor = 'mediumspringgreen'

Ispline = 0

Linecolor = 'red'
Linewidth = 1.
Markersize = 4.
Markercolor = 'red'
Linestyle = 'solid'
Markertype = 'o'
Fillstyle = 'full'
FillStyle = 'full'

LineColor = 'red'
LineWidth = 1.
MarkerSize = 6.
MarkerColor = 'red'
LineStyle = 'solid'
MarkerType = 'o'

Textcolor = 'black'

Histbarwidth = 1.
Histcolor = 'yellow'
HistEdgeColor = 'blue'
Histedgecolor = 'blue'

Klegend = 0
Igetconsole = True

Imarker = 0
Ihist = 0
Iprof = 0
Ispline = 0
Isame = 0
Iscatter = 0
Iscat3d = 0
Icont3d = 0
Itrisurf = 0
Iclosed = 0
Iboxes = 0
Inoempty = 0
Ierr = 0
Isurf = 0
Iline = 0
Iinter = 0
Ifill1d = 0
Xtit = ''
Ytit = ''
Ztit = ''
Ptit = ''
Gtit = ''
Colors = ['black','red','blue','green','cyan','magenta','gray','yellow', 'white']
Surfcolors = ['none','blue','cyan','gray','green','navajowhite','magenta','mediumspringgreen','red','salmon','yellow']
Linestyles = ['none','dashed','dotted','dashdot','solid']
Markertypes = ['none','.','o','v','s','*','^','<','>','+','P','x','X']
Tightpad = 2.5
Xtightpad = 0.5
Ytightpad = 1.0
ColorbarPad = '!'
LeftMargin = 0.12
RightMargin = 0.92
TopMargin = 0.86
BottomMargin = 0.13
#ScaleSizeX = 1.4143 # for savefig in printfig to adjust aspect ratio
ScaleSizeX = 1. # for savefig in printfig to adjust aspect ratio
ScaleSizeY = 1. # for savefig in printfig to adjust aspect ratio
Xspace = 0.4
Yspace = 0.5
LexpX = 0.
LexpY = 0.05
LexpRot = 'horizontal'
LexpPow = 2
Kgrid = True
Kbox = True
KxAxis = True
KyAxis = True
KzAxis = True

CanButIds = []
CanButId = 0

Hull2D = []
Hull3D = []
Hull3DList = []
THull3D = type(Hull3D)
Ishow = 1

#+PATCH,//WAVES/PYTHON
#+KEEP,nxyzglobal,T=PYTHON.
global N1, N2, N3, N4, N5, N6, N7,N8,N9,Nv,Nx,Nxy,Nxyz
N1 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
N2 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
N3 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
N4 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
N5 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
N6 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
N7 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
N8 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
N9 = pd.DataFrame(columns=['x','y','z','s','t','bx','by','bz','b'])
Nv = pd.DataFrame(columns=['x'])
Nx = pd.DataFrame(columns=['x'])
Nxy = pd.DataFrame(columns=['x','y'])
Nxy = pd.DataFrame(columns=['x','y'])
Nxyz = pd.DataFrame(columns=['x','y','z'])

#CanButIds = []
#CanButId = 0
#Legend = []

Mode3ds = ['none','boxes','cont3d','hist','inter','surf','trisurf']
global \
Wave, Root, WaveOut, Editor, WinGeo, \
Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus
# +PATCH,//WAVES/PYTHON
# +KEEP,statusglobal,T=PYTHON.
global Istatus, WarningText, ErrorText, Gdebug, Platform, System, Uname

Istatus = 0
WarningText = ''
ErrorText = ''

Platform = platform.platform().upper()
System = platform.system().upper()

if System != 'WINDOWS':
    os.system('uname > .uname')
    fun = open('.uname','r')
    Uname = fun.readline().strip()
    System = Uname[0:5].upper()
#if System != 'WINDOWS':

Gdebug = 0

#+seq,m_hbook.
#+seq,mhb_to_pylist.

def debug(kmenu=None,kitem=None):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  pass
#  print("\n\n debug::kmenu,kitem",kmenu,kitem,SMitem[kmenu])
#  print("\n\n debug::kmenu,kitem",kmenu,kitem)
#enddef debug(kmenu,kitem)

def startgui(): print("Suggestion: Preceed to pmenu()")
def start(): print("Suggestion: Preceed to startgui()")
Debug = 0
Gdebug = 0

#begin of waves

global WAVEPATH, WAVESPATH,Sepp

pwd = os.getcwd()

global WisLinux

if System.upper() == 'LINUX' or System.upper() == 'MINGW':
  WisLinux = 1
  Sepp = '/'
else:
  Sepp = '\\'
#endif

try:
  WAVEPATH = os.environ['WAVE']
except:
  wtest = os.path.split(pwd)
  WAVEPATH = wtest[0]
  #print("\n*** Warning in waves.py: Environment variable WAVE is not defined ***")
  #print("*** Assuming ",WAVEPATH)
#endtry

if not os.path.isdir(WAVEPATH):
  print("*** Error in waves.py: Environment variable WAVE is not pointing to a directory ***")
  print("*** This variable denotes the directory, where WAVE is installed ***")
  quit()
#endif not os.path.isdir(WAVEPATH)

if os.path.isdir(WAVEPATH + "/waves"):
  WAVESPATH = WAVEPATH + "/waves"
  sys.path.append(WAVESPATH + "/python")
  WisLinux = 1
elif os.path.isdir(WAVEPATH + Sepp + "waves"):
  WAVESPATH = WAVEPATH + Sepp + "waves"
  sys.path.append(WAVESPATH + Sepp + 'python')
elif os.path.isdir(WAVEPATH + "/python"):
  sys.path.append(WAVEPATH + "/python")
  WisLinux = 1
elif os.path.isdir(WAVEPATH + Sepp + 'python'):
  sys.path.append(WAVEPATH + Sepp + 'python')
#endif os.path.isdir(WAVEPATH + "/waves")


def Getline(Fwvs):
  global Debug

  line = getline(Fwvs)

  if Debug > 0: print(line)

  line = line.upper()

  return line #-----------------------------------------------------

def getline(Fwin):
  global Debug

  ivalid = 0
  while ivalid == 0:
    line = Fwin.readline()
    if Debug > 0: print(line)
    line = line.strip()
    if len(line) > 0:
      c = list(line)
      if c[0] != 'EOF':
        ivalid = -1
      elif c[0] != '*':
        ivalid = 1
    #endif len(line) > 0

  return line #----------------------------------------------------------

def getlineI(Fwin):
  global Debug

  sl = getline(Fwin)
  iint = int(sl)

  if Debug > 0:
    print(sl)

  return iint #----------------------------------------------------

def getlineR(Fwin):
  global Debug

  sl = getline(Fwin)
  rint = float(sl)

  if Debug > 0:
    print(sl)

  return rint #-------------------------------------------------

def readwavein():

  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  fo = open('wave.in.bck','w')
  Fwin = open(FWAVEIN)

  #Start
  Nwavein = 0
  Wavein = []
  WaveinO = []

  Debug = 0

  iiwaves = 0
  iewaves = 0

  for line in Fwin:

    if Debug > 0:
      print(Nwavein,":",line)

    fo.write(line)
    WaveinO.append(line)

    #            0         1       2     3      4         5     6  7     8
    hline = ['blanks','varnam','varval','','linetype','vartype',0,'','varvalo']

    pure = line.strip()
    lline = line.rstrip()
    lenblank = len(lline) - len(pure)

    i=0
    hline[0]=''
    while i < lenblank:
      hline[0] = hline[0] + ' '
      i+= 1

    if len(pure) == 0:
      hline[0] = line
      hline[4] = 'b'
    elif pure[0] == '!':
      hline[0] = line
      hline[4] = 'c'
    else:

      words = line.split()

      hline[6] = Nwavein

      if pure[0] == '$':

        #namelist line

        if words[0].lower() == '$waves': iiwaves = 1

        hline[0] = line
        hline[4] = 'n'

        if words[0].lower() == '$end':
          hline[4] = 'e'
          if iiwaves == 1: iewaves = 1
          else: iiwaves = 0

      else:

        varlin = pure.split('!',1)

        if len(varlin) > 1: hline[7] = varlin[1]

        words = varlin[0].split('=')
        varnam = words[0].strip()
        varnam = varnam.upper()

        hline[1] = varnam

        if Debug > 1:
          print(varnam)

        varval = words[1].strip()

        if varval[0] == "'" or varval[0] == '"':
          hline[4] = 'v'
          hline[5] = 'C' #character string
          varval = varval[1:-1]
        else:
          varval = varval.lower()
          hline[4] = 'v'
          hline[5] = 'I'
          if re.search('\\' + '(',varval):
            hline[5] = 'CMPLX'
            if re.search('D',varval):
              hline[5] = 'DCMPLX'
              hline[5] = re.sub('d','e',hline[4])
          elif re.search('\\' + '.',varval):
            hline[5] = 'R'
            if re.search('D',varval):
              hline[5] = 'D'
              hline[5] = re.sub('d','e',hline[5])

        hline[2] = varval
        hline[8] = varval

    Wavein.append(hline)
    Nwavein = Nwavein+1
  # endfor

  fo.close()
  Fwin.close()

  if iewaves != 1:
    Wexit("*** Error in readwavein: Bad file wave.in, check namelist $WAVES or copy older input file (e.g. wave.in.bck) to wave.in ***")

  for i in range(Nwavein):
    if Wavein[i][1] == 'IPIN':
      if int(Wavein[i][2]) == 0:
        for j in range(i,Nwavein):
          if Wavein[j][1] == 'IPINHV' or \
          Wavein[j][1] == 'IPINWV' or \
          Wavein[j][1] == 'IPINRV':
            Wavein[j][2] = '1'
          #endif Wavein[j][1] == 'IPINHV'
        #endfor j in range(i,Nwavein)
      elif int(Wavein[i][2]) == 3:
        for j in range(i,Nwavein):
          if Wavein[j][1] == 'IPINCIRC':
            if int(Wavein[j][2]) != 0 :
              Wavein[j][2] = 0
              for k in range(j,Nwavein):
                if Wavein[k][1] == 'IPINHV': Wavein[k][2] = 1
                if Wavein[k][1] == 'IPINWV': Wavein[k][2] = 1
                if Wavein[k][1] == 'IPINRV': Wavein[k][2] = 0
              #endfor k in range(j,Nwavein)
            else:
              for k in range(j,Nwavein):
                if Wavein[k][1] == 'IPINHV': Wavein[k][2] = 0
                if Wavein[k][1] == 'IPINWV': Wavein[k][2] = 0
                if Wavein[k][1] == 'IPINRV': Wavein[k][2] = 1
              #for k in range(j,Nwavein)
          #endif Wavein[j][1] == 'IPINHV'
        #endfor j in range(i,Nwavein)
      #endif int(Wavein[i][2]) == 0
    #endif Wavein[i][1] == 'IPIN'
  #endfor i in range(Nwavein)

  print("\n--- wave.in read ---")

#---------------------------------------- end readwavein

def debug(kmenu=None,kitem=None):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  pass
#  print("\n\n debug::kmenu,kitem",kmenu,kitem,SMitem[kmenu])
#  print("\n\n debug::kmenu,kitem",kmenu,kitem)
#enddef debug(kmenu,kitem)

global ReliefBd
ReliefBd=2
global ReliefButBd
ReliefButBd=5

def get_menu_depth(kmenu):
  global Wmenu
  depth = 0
  while kmenu > 0:
    #print(Wmenu[kmenu])
    kmenu = Wmenu[kmenu][2][1]
    depth += 1
  #endwhile
  return depth
#enddef get_menu_depth(kmenu)

def pmenu_update(kmenu):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  Imenu = kmenu
  if kmenu == -1:
    pass
  else:
    # The stupidest way to refresh the menu
    try:
      PMenuGeo[Imenu] = Pmenu[Imenu].geometry()
      Pmenu[Imenu].destroy()
    except: pass
    pmenu(Imenu)
    #endtry
  #endif kmenu == -1
  pass
#def pmenu_update(kmenu)

def MenuKeyPress(event,kmenu):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  print("*** MenuKeyPress:",event,kmenu)

def SelectButton(kselect,kmenu):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH


  #print("SelectButton: ",kselect," ",kmenu)

  i = 0
  iselect = -1

  menu = Wmenu[kmenu]

  i = 5
  while i < menu[0]:

    if menu[i] == 'MAPPING' \
    or menu[i] == 'TOGGLE' \
    or menu[i] == 'BONDING' :

      iselect += 1

      if iselect == kselect:

        jitemv = i - 1

        if menu[i] == 'TOGGLE':

          ivar = menu[i+2]
          ivet = Variables[ivar][5]
          iselect = ivar

          if ivet: continue

          Variables[ivar][3] = Variables[ivar][1]

          if Variables[ivar][1] == '1': Variables[ivar][1] = '0'
          else: Variables[ivar][1] = '1'

        else:

          imap = menu[i+1]
          ivet = MappingVeto[imap]
          jitemv = i - 1

          if ivet: continue

          Mapping[imap][4] = Mapping[imap][2]

          if Mapping[imap][2]:
            Mapping[imap][2] = 0
          else:
            Mapping[imap][2] = 1
          #endif Mapping[imap][2]

        #endif menu[i] == 'VARIABLE' or menu[i] == 'TOGGLE'
        break
      #endif iselect == kselect
    #endif VARIBLE...BONDING
    i += 1
  #end while i < Wmenu[kmenu][0]

  CheckAll(kmenu,jitemv,iselect)
def MenuEnter(ev,kmenu):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  if kmenu < 0: return

  Nsitem = -1
  if kmenu >= 0 and kmenu < len(SMitem):
    Imenu = kmenu
    Nsitem = SMitem[Imenu][0]
  #endif kmenu >= 0 and kmenu < len(SMitem)

def ToggleVar(kmenu,kitem,kvar):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH


  Debug = 0

  typ = Wmenu[kmenu][kitem+1]

  if typ == 'BONDING':

    mapp = Mapping[kvar]
    istat = Mapping[kvar][2]

    if istat == 1:
      i=0
      while i < mapp[5]:
        ivar = Mapping[kvar][7+i*3]
        var = Variables[ivar]
        Variables[ivar][3] = Variables[ivar][1]
        if Debug > 1: print(i," ",ivar," ",var)
        if var[2] == 'C': Variables[ivar][1] = ''
        else: Variables[ivar][1] = '0'
        i+=1
      #endwhile i <= Mapping[kvar][5]
      Mapping[kvar][4] = Mapping[kvar][2]
      Mapping[kvar][2] = 0
    else:
      i=0
      while i < Mapping[kvar][5]:
        ivar = Mapping[kvar][7+i*3]
        var = Variables[ivar]
        Variables[ivar][3] = Variables[ivar][1]
        val = Mapping[kvar][8+i*3]
        if Debug > 1: print(i," ",ivar," ",var," ",val)
        Variables[ivar][1] = val
        i+=1
      #endwhile i <= Mapping[kvar][5]:
      Mapping[kvar][4] = Mapping[kvar][2]
      Mapping[kvar][2] = 1
    #endif istat == 1:

  elif typ == 'MAPPING':

    istat = Mapping[kvar][2]

    if istat == 1:
      i=0
      Mapping[kvar][4] = Mapping[kvar][2]
      Mapping[kvar][2] = 0
    else:
      i=0
      while i < Mapping[kvar][5]:
        ivar = Mapping[kvar][7+i*3]
        var = Variables[ivar]
        Variables[ivar][3] = Variables[ivar][1]
        val = Mapping[kvar][8+i*3]
        if Debug > 1: print(i," ",ivar," ",var," ",val)
        Variables[ivar][1] = val
        i+=1
      #endwhile i <= Mapping[kvar][5]:
      Mapping[kvar][4] = Mapping[kvar][2]
      Mapping[kvar][2] = 1

    #endif istat == 1:

  elif typ == 'TOGGLE':

    Variables[kvar][3] = Variables[kvar][1]

    if Variables[kvar][1] == '0':
      Variables[kvar][1] = '1'
    else:
      Variables[kvar][1] = '0'
    #endif Variables[kvar][1] == '0':

  else: print("*** Error in ToggleVar: Unknown keyword: ",typ)
  #endif typ == 'TOGGLE':

  if typ == 'MAPPING' or typ == 'BONDING':
    ivar = abs(Mapping[kvar][1][1])
    var = Variables[ivar]
    var[3] = var[1]
    var[1] =  str(Mapping[kvar][2])
  #endif typ == 'MAPPING' or typ == 'BONDING'

  CheckAll(kmenu,kitem,kvar)

  Debug = 0

  if 1: # Fake intendations
    Lmitem = 1
#    print("Kmitem",Kmitem," ",SMitem[kmenu][Kmitem])
#focuskvar2{
    Debug = 0


    if Lmitem == 1:
        ew = 0
    #endif Lmitem == 1:

    if Debug > 1:
      print("\n focuskvar2: \n kmenu, kvar, FIOitem: ",kmenu," ",kvar," ",FIOitem)
      print("Wmenu[kmenu]: \n",Wmenu[kmenu])
      print("ew: ",ew,"\n")
    #endif Debug > 1:

    if Lmitem == 0: CheckButtons()

#    typ = SMitem[kmenu][kitem]
#    if type(typ) == list and typ[1] == 'MENU':
#      typ[0].configure(fg='yellow')

    i = 1
    while i < len(SMitem[kmenu]):

      widg = SMitem[kmenu][i][0]
      #if widg == 0: break

      if widg.winfo_exists() == 0:
        i+= 1
        continue
      #endif widg.winfo_exists() == 0

      typ = SMitem[kmenu][i][1]

      if Debug > 0: print("FocusIn2: i, widg, typ: ",i," ",widg," ",typ)

      if typ == 'OkButton' \
      or typ == 'VARIABLE' \
      or typ == 'MENU' \
      or typ == 'HelpButton':
        i+= 1
        continue
      #endif typ == 'OkButton'

      if widg == ew:
        Kmitem = i
      #endif widg == ew:

      if typ == 'MAPPING' or typ == 'BONDING':

        imap = SMitem[kmenu][i][2]
        mapp = Mapping[imap]
        istat = mapp[2]
        iveto = MappingVeto[imap]
        fgcol = F_color
        reli = RAISED

        if iveto:
          bgcol = Deselect_color
          fgcol = Vfgcol
        else:
          if istat:
#            fgcol = Hlcol
            bgcol = Select_color
            reli = SUNKEN
          else:
            bgcol = Deselect_color
          #endif istat:
        #endif iveto:

        widg.configure(relief=reli)
      elif typ == 'TOGGLE':

        ivar = SMitem[kmenu][i][2]
        if Debug > 1: print("ivar: ",ivar)
        var = Variables[ivar]

        if Debug > 1: print(Variables[ivar])

        iveto = var[5]
        fgcol = F_color

        bgcol = Deselect_color
        reli = RAISED
        if var[1] == '1':
          bgcol = Select_color
          reli = SUNKEN
        #endif var[1] == '1':

        if iveto:
          fgcol = Vfgcol
        #endif iveto:

        if Debug > 1:
          print(kmenu," ",ivar," ",Variables[ivar])
          print(fgcol," ",bgcol," ",reli)
        #endif Debug > 1:

        widg.configure(relief=reli, \
        )

      else:
        print("*** Error in FocusIn/Out: Unknown typ: ",typ)
      #endif typ == 'MAPPING' or typ == 'BONDING':

      i+= 1
    #endwhile i <= Nmitem:
    Debug = 0
#}focuskvar2
    Lmitem = 0
  #endif 1:

  pmenu(kmenu) # to refresh menu by brute force

#endif ToggleVar(kmenu,kitem,kvar):

def DestroyMenu():
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  Pmenu[Imenu].destroy()
  SMitem[Imenu] = []
  SMexist[Imenu] = 0
  Pmenu[Imenu] = None
  Imenu = Wmenu[Imenu][2][1]

  if Imenu == -1 or Pmenu[Imenu] != None:
    pmenu_update(Imenu)

  pass
#enddef DestroyMenu()

def pmenu(kmenu):
  # Sets up the menu kmenu. The colors for all widgets are preset here,
  # however, they are overwritten in FocusIn(...), which is triggered for each
  # widget at the end of pmenu(kmenu)

  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  global \
  T_color,Eb_color,F_color,Veto_color,Passiv_color,Bg_color,B_color,\
  Select_color,Select_color,Select_color,Deselect_color, \
  Fgcol,Bgcol,Afgcol,Abgcol,Hlcol,Hlbcol,Vfgcol,Vbgcol



  Imenu = kmenu

  Calculate()
  CheckVetos()

  #if Imenu == 39: Debug=2
  if Debug > 0:
    print("Imenu: ",Imenu)
    print("Menu and veto: ",Wmenu[kmenu][1]," ",MenuVeto[kmenu][2])
    print(Wmenu[Imenu][0]," ",Wmenu[Imenu][1]," ",Wmenu[Imenu][2]," ",Wmenu[Imenu][3])
  #endif Debug > 0:

  if re.search('CONTINUATION',Wmenu[Imenu][3]):
    Mtit = Wmenu[Imenu][3] + '(continued)'
  else:
    Mtit = Wmenu[Imenu][3]
  #endif re.search('CONTINUATION',Wmenu[Imenu][3])

  iexist = 0

  if Pmenu[Imenu] != None:
    if Pmenu[Imenu].winfo_exists():
      iexist = 1
    #endif Pmenu[Imenu].winfo_exists()
  #endif Pmenu[Imenu] != None
  if iexist == 0:

    Pmenu[Imenu] = Toplevel()
    Pmenu[Imenu].attributes('-topmost', 1)

    depth = get_menu_depth(Imenu)

    if not PMenuGeo[Imenu]:
      if WavesMode == 'WPLOT' or WavesMode == 'WSHOP':
        geodum = get_geo_all()
        dw = CanW / 10 * (depth - 1)
        wp = '+' + str(int(WinX+CanW/5+dw)) + '+' + str(WinY)
      else:
        wp = WinPos
      #endif
    else:
      if WavesMode == 'WPLOT' or WavesMode == 'WSHOP':
        geodum = get_geo_all()
        dw = CanW / 10 * (depth - 1)
        wp = '+' + str(int(WinX+CanW/5+dw)) + '+' + str(WinY)
      else:
        wp = PMenuGeo[Imenu]
      #endif
    #endif not PMenuGeo[Imenu]:

    Pmenu[Imenu].geometry(wp)

    Pmenu[Imenu].configure()
    Pmenu[Imenu].title(Mtit)
  else:
    if Ifocus == -1: Pmenu[Imenu].focus_set()
    return
  #endif iexist == 0

  item = 5
  iselect = -1
  Nsitem = -1
  SFrame = []

  if Debug > 1: print("\n SMexist: \n",SMexist,"\n")

  if SMexist[Imenu] == 0:
    SMitem[Imenu].append(Nsitem)
    SComment[Imenu].append(Nsitem)
  else:
    SMitem[Imenu][0] = -1
    SComment[Imenu][0] = -1
  #endif SMexist[Imenu] == 0

  nitem = Wmenu[Imenu][0]

  while item <= Wmenu[Imenu][0]:

    if Debug > 1:
      print(item," ",Wmenu[Imenu][item])

    if Wmenu[Imenu][item] == 'TEXT':


      item += 1
      itemt = item + 1
      itemv = item + 2
      itemn = item + 3

      if SMexist[Imenu] == 0:
        SMitem[Imenu].append([0,'',-1,0])
        SComment[Imenu].append(0)
      #endif SMexist[Imenu] == 0

      Nsitem+= 1
      SMitem[Imenu][0]+= 1

      if Wmenu[Imenu][4] == 'MENU':

        Iarr = -1

        if Wmenu[Imenu][itemt] == 'ARRAY':

          Iarr = Wmenu[Imenu][itemv]

          Check_Array(Iarr)

          arr = Arrays[Iarr]

          arrnam = arr[0]
          arrtit = arr[1]
          ivar = arr[2]
          nvars = arr[3]
          ndim = arr[4]
          varis = arr[5]

          actrl = Variables[ivar]

          veto = actrl[5]
          if veto != 0:
            vcolor = Veto_color
            vstate = 'disable'
          else:
            vcolor = T_color
            vstate = 'normal'
          #endif Variables[ivar][5] != 0

          SFrame.append(None)
          SFrame[Nsitem] = Frame(Pmenu[Imenu], relief=RIDGE, bd=ReliefBd,)
          slab=StringVar()
          slab.set(Wmenu[Imenu][item])

          if item < nitem - 3:
            if Wmenu[Imenu][item+3] == 'COMMENT':
              slab.set(Wmenu[Imenu][item] + '\n' + Wmenu[Imenu][item+4])
          #endif item < nitem - 3
          slabel = Label(SFrame[Nsitem],text=slab.get(),
                         font = MyWavesFont)
          SComment[Imenu][Nsitem]=slabel

          SMitem[Imenu][Nsitem+1][1] = 'VARIABLE'
          SMitem[Imenu][Nsitem+1][2] = ivar

          stext=StringVar()
          stext.set(actrl[1])

          lens = max(10,len(stext.get()))
          SMitem[Imenu][Nsitem+1][0] = \
          Entry(SFrame[Nsitem],bd=ReliefBd,justify=CENTER, \
          font = MyWavesFont, \
          text=stext,width=lens)

          slabel.pack(side=LEFT)
          SMitem[Imenu][Nsitem+1][0].pack(side=RIGHT)

          SFrame[Nsitem].pack(fill='x', padx=PadX, pady=PadY)

          ivarray = int(arr[-1])
          varray = Variables[ivarray]
          idx = int(varray[1])

          if SMexist[Imenu] == 0:
            SMitem[Imenu].append([0,'',-1,0])
            SComment[Imenu].append(0)
          #endif SMexist[Imenu] == 0

          Nsitem+= 1
          SMitem[Imenu][0]+= 1

          veto = varray[5]

          if veto != 0:
            vcolor = Veto_color
            vstate = 'disable'
          else:
            vcolor = T_color
            vstate = 'normal'
          #endif Variables[ivar][5] != 0

          SFrame.append(None)
          SFrame[Nsitem] = Frame(Pmenu[Imenu], relief=RIDGE, bd=ReliefBd)
          slab=StringVar()
          slab.set("Index of current item")

          slabel = Label(SFrame[Nsitem],text=slab.get(),
                         font = MyWavesFont)
          SComment[Imenu][Nsitem]=slabel

          SMitem[Imenu][Nsitem+1][1] = 'VARIABLE'
          SMitem[Imenu][Nsitem+1][2] = ivarray

          stext=StringVar()
          stext.set(varray[1])

          lens = max(10,len(stext.get()))
          SMitem[Imenu][Nsitem+1][0] = \
          Entry(SFrame[Nsitem],bd=ReliefBd,justify=CENTER, \
          font = MyWavesFont, \
          text=stext,width=lens)

          slabel.pack(side=LEFT)
          SMitem[Imenu][Nsitem+1][0].pack(side=RIGHT)

          SFrame[Nsitem].pack(fill='x', padx=PadX, pady=PadY)

          # Field for current item

          ifound = nvars

          for i in range(nvars):

            v = arr[5][i]
            vartit = v[0]
            varnambase = v[1]
            varnam = varnambase + '(' + str(idx) + ')'

            if idx >  len(v[2]):
              ivar = -1
            else:
              ivar = v[2][idx-1][6]
            #endif idx >  len(v[2])

            if ivar <0:
              Quit("PMENU: Variable nicht gefunden; dürfte nicht passieren!")
              ivar = GetVarNum(varnam)
              if ivar < 0:
                ivar = create_variable(varnam,'R')
                vv = Variables[ivar]
                v[2].append([idx,vv[1],vv[2],vv[3],vv[4],vv[5],ivar])
              #endif ivar < 0:
            #endif ivar <0

            var = Variables[ivar]
            vara = v[2]
            kdx = vara[idx-1][0]

            if idx != kdx: continue

            if SMexist[Imenu] == 0:
              SMitem[Imenu].append([0,'',-1,0])
              SComment[Imenu].append(0)
            #endif SMexist[Imenu] == 0
            Nsitem+= 1
            SMitem[Imenu][0]+= 1

            veto = var[5]
            if veto != 0:
              vcolor = Veto_color
              vstate = 'disable'
            else:
              vcolor = T_color
              vstate = 'normal'
            #endif Variables[ivar][5] != 0

            SFrame.append(None)
            SFrame[Nsitem] = Frame(Pmenu[Imenu],relief=RIDGE,bd=ReliefBd)
            slab=StringVar()
            #slab.set(Wmenu[Imenu][item])
            slab.set(varnam)

            slabel = Label(SFrame[Nsitem],text=slab.get(),
                           font = MyWavesFont)
            SComment[Imenu][Nsitem]=slabel

            SMitem[Imenu][Nsitem+1][1] = 'VARIABLE'
            SMitem[Imenu][Nsitem+1][2] = ivar

            stext=StringVar()
            sval = var[1]
            stext.set(sval)

            lens = max(10,len(stext.get()))
            SMitem[Imenu][Nsitem+1][0] = \
            Entry(SFrame[Nsitem],bd=ReliefBd,justify=CENTER, \
            font = MyWavesFont, \
            text=stext,width=lens)

            slabel.pack(side=LEFT)
            SMitem[Imenu][Nsitem+1][0].pack(side=RIGHT)

            SFrame[Nsitem].pack(fill='x', padx=PadX, pady=PadY)

          #endfor i in range(nvars)

          pass

        elif Wmenu[Imenu][itemt] == 'VARIABLE':

          ivar = Wmenu[Imenu][itemn]

          if Variables[ivar][5] != 0:
            vcolor = Veto_color
            vstate = 'disable'
          else:
            vcolor = T_color
            vstate = 'normal'
          #endif Variables[ivar][5] != 0

          SFrame.append(None)
          SFrame[Nsitem] = Frame(Pmenu[Imenu], relief=RIDGE, bd=ReliefBd)
          slab=StringVar()
          slab.set(Wmenu[Imenu][item])

          if item < nitem - 4:
            if Wmenu[Imenu][item+4] == 'COMMENT':
              slab.set(Wmenu[Imenu][item] + '\n' + Wmenu[Imenu][item+5])
          #endif item < nitem - 4

          slabel = Label(SFrame[Nsitem],text=slab.get(),
                         font = MyWavesFont)
          SComment[Imenu][Nsitem]=slabel

          SMitem[Imenu][Nsitem+1][1] = 'VARIABLE'
          SMitem[Imenu][Nsitem+1][2] = ivar

          stext=StringVar()
          Wmenu[Imenu][itemv] = Variables[ivar][1]
          stext.set(Wmenu[Imenu][itemv])

          if Variables[ivar][2] == 'C':
            lens = max(15,len(stext.get()))
            SMitem[Imenu][Nsitem+1][0] = \
            Entry(SFrame[Nsitem],bd=ReliefBd,justify=CENTER, \
            font = MyWavesFont, \
            text=stext,width=lens)
          else:
            lens = max(10,len(stext.get()))
            SMitem[Imenu][Nsitem+1][0] = \
            Entry(SFrame[Nsitem],bd=ReliefBd,justify=CENTER, \
            font = MyWavesFont, \
            text=stext,width=lens)
          #endif Variables[ivar][2] == 'C'

          SMitem[Imenu][Nsitem+1][0].bind("<KeyPress>", \
          lambda event, kmenu=Imenu, kitem=itemv, kvar=ivar: \
          FocusIn(event,kmenu,kitem,kvar))

          slabel.pack(side=LEFT)
          SMitem[Imenu][Nsitem+1][0].pack(side=RIGHT)

          SFrame[Nsitem].pack(fill='x', padx=PadX, pady=PadY)

        elif Wmenu[Imenu][itemt] == 'TOGGLE' or \
        Wmenu[Imenu][itemt] == 'MAPPING' or Wmenu[Imenu][itemt] == 'BONDING':

          SMitem[Imenu][Nsitem+1][1] = Wmenu[Imenu][itemt]

          vcolor = T_color
          vstate = 'normal'

          if Wmenu[Imenu][itemt] == 'TOGGLE':

            ivar = Wmenu[Imenu][itemn]

            SMitem[Imenu][Nsitem+1][2] = ivar

            sbut = StringVar()
            sbut.set(Wmenu[Imenu][item])

            if item < nitem - 4:
              if Wmenu[Imenu][item+4] == 'COMMENT':
                sbut.set(Wmenu[Imenu][item] + '\n' + Wmenu[Imenu][item+5])
            #endif item < nitem - 4:

            SMitem[Imenu][Nsitem+1][0] = Button(Pmenu[Imenu],text=sbut.get(),\
            font = MyWavesFont, \
            command=\
            lambda kmenu=Imenu,kitem=item,kvar=ivar: ToggleVar(kmenu,kitem,kvar))
            if Variables[ivar][1] == '0':
              SMitem[Imenu][Nsitem+1][0].config(relief=RAISED,\
              bd=ReliefButBd)
            else:
              SMitem[Imenu][Nsitem+1][0].config(relief=SUNKEN,\
              bd=ReliefButBd)
            #endif Variables[ivar][1] == '0':
            SFrame.append(SMitem[Imenu][Nsitem+1][0])
            SFrame[Nsitem].pack(fill='x', padx=PadX, pady=PadY)

            SMitem[Imenu][Nsitem+1][0].bind("<Return>", \
            lambda event, kmenu=Imenu,kitem=item,kvar=ivar: ToggleVar(kmenu,kitem,kvar))

          elif Wmenu[Imenu][itemt] == 'MAPPING' or Wmenu[Imenu][itemt] == 'BONDING':

            imap = Wmenu[Imenu][itemv]

            fgcol = Fgcol
            bgcol = Select_color
            afgcol = Afgcol
            abgcol = Abgcol
            hlcol = Hlcol
            hlbcol = Hlbcol
            vfgcol = Vfgcol
            vbgcol = Deselect_color
            reli = RAISED

            if MappingVeto[imap] != 0: #veto
              fgcol = vfgcol
              bgcol = vbgcol
              afgcol = vfgcol
              abgcol = vbgcol
              vstate = 'disable'
            elif Mapping[imap][2] != 0: # Mapping/Bonding status is active
              bgcol = Select_color
              vstate = 'normal'
              reli = SUNKEN
            else:
              bgcol = Deselect_color
              vstate = 'normal'
            #if MappingVeto[imap] != 0:

            SMitem[Imenu][Nsitem+1][2] = imap

            ivar = imap

            sbut = StringVar()
            sbut.set(Wmenu[Imenu][item])

            if item < nitem - 4:
              if Wmenu[Imenu][item+4] == 'COMMENT':
                sbut.set(Wmenu[Imenu][item] + '\n' + Wmenu[Imenu][item+5])
            #endif item < nitem - 4:

            #print('MAPPING, BONDING: ',Pmenu[Imenu])
            SMitem[Imenu][Nsitem+1][0] = Button(Pmenu[Imenu],text=sbut.get(),\
            font = MyWavesFont, \
            relief=reli,bd=ReliefButBd, \
            command=\
            lambda kmenu=Imenu,kitem=item,kvar=ivar: ToggleVar(kmenu,kitem,kvar))
            SMitem[Imenu][Nsitem+1][0].bind("<Return>", \
            lambda event, kmenu=Imenu,kitem=item,kvar=ivar: ToggleVar(kmenu,kitem,kvar))

            SFrame.append(SMitem[Imenu][Nsitem+1][0])
            SFrame[Nsitem].pack(fill='x', padx=PadX, pady=PadY)

          #endif Wmenu[Imenu][itemt] == 'TOGGLE':

        #endif Wmenu[Imenu][itemt] == 'VARIABLE' etc.:

      #endif Wmenu[Imenu][4] == 'MENU'

#error      elif Wmenu[Imenu][itemt] == 'SELECT':
      elif Wmenu[Imenu][4] == 'SELECT':


        iselect+= 1

        SMitem[Imenu][Nsitem+1][1] = Wmenu[Imenu][itemt]

        if Wmenu[Imenu][itemt] == 'VARIABLE':

          ivar = Wmenu[Imenu][itemn]

          if Variables[ivar][5] != 0:
            vcolor = Veto_color
            vstate = 'disable'
          else:
            vcolor = T_color
            vstate = 'normal'

          SMitem[Imenu][Nsitem+1][2] = ivar

        elif Wmenu[Imenu][itemt] == 'TOGGLE':

          ivar = Wmenu[Imenu][itemn]

          if Variables[ivar][5] != 0:
            vcolor = Veto_color
            vstate = 'disable'
          else:
            vcolor = T_color
            vstate = 'normal'
          #endif

          SMitem[Imenu][Nsitem+1][2] = ivar

        elif Wmenu[Imenu][itemt] == 'MAPPING' or Wmenu[Imenu][itemt] == 'BONDING':

          imap = Wmenu[Imenu][itemv]
          #print(Wmenu[Imenu])
          #print("Veto:",MappingVeto[imap])
          #print("Act:",Mapping[imap][2])

          #print("fg",F_color,Veto_color,Passiv_color)
          vcolor = F_color

          if MappingVeto[imap] != 0:
            vcolor = Veto_color
            vstate = 'disable'
            reli = FLAT
          elif Mapping[imap][2] != 0:
            reli = SUNKEN
            vcolor = F_color
            vstate = 'normal'
          else:
            reli = RAISED
            vcolor = F_color
            vstate = 'normal'
          #endif

          SMitem[Imenu][Nsitem+1][2] = imap

        #endif Wmenu[Imenu][itemt] == 'TOGGLE':

        sbut = StringVar()
        sbut.set(Wmenu[Imenu][item])

        if item < nitem - 4:
          if Wmenu[Imenu][item+4] == 'COMMENT':
            sbut.set(Wmenu[Imenu][item] + '\n' + Wmenu[Imenu][item+5])
        #endif item < nitem - 4

        #print(Imenu,sbut.get())
        #print(Pmenu[Imenu])
        SMitem[Imenu][Nsitem+1][0] = Button(Pmenu[Imenu],text=sbut.get(),\
        font = MyWavesFont, fg=vcolor, relief=reli, \
        bd=ReliefButBd, \
        command=\
        lambda kselect=iselect, kmenu = Imenu: SelectButton(kselect,kmenu))
        SMitem[Imenu][Nsitem+1][0].bind("<Return>", \
        lambda event, kselect=iselect, kmenu = Imenu: SelectButton(kselect,kmenu))

#        if Wmenu[Imenu][itemv]  != 0:
#          color = Select_color
#          relief = 'SUNKEN'
#        else:
#          color = Deselect_color
#          relief = 'RAISED'
#        #endif
#+self,if=oldcolors.
#        if Wmenu[Imenu][itemv]  != '0':
#          SMitem[Imenu][Nsitem+1][0].config(relief=SUNKEN, bg=Select_color)
#        else:
#          SMitem[Imenu][Nsitem+1][0].config(relief=RAISED, bg=Deselect_color)
#        #endif Wmenu[Imenu][itemv]  != 0:
#+self,if=-oldcolors.
#        if Wmenu[Imenu][itemv]  != '0':
#          SMitem[Imenu][Nsitem+1][0].config(relief=SUNKEN)
#        else:
#          SMitem[Imenu][Nsitem+1][0].config(relief=RAISED)
#        #endif Wmenu[Imenu][itemv]  != 0:
#+self.

        SFrame.append(SMitem[Imenu][Nsitem+1][0])
        SFrame[Nsitem].pack(fill='x', padx=PadX, pady=PadY)

      # endelif Wmenu[Imenu][4] == 'SELECT':

    #endif Wmenu[Imenu][item] == 'TEXT':

    elif Wmenu[Imenu][item] == 'COMMENT':

      pass # already done
    #endif Wmenu[Imenu][item] == 'COMMENT':

    elif Wmenu[Imenu][item] == 'MENU' or Wmenu[Imenu][item] == 'SELECT':


      if SMexist[Imenu] == 0:
        SMitem[Imenu].append([0,'',-1,0])
        SComment[Imenu].append(0)
      #endif

      SMitem[Imenu][0]+= 1
      Nsitem+= 1

      item+= 1

      idaughter = Wmenu[Imenu][item]

      fgcol = Fgcol
      bgcol = Bgcol
      afgcol = Afgcol
      abgcol = Abgcol
      hlcol = Hlcol
      hlbcol = Hlbcol
      vfgcol = Vfgcol
      vbgcol = Deselect_color

      for ia in range(Nmenuactive):
        mena = MenuActive[ia]
        if mena[0] == Wmenu[idaughter][1]:
          SMitem[Imenu][Nsitem][3] = 0
          for k in range(len(mena[1])):
            var = mena[1][k].strip()
            var = Variables[GetVarNum(var)]
            if var[1]:
              SMitem[Imenu][Nsitem][3] = 1
              break
            #endif var[1]
            break
        #endif mena[0] == Wmenu[idaughter][1]:
      #endfor ia in len(Nmenuactive)

      if MenuVeto[idaughter][2] != 0:
        afgcol = vfgcol
        abgcol = vbgcol
        fgcol = vfgcol
        bgcol = vbgcol
        hlcol = vfgcol
        hlbcol = vbgcol
      #endif MenuVeto[idaughter][2] != 0:

      SMitem[Imenu][Nsitem+1][1] = 'MENU'
      SMitem[Imenu][Nsitem+1][2] = MenuVeto[idaughter][2]

      #if idaughter == 4: print("pmenu:",Imenu,Nsitem, MenuVeto[idaughter][2])
      SMitem[Imenu][Nsitem+1][0] = \
      Button(Pmenu[Imenu],text=Wmenu[idaughter][3], \
      fg=fgcol, \
      font = MyWavesFont, \
      command=\
      lambda kmenu=idaughter: pmenu(kmenu))
      SMitem[Imenu][Nsitem+1][0].bind("<Return>", \
      lambda event, kmenu=idaughter: pmenu(kmenu))

      SFrame.append(SMitem[Imenu][Nsitem+1][0])
      SFrame[Nsitem].pack(fill='x', padx=PadX, pady=PadY)

    #endif Wmenu[Imenu][item] == 'MENU/SELECT':

    item+= 1

  #endwhile item

  nsframe = Nsitem

  BottomFrame = Frame(Pmenu[Imenu], bd=ReliefBd)
  SFrame.append([BottomFrame,'BottomFrame',-1])

  ihbutton = None
  if Help[Imenu][0] > 0:

    HButton = Button(BottomFrame,text='HELP', name='bHelpButton',command=\
    lambda kmenu=Imenu: HelpText(kmenu),
      font = MyWavesFont)
    HButton.bind("<Return>", lambda event, kmenu=Imenu: HelpText(kmenu))
    HButton.pack(side=LEFT,fill='x', padx=PadX, pady=PadY)
    SMitem[Imenu][0]+= 1
    Nsitem+= 1
    ihbutton = Nsitem

    if SMexist[Imenu] == 0:
      SMitem[Imenu].append([HButton,'HelpButton',-1])
      SComment[Imenu].append('Help')
    else:
      SMitem[Imenu][Nsitem+1][0]=HButton
    #endif SMexist[Imenu] == 0

  #endif Help[Imenu)[0] > 0:
  OKButton = Button(BottomFrame,text='OK', name='bOkButton',\
  font = MyWavesFont, \
  command=DestroyMenu)

  OKButton.bind("<Return>", lambda event: DestroyMenu())
  OKButton.pack(fill='x', padx=PadX, pady=PadY)

  SMitem[Imenu][0]+= 1
  Nsitem+= 1
  iokbutton = Nsitem

  if SMexist[Imenu] == 0:
    SMitem[Imenu].append([OKButton,'OkButton',-1])
    SComment[Imenu].append('OK')
  else:
    SMitem[Imenu][Nsitem+1][0]=OKButton
  #endif SMexist[Imenu] == 0

  nsframe+= 1
  SFrame[nsframe][0].pack(fill='x', padx=PadX, pady=PadY)

  item=0

  while item <= Nsitem :

    lvar = -2

    if item == ihbutton:
      lvar = -11
    elif item == iokbutton:
      lvar = -12
    #endif item == ihbutton

    butt = SMitem[Imenu][item+1][0]
    words = str(butt).split('!')

    SMitem[Imenu][item+1][0].bind('<Up>',lambda event,
                                  kmenu=Imenu, kitem=item,:
                                  Up(event,kmenu,kitem))
    SMitem[Imenu][item+1][0].bind('<Down>',lambda event,
                                  kmenu=Imenu, kitem=item,:
                                  Down(event,kmenu,kitem))
    SMitem[Imenu][item+1][0].bind('<KeyPress>',lambda event,
                                  kmenu=Imenu, kitem=item, kvar=lvar:
                                  FocusIn(event,kmenu,kitem,kvar))
    SMitem[Imenu][item+1][0].bind('<FocusIn>',lambda event,
                                  kmenu=Imenu, kitem=item, kvar=lvar:
                                  FocusIn(event,kmenu,kitem,kvar))
    SMitem[Imenu][item+1][0].bind('<FocusOut>',lambda event,
                                  kmenu=Imenu, kitem=item, kvar=lvar:
                                  FocusOut(event,kmenu,kitem,kvar))
    SMitem[Imenu][item+1][0].bind('<Enter>',lambda event,
                                  kmenu=Imenu, kitem=item, kvar=lvar:
                                  FocusIn(event,kmenu,kitem,kvar))
    SMitem[Imenu][item+1][0].bind('<Leave>',lambda event,
                                  kmenu=Imenu, kitem=item, kvar=lvar:
                                  FocusOut(event,kmenu,kitem,kvar))

    # to trigger FocusIn in order to setup colors
    if Ifocus == -1: SMitem[Imenu][item+1][0].focus_set()

    item = item + 1

  #end while item <= Nmitem :

  if SMexist[Imenu] == 0:
    SMexist[Imenu] = 1
  #endif SMexist[Imenu] == 0

  #CheckButtons()

#---------------------------------------- end pmenu

def setup_input(event):

  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  Debug = 0
  if kWaveinRead == 0:
    readwavein()
    readwvs()
    Ipmenu = 0
    Imenu = 0
    kWaveinRead = 1

  pmenu(0)

#---------------------------------------- end setup_wavein()
#!/usr/bin/env python

# +PATCH,//WAVES/PYTHON
# +KEEP,readwvs,T=PYTHON.

def Check_Array(iarr):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  arr = Arrays[iarr]
  nactrl = arr[2]
  vctrl = Variables[nactrl]

  aname = arr[0]
  atit = arr[1]
  nvars = arr[3]
  nmax = arr[4] # array size
  varis = arr[5]

  if len(varis) != nvars:
    print("\n *** Error in Check_Array: Incorrect number of variables for array",NL,arr)
    Quit("*** waves.py aborted in Check_Array ***\n")
  #endif len(varis) != nvars

  ivarray = arr[-1]
  varray = Variables[ivarray]

  if vctrl[1] != vctrl[3]:
    varray[3] = varray[1]
    varray[1] = vctrl[1]
  #endif vctrl[1] != vctrl[3]:

  idx = int(varray[1])

  if idx < 0:

    if int(vctrl[1]) > 1:

      # Delete item

      idx = -idx

      for ivar in range(nvars):

        var = arr[5][ivar]
        varitems = var[2]
        nitems = len(varitems)

        if idx > nitems: break

        waritems = []

        for i in range(nitems):
          vitem = varitems[i]
          kdx = vitem[0]
          if idx != kdx:
            if kdx > idx: vitem[0] = kdx - 1
            waritems.append(vitem)
          #endif
        #endfor i in range(nitems)

        arr[5][ivar][2] = waritems

      #endfor ivar in range(nvars)

      varray[1] = str(nitems - 1)

      vctrl[3] = vctrl[1]
      vctrl[1] = varray[1]

    #endif int(vctrl[1]) > 1

    else:

      varray[1] = varray[3]
      return

  #endif idx < 0

  # order items

  for ivar in range(nvars):

    var = arr[5][ivar]
    varitems = var[2]
    nitems = len(varitems)

    for k in range(nitems):
      kdx = k + 1
      ifound = -1
      for i in range(nitems):
        vitem = varitems[i]
        idx = vitem[0]
        if idx == kdx:
          ifound = idx
          uitem = arr[5][ivar][2][k]
          arr[5][ivar][2][k] = vitem
          arr[5][ivar][2][i] = uitem
          break
        #endif idx == i + 1:
      #endfor i in range(nitems)
      if ifound == -1:
        print("\n*** Error in Check_Arrays: Could not sort indices for array\n\n",aname)
        Quit("*** Aborting waves.py ***")
      #endif ifound > -1
    #endfor i in range(nitems)

  #endfor ivar in range(nvars)

  #reindex

  for ivar in range(nvars):
    var = arr[5][ivar]
    varitems = var[2]
    nitems = len(varitems)
    for i in range(nitems):
      varitems[i][0] = i + 1
    #endfor i in range(nitems)
  #endfor ivar in range(nvars)

  # create missing

  for ivar in range(nvars):

    var = arr[5][ivar]
    vartit = var[0]
    varbasename = var[1]
    varitems = var[2]
    nitems = len(varitems)

    vtype = None
    for i in range(nitems):
      vitem = varitems[i]
      if vtype == None and vitem[2] != None:
        vtype = vitem[2]
      #endif vtype == None and vitem[2] != None
    #endfor i in range(nitems)

    for i in range(nitems):

      k = i + 1
      vitem = varitems[i]
      idx = vitem[0]
      varname = varbasename + '(' + str(k) + ')'

      kvar = GetVarNum(varname)
      if kvar < 0:
        kvar = create_variable(varname,vtype)
        Variables[kvar][3] = Variables[kvar][1]
      #endif kvar < 0

      var = Variables[kvar]

      vitem[1] = var[1]
      vitem[2] = var[2]
      vitem[3] = var[3]
      vitem[4] = var[4]
      vitem[5] = var[5]
      vitem[6] = kvar

    #endfor i in range(nitems)

  #endfor ivar in range(nvars)

#enddef Check_Array(iarr)

def test():
  print("Test() called")
#enddef test()

def AddMenu(nam,moth,tit,items):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  menu = []

  name = nam
  title = tit
  nitem = len(items)
  key = 'MENU'

  mother = [moth,-1]

  menu.append(nitem)
  menu.append(name)
  menu.append(mother)
  menu.append(title)
  menu.append(key)

  iknown = -1
  i = 0
  while i<=Nmenu:
    if name == Wmenu[i][1]:
      iknown = 1
      break
    i+= 1
  #endwhile i<=Nmenu

  if name == mother[0]:
    isame = 1
  else:
    isame = 0
  #endif name == mother[0]

  if not isame and iknown == -1:

    menu[1] = name
    menu[4] = key

    #print(menu)
    Wmenu.append(menu)
    MenuVeto.append([-1,'0',0,0,'class'])
    MenuAllVeto.append([0])

    Nmenu = NmenuOld
    Nmenu+= 1
    NmenuOld = Nmenu

    MenuMother = mother

    if Nmenu > 0: getmother(iknown,isame)

    menu[2][1] = IMother

    Help.append([])
    Help[Nmenu].append(0)

    pmenu = None
    Pmenu.append(pmenu)
    PMenuGeo.append(WinPos)
    SMitem.append([])
    SComment.append([])
    SMexist.append(0)

  #if not isame and iknown == -1

  if isame:
    Nmenuold = Nmenu
    Nmenu = IMother
  #endif isame

  if title == '$TITLE':
    title = getline(Fwvs)
    c = list(title)
    Wmenu[Nmenu][3] = title
  else:
    print('*** Menu title missing of Menu: ',name)
    Wexit('')
  #endif title == '$TITLE'

  for it in items:
    Wmenu[Nmenu].append(it)
    Wmenu[Nmenu][0]+= 1
  #endfor it in Items

#enddef AddMenu()

def setspecvar():
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  for m in range(Nmap+1):
    M = Mapping[m]
    ivar = abs(M[1][1])
    var = Variables[ivar]
    Specvar[ivar][0] = m
    Specvar[ivar][1] = M[3]
    Specvar[ivar][2] = M[2]
  #endfor m in range(Nmap+1)
#enddef setspecvar()

def create_variable(varnam,vartype):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  ivar = -1

  i=0
  while i < Nvar:
    if varnam == Variables[i][0]:
      return i
    i+= 1
  #endwhile i < Nvar:

# concept of variables: [
# 0:'name', 1:value, 2:'type', 3:previous value,4: initial value, 5:veto,
# 6: array to control, 7: line number, i.e. index in Wavein])

  specvar = [-1,'oper',-1,'class']

  Variables.append(['name',-1,'type', -1,-1,0, -1,-1])
  Nvar += 1
  if varnam == 'NSTEPMX': Quit("Bau NSTEPMX")

  Variables[Nvar][0]=varnam

  if vartype == 'R':
    Variables[Nvar][1]=0.0
  elif vartype == 'I':
    Variables[Nvar][1]=0
  else:
    Variables[Nvar][1]=''
  #endif vartype == 'R'

  Variables[Nvar][2]=vartype
  Variables[Nvar][3]=Variables[Nvar][1]
  Variables[Nvar][4]=None
  Variables[Nvar][5]=0 # Veto
  Variables[Nvar][6]=-1 # Array
  Variables[Nvar][7]=-1

  Specvar.append(specvar)

  return Nvar

#enddef create_variable(varnam,vartype)

def GetVarNum(var):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  ivar = -1

  i=0
  while i < Nvar:
    if var == Variables[i][0]:
      return i
    i+= 1
  #endwhile i < Nvar:

  i=0
  specvar = [-1,'oper',-1,'class']
  while i < Nwavein:
    if Wavein[i][1] == var and Wavein[i][4] == 'v':
      Variables.append(['name',-1,'type', -1,-1,0, -1,-1])
      Nvar += 1
      Variables[Nvar][0]=var
      Variables[Nvar][1]=Wavein[i][2] # value
      Variables[Nvar][2]=Wavein[i][5]
      Variables[Nvar][3]=Wavein[i][2] # previous value
      Variables[Nvar][4]=Wavein[i][2]
      Variables[Nvar][5]=0 # Veto
      Variables[Nvar][6]=-1 # Array
      Variables[Nvar][7]=i # line number
      Specvar.append(specvar)
      return Nvar
    i+= 1
  #endwhile i < Nwavein:

  return ivar

def getmother(iknown,isame):

  global Menu, Nmenu, MenuMother, IMother, Debug

  mother = MenuMother

  if iknown == -1: mmenu = Nmenu
  else: mmenu = iknown

  IMother = -1
  ifound = 0
  i = 0
  while i<=Nmenu and ifound == 0:
    daughter = Wmenu[i][1]
    if daughter == mother[0]:
      ifound = 1
      IMother = i
    i+= 1

  if ifound == 0:
    print('\n *** Error in getmother(): Bad mother menu')
    print('\n Menu: ',Wmenu[mmenu][1],'\n')
    Wexit('')

  if not isame:

    Wmenu[IMother][0]+= 1

    if Wmenu[IMother][4] == 'MENU':
      Wmenu[IMother].append('MENU')
    elif Wmenu[IMother][4] == 'SELECT':
      Wmenu[IMother].append('SELECT')

    Wmenu[IMother].append(mmenu)
    Wmenu[IMother][0]+= 1

#end of getmother --------------------------------------------------

def readwvs():

  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH


  Debug = 0

  try:
    Fwvs = open(FWVS,'r')
  except:
    Quit("\n\n*** Error: Configuration file",FWVS,"not found  ***")
  #endtry

  nline = 0
  for line in Fwvs:


    if line == '$EOF': break

    nline += 1
#    debug(nline,line)

    line = line.strip()
    c = list(line)

    if Debug>0: print(nline,":",line)

    if len(line) > 0 and c[0] != '*':

      if nline == 1:
        WAVECom = line
        if WavesMode == 'WSHOP':
          print("\n\n--- Command to run WAVE read from '",FWVS,"' is '",WAVECom,"'")
          print("\nIn case of trouble check command, it must not contain commands\nto start GUIs like waveplot.py!\n\n")
          sleep(3)
      #endif
      elif nline == 2: ROOTCom = line
      elif nline == 3: EWOUTCom = line
      elif nline == 4: EDICom = line
      elif nline == 5: WinDef = line

      specvar = [-1,'oper',-1,'class']
      variable = ['name',-1,'type', -1,-1,0 ,-1,-1]

      words = line.split()

      if words[0].upper() == '$VETO': #BreakVeto

        Nveto+= 1
        Veto.append([-1,'name',1,'type',1])

        name = Getline(Fwvs)
        val = getlineI(Fwvs)
        Veto[Nveto][1] = val
        goal = Getline(Fwvs) # veto object

        ifound = -1

        i = 0
        while i <= Nmap and ifound == -1:

          if Debug > 1: print(i," ",Mapping[i][1])

          if name == Mapping[i][1]:
            ifound = i
            Veto[Nveto][0] = ifound
            if Mapping[i][3] == 'MAPPING': Veto[Nveto][3] = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': Veto[Nveto][3] = 'BONDING'
          #endif name == Mapping[i][1]

          i+= 1
        #endwhile i <= Nmap and ifound == -1

        i = 0

        while i <= Nvar and ifound == -1:

          if Debug > 1: print(i," ",Variables[i][0])

          if name == Variables[i][0]:
            ifound = i
            Veto[Nveto][0] = ifound
            Veto[Nveto][3] = 'VARIABLE'
          #endif name == Variables[i][0]

          i+= 1

        #endwhile i <= Nvar and ifound == -1

        i = 0
        while i < Nwavein and ifound == -1:

          if Wavein[i][1] == name and Wavein[i][4] == 'v':

            ifound = 1
            ii = 0
            iifound = -1

            while ii<=Nvar and iifound == -1:

              if name == Variables[ii][0]:
                ifound = ii
                iifound = ii
                Veto[Nveto][0] = ifound
                Veto[Nveto][3] = 'VARIABLE'
              #endif name == Variables[ii][0]
              ii+= 1
            #endwhile ii<=Nvar and iifound == -1

            if iifound == -1:

              Nvar += 1

              Specvar.append(specvar)
              Specvar[Nvar][0] = -1

              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=name
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar

              Veto[Nveto][0] = ifound
              Veto[Nveto][3] = 'VARIABLE'
            #endif iifound == -1

          #if Wavein[i][1] == name and Wavein[i][4] == 'v'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad veto variable for ',name,' ***')
          Wexit('')
        #endif ifound == -1

        ifound = -1

        i = 0
        while i <= Nmap and ifound == -1:
          if Debug > 1: print(i," ",Mapping[i][1])
          if goal == Mapping[i][1]:
            ifound = i
            Veto[Nveto][2] = ifound
            if Mapping[i][3] == 'MAPPING': Veto[Nveto][4] = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': Veto[Nveto][4] = 'BONDING'
          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if goal == Variables[i][0]:
            ifound = i
            Veto[Nveto][2] = ifound
            Veto[Nveto][4] = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:
          if Wavein[i][1] == goal and Wavein[i][4] == 'v':
            ifound = 1
            ii = 0
            iifound = -1
            while ii<=Nvar and iifound == -1:
              if goal == Variables[ii][0]:
                ifound = ii
                iifound = ii
                Veto[Nveto][2] = ifound
                Veto[Nveto][4] = 'VARIABLE'
              ii+= 1

            if iifound == -1:

              Nvar += 1
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])
              Specvar.append(specvar)

              Variables[Nvar][0]=goal
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar

              Veto[Nveto][2] = ifound
              Veto[Nveto][4] = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad vetoed variable ',name,' ***')
          Wexit('')

      # endif VETO

      elif words[0].upper() == '$INVERSVETO':

        invveto = [-1,'name',1,'type',1] #BreakInversVeto

        Ninvveto+= 1
        InvVeto.append(invveto)

        name = Getline(Fwvs)
        val = getlineI(Fwvs)
        InvVeto[Ninvveto][1] = val
        goal = Getline(Fwvs) # veto object

        ifound = -1

        i = 0
        while i <= Nmap and ifound == -1:

          if Debug > 1: print(i," ",Mapping[i][1])

          if name == Mapping[i][1]:
            ifound = i
            InvVeto[Ninvveto][0] = ifound
            if Mapping[i][3] == 'MAPPING': InvVeto[Nveto][3] = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': InvVeto[Nveto][3] = 'BONDING'

          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if name == Variables[i][0]:
            ifound = i
            InvVeto[Ninvveto][0] = ifound
            InvVeto[Ninvveto][3] = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:

          if Wavein[i][1] == name and Wavein[i][4] == 'v':

            ifound = 1
            iifound = -1

            ii = 0
            while ii<=Nvar and iifound == -1:
              if name == Variables[ii][0]:
                ifound = ii
                iifound = ii
                InvVeto[Ninvveto][0] = ifound
                InvVeto[Ninvveto][3] = 'VARIABLE'

              ii+= 1

              if iifound == -1:

                Nvar += 1
                Specvar.append(specvar)
                Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

                Variables[Nvar][0]=name
                Variables[Nvar][1]=Wavein[i][2]
                Variables[Nvar][2]=Wavein[i][5]
                Variables[Nvar][3]=Wavein[i][2]
                Variables[Nvar][4]=Wavein[i][2]
                Variables[Nvar][7]=i # line number

                iifound = Nvar
                ifound = Nvar

                InvVeto[Ninvveto][0] = ifound
                InvVeto[Ninvveto][3] = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad inverse veto variable for ',goal,' ***')
          Wexit('')
        #endif ifound == -1

        ifound = -1

        i = 0
        while i <= Nmap and ifound == -1:
          if Debug > 1: print(i," ",Mapping[i][1])
          if goal == Mapping[i][1]:
            ifound = i
            InvVeto[Ninvveto][2] = ifound
            if Mapping[i][3] == 'MAPPING': InvVeto[Nveto][4] = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': InvVeto[Nveto][4] = 'BONDING'
          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if goal == Variables[i][0]:
            ifound = i
            InvVeto[Ninvveto][2] = ifound
            InvVeto[Ninvveto][4] = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:

          if Wavein[i][1] == goal and Wavein[i][4] == 'v':

            ifound = 1
            iifound = -1

            ii = 0
            while ii<=Nvar and iifound == -1:
              if goal == Variables[ii][0]:
                ifound = ii
                iifound = ii
                InvVeto[Ninvveto][2] = ifound
                InvVeto[Ninvveto][4] = 'VARIABLE'
              ii+= 1

            if iifound == -1:

              Nvar += 1

              Specvar.append(specvar)
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=goal
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar

              InvVeto[Ninvveto][2] = ifound
              InvVeto[Ninvveto][4] = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad inverse vetoed variable ',goal,' ***')
          Wexit('')
        #endif ifound == -1

        InvVeto.append(invveto)

      # endif INVERSEVETO

      elif words[0].upper() == '$MENU' or words[0].upper() == '$SELECT':

#        if Nmenu == 0 or Nmenu == 36: debug(Nmenu,Nmenu)
        menu = [] #BreakSelect

        nitem = 4
        key = 'MENU'

        if words[0].upper() == '$SELECT': key = 'SELECT'

        name = ''
        mother = ['',-1]
        title = ''

        menu.append(nitem)
        menu.append(name)
        menu.append(mother)
        menu.append(title)
        menu.append(key)

        name = Getline(Fwvs)
        mother[0] = Getline(Fwvs)

        iknown = -1
        i = 0
        while i<=Nmenu:
          if name == Wmenu[i][1]:
            iknown = 1
            break
          i+= 1
        #endwhile i<=Nmenu

        if name == mother[0]:
          isame = 1
        else:
          isame = 0
        #endif name == mother[0]

        if not isame and iknown == -1:

          menu[1] = name
          menu[4] = key
          Wmenu.append(menu)
          MenuVeto.append([-1,'0',0,0,'class'])
          MenuAllVeto.append([0])

          Nmenu = NmenuOld
          Nmenu+= 1
          NmenuOld = Nmenu

          MenuMother = mother
          if Nmenu > 0: getmother(iknown,isame)

          menu[2][1] = IMother

          Help.append([])
          Help[Nmenu].append(0)

          pmenu = None
          Pmenu.append(pmenu)
          PMenuGeo.append(WinPos)
          SMitem.append([])
          SComment.append([])
          SMexist.append(0)

        #endif not isame and iknown == -1

        if isame:
          Nmenuold = Nmenu
          Nmenu = IMother
        #endif isame

        title = Getline(Fwvs)

        if title == '$TITLE':
          title = getline(Fwvs)
          c = list(title)
          Wmenu[Nmenu][3] = title
        else:
          print('*** Menu title missing of Menu: ',name)
          Wexit('')
        #endif title == '$TITLE'

      #endif MENU

      elif words[0].upper() == '$MENUACTIVE':

        Nmenuactive += 1
        act = Getline(Fwvs)
        trig = Getline(Fwvs).split(',')
        MenuActive.append([act,trig])

      elif words[0].upper() == '$MENUVETO' or words[0].upper() == '$INVERSEMENUVETO':

        Nmenuveto+= 1 #BreakMenuVeto

        if words[0].upper() == '$INVERSEMENUVETO':
          MenuVeto[Nveto][Nmenu][3] = 1

        name = Getline(Fwvs)
        vetoval = Getline(Fwvs)
        goal = Getline(Fwvs)
        if Debug > 0: print(Nmenuveto," ",name," ",goal)
        #Break MENUVETO
        ifound = -1
        i = 0
        while i <= Nmap and ifound == -1:
          if Debug > 1:
            print(i," ",Mapping[i][1])
          if name == Mapping[i][1]:
            ifound = i
            vetovar = ifound
            if Mapping[i][3] == 'MAPPING': vetoclass = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': vetoclass = 'BONDING'
          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if Debug > 1: print(i," ",Variables[i][0])
          if name == Variables[i][0]:
            ifound = i
            vetovar = ifound
            vetoclass = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:
          if Wavein[i][1] == name and Wavein[i][4] == 'v':

            ifound = 1
            iifound = -1

            ii = 0
            while ii<=Nvar and iifound == -1:
              if name == Variables[ii][0]:
                ifound = ii
                iifound = ii
                vetovar = ifound
                vetoclass = 'VARIABLE'
              ii+= 1

            if iifound == -1:

              Nvar += 1

              Specvar.append(specvar)
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=name
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar
              vetovar = ifound
              vetoclass = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad ',words[0], 'variable ',name,' ***')
          Wexit('')

        ifound = -1
        i = 0
        while i <= Nmenu and ifound == -1:
          if goal == Wmenu[i][1]:

            MenuVeto[i][0] = vetovar
            MenuVeto[i][1] = vetoval
            MenuVeto[i][2] = 0
            MenuVeto[i][3] = 0
            MenuVeto[i][4] = vetoclass

            ifound = i
#            print(Wmenu[i])
#            print(Variables[vetovar])
#            print(MenuVeto[i])
#            Wexit('')

          i+= 1

        if ifound == -1:
          print('*** Bad vetoed menu ',name,' ***')
          Wexit('')

      # endif MENUVETO

      elif words[0].upper() == '$MENUALLVETO':

        name = Getline(Fwvs) #BreakMenuAllVeto
        vetoval = Getline(Fwvs)
        goal = Getline(Fwvs)

        inverse = 0
        # TO CONFUSING... if words[0].upper() == '$INVERSEMENUALLVETO': inverse = 1

        ifound = -1
        i = 0
        while i <= Nmap and ifound == -1:
          if Debug > 1: print(i," ",Mapping[i][1])
          if name == Mapping[i][1]:
            ifound = i
            vetovar = ifound
            if Mapping[i][3] == 'MAPPING': vetoclass = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': vetoclass = 'BONDING'
          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if name == Variables[i][0]:
            ifound = i
            vetovar = ifound
            vetoclass = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:
          if Wavein[i][1] == name and Wavein[i][4] == 'v':
            ifound = 1
            ii = 0
            iifound = -1
            while ii<=Nvar and iifound == -1:
              if name == Variables[ii][0]:
                ifound = ii
                iifound = ii
                vetovar = ifound
                vetoclass = 'VARIABLE'
              ii+= 1

            if iifound == -1:
              Nvar += 1

              Specvar.append(specvar)
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=name
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar
              vetovar = ifound
              vetoclass = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad ',words[0],' for ',name,' ***')
          Wexit('')

        ifound = -1
        i = 0
        while i <= Nmenu and ifound == -1:
          if goal == Wmenu[i][1]:
            MenuAllVeto[i][0] += 1
            MenuAllVeto[i].append([vetovar,vetoval,0,inverse,vetoclass])
            ifound = i
          i+= 1

        if ifound == -1:
          print('*** Bad ',words[0],' for ',name,' ***')
          Wexit('')

      # endif MENUALLVETO

      elif words[0].upper() == '$VARIABLE' or words[0].upper() == '$ARRAY':

        #debug(nline,0)

        if words[0].upper() == '$ARRAY':
          Quit("ARRAY 2 !!")
        #endif words[0].upper() == '$ARRAY'

        Wmenu[Nmenu].append('TEXT') #BreakVariable
        Wmenu[Nmenu][0]+= 1

        Wmenu[Nmenu].append(text)
        Wmenu[Nmenu][0]+= 1

        if key == '$VARIABLE':
          Wmenu[Nmenu].append('VARIABLE')
        elif key == '$ARRAY':
          Wmenu[Nmenu].append('ARRAY')
        else:
          print("*** Bad key", key, " for $TEXT ",text," ***")
          Wexit('')
        #endif key == '$VARIABLE'

        if key == '$ARRAY':
          Quit("ARRAY 1 !!")
        #endif words[0].upper() == '$ARRAY'

        Wmenu[Nmenu][0]+= 1

        vname = Getline(Fwvs)
        ifound = 0
        iifound = 0

        i = 0
        while i < Nwavein and ifound == 0:

          if Wavein[i][1] == vname and Wavein[i][4] == 'v':

            Wmenu[Nmenu].append(Wavein[i][2])
            Wmenu[Nmenu][0]+= 1
            ifound = 1

            ii = 0
            iifound = 0
            while ii <= Nvar and iifound == 0:
              if vname == Variables[ii][0]:
                iifound = 1
                Wmenu[Nmenu].append(ii)
                Wmenu[Nmenu][0]+= 1
                Lastvar = ii
              #endif vname == Variables[ii][0]:

              ii+= 1
            #endwhile ii <= Nvar and iifound == 0:

            if iifound == 0:

              Nvar += 1

              Specvar.append(specvar)
# concept of variables: [
# 0:'name', 1:value, 2:'type', 3:previous value,4: initial value, 5:veto,
# 6: array to control, 7: line number, i.e. index in Wavein])
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=vname
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              Wmenu[Nmenu].append(Nvar)
              Wmenu[Nmenu][0]+= 1
              Lastvar = Nvar

              iifound = Nvar
            #endif iifound == 0:
          #endif Wavein[i][1] == vname and Wavein[i][4] == 'v':

          i+= 1
        #end while i < Nwavein and ifound == 0:

        if words[0].upper() == '$ARRAY':
          Quit("ARRAY!!")
        #endif words[0].upper() == '$ARRAY'

      elif words[0].upper() == '$TEXT':

        Wmenu[Nmenu].append('TEXT') #BreakText
        Wmenu[Nmenu][0]+= 1

        text = getline(Fwvs)
        Wmenu[Nmenu].append(text)
        Wmenu[Nmenu][0]+= 1

        key = Getline(Fwvs)

        if key == '$VARIABLE':
          Wmenu[Nmenu].append('VARIABLE')
        elif key == '$ARRAY':
          Wmenu[Nmenu].append('ARRAY')
        elif key == '$TOGGLE':
          Wmenu[Nmenu].append('TOGGLE')
        elif key == '$MAPPING':
          Wmenu[Nmenu].append('MAPPING')
        elif key == '$BONDING':
          Wmenu[Nmenu].append('BONDING')
        else:
          print("*** Bad key", key, " for $TEXT ",text," ***")
          Wexit('')
        #endif key == '$VARIABLE'

        Wmenu[Nmenu][0]+= 1

        if key == '$ARRAY':
#          debug(key,1)
#          Ical += 1

          arr = []
          arrnam = getline(Fwvs).upper()

          arrtit = getline(Fwvs)
          actrl = getline(Fwvs).upper()
          nactrl = GetVarNum(actrl)
          vctrl = Variables[nactrl]

          chdims = getline(Fwvs)
          narrvars = int(chdims.split()[0])
          narrdim = int(chdims.split()[1])

          arr.append(arrnam) #0
          arr.append(arrtit) #1
          arr.append(nactrl) #2
          arr.append(narrvars) #3
          arr.append(narrdim) #4
          arr.append('list') #5

          arrvars = []
          for i in range(narrvars):
            atit = getline(Fwvs)
            avar = getline(Fwvs).upper()
            arrvars.append([atit,avar]) # 5
          #endfor i in range(narrvars)

          arr[5] = arrvars

          # Is the array already known?{

          ifound = -1

          if Narr > -1:
            for i in range(Narr+1):
              if arrnam == Arrays[i][0]:
                ifound = i
                break
              #endif arrnam == Arrays[i][0]
            #endfor i in range(Narr+1)
          #endif Narr > -1

          # }Is the array already known?

          if ifound > -1:

            arr = Arrays[Narr]

          else:

            for ivar in range(narrvars):

              arrvar = arr[5][ivar]
              vnamebase = arrvar[1]

              i = 0
              ifound = 0
              afound = []

              while i < Nwavein:

                win = Wavein[i]
                win1 = win[1].split('(')

                if win1[0] == vnamebase and win[4] == 'v':

                  i1 = win[1].find('(') + 1
                  i2 = win[1].find(')')
                  ind = int(win[1][i1:i2])

                  ifound += 1

                  if ifound == 1: ind1 = ind

                  #print(ivar,i,ind)

                  # Wavein[i][2]) # value
                  # Wavein[i][5]) # type
                  # Wavein[i][2]) # prev. value
                  # Wavein[i][2]) # initial value
                  # veto flag

                  iveto = 0
                  kvar = -1

                  aval = [ind,win[2],win[5],win[2],win[2],iveto,kvar]
                  if len(afound) > narrdim:
                    print("\n*** Error in readwvs(): Dimension of array",arrnam," exceeded***")
                    Quit("*** wave.py aborted \n")
                  #endif len(afound) > narrdim
                  afound.append(aval)

                #endif Wavein[i][1] == vname and Wavein[i][4] == 'v'

                i+=1

              #endwhile i < Nwavein and ifound == 0

              nvar = int(vctrl[1])
              ifound = 0
              for idx in range(1,nvar+1):
                vname = vnamebase + '(' + str(idx) + ')'
                kvar = GetVarNum(vname)
                if kvar < 0:
                  #print("*** Error in readwvs: Variable", vname, " not found in wave.in ***")
                  aval = [idx,None,None,None,None,None,kvar]
                  afound.append(aval)
                  if len(afound) > narrdim:
                    print("\n*** Error in readwvs(): Dimension of array",arrnam," exceeded***")
                    Quit("*** wave.py aborted \n")
                  #endif len(afound) > narrdim
                else:
                  ifound = 1
                  afound[idx-1][6] = kvar
                #endif kvar < 0
              #endfor idx in range(1:nvar+1)

              if ifound == 0:
                print("*** Error in readwvs: Array variable", vnamebase, " not found in wave.in ***")
              #endif ifound == 0

              arrvar.append(afound)

            #endfor ivar in range(narrvars)

            Narr+= 1
            Nvar += 1

# concept of variables: [
# 0:'name', 1:value, 2:'type', 3:previous value,4: initial value, 5:veto,
# 6: array to control, 7: line number, i.e. index in Wavein])

            Variables.append(['name',-1,'type',  -1,0,0,   Narr,-1])

            Variables[Nvar][0]="Varray"+str(Narr)
            Variables[Nvar][1]=str(ind1)
            Variables[Nvar][2]='I'
            Variables[Nvar][3]=str(ind1)
            Variables[Nvar][4]=str(ind1)

            specvar = [-1,'oper',-1,'class']
            Specvar.append(specvar)
            Specvar[Nvar][0] = -1

            arr.append(Nvar)

            # Check ordering of indices


            Arrays.append(arr)

          #endif ifound > -1

          Wmenu[Nmenu].append(Narr)
          Wmenu[Nmenu][0]+= 1

#          print(Narr)
#          print(Arrays)
#          Quit("Baustelle")

        elif key != '$MAPPING' and key != '$BONDING':

          vname = Getline(Fwvs)

          ifound = 0
          iifound = 0

          i = 0
          while i < Nwavein and ifound == 0:

            if Wavein[i][1] == vname and Wavein[i][4] == 'v':

              #debug("VAR",1)
              Wmenu[Nmenu].append(Wavein[i][2])
              Wmenu[Nmenu][0]+= 1

              ifound = 1
              ii = 0
              iifound = 0

              while ii <= Nvar and iifound == 0:

                if vname == Variables[ii][0]:
                  iifound = 1
                  Wmenu[Nmenu].append(ii)
                  Wmenu[Nmenu][0]+= 1
                  Lastvar = ii
                #endif vname == Variables[ii][0]
                ii+= 1

              #endwhile ii <= Nvar and iifound == 0

              if iifound == 0:

                Nvar += 1

                Specvar.append(specvar)
                Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

                Variables[Nvar][0]=vname
                Variables[Nvar][1]=Wavein[i][2]
                Variables[Nvar][2]=Wavein[i][5]
                Variables[Nvar][3]=Wavein[i][2]
                Variables[Nvar][4]=Wavein[i][2]
                Variables[Nvar][7]=i # line number

                Wmenu[Nmenu].append(Nvar)
                Wmenu[Nmenu][0]+= 1
                Lastvar = Nvar

                iifound = Nvar

              #endif iifound == 0

            i+= 1
          #end while i < Nwavein and ifound == 0:

          if ifound == 0:
            print("*** Error in readwvs(): Variable",vname," not found in wave.in ***")
            Quit("*** waves.py aborted ***")
          #endif ifound == 0

        elif key == '$MAPPING' or key == '$BONDING':

          Nmap+= 1

          Mapping.append([6,['name',-1],1,'type',1,-1])
          MappingVeto.append(0)

          if key == '$MAPPING':
            Mapping[Nmap][3]='MAPPING'
          else:
            Mapping[Nmap][3]='BONDING'

          name = Getline(Fwvs) # mapping name
          #Check, if Mapping is also a normal variable{
          isvar = GetVarNum(name)
          #}Check, if Mapping is also a normal variable

          if isvar < 0:
            Nvar += 1 # Mapping is also a variable
            ivar = -Nvar
            Variables.append(['name',-1,'type', -1,-1,0, -1,-1])
            Specvar.append(specvar)

            smap2 = str(Mapping[Nmap][2])
            Variables[Nvar][0] = name
            Variables[Nvar][1] = smap2
            Variables[Nvar][2] = 'I'
            Variables[Nvar][3] = smap2
            Variables[Nvar][4] = smap2
          #endif isvar < 0:

          Mapping[Nmap][1]=[name,ivar]
          ment = getlineI(Fwvs)

          ient = 1
          Mapping[Nmap][5] = ment

          iset = 1

          while ient <= ment:

            var = Getline(Fwvs)

            Mapping[Nmap][0]+= 3

            ivar = GetVarNum(var)

            #print(nline,var,ivar)
            #for ip in range(10):
            #  print(ip,Variables[ip])
            #quit()

            # Brute force correction, since menu structure has changed in 2020
            if ivar == -1:
              if var != 'KBRECM':
                print("*** Error in readwvs: Variable ", var, " not found for mapping ",name)
                Wexit('')
              else:
                var = Variables[Nvar-1]
                var = ['KBRECM','0','I',  '0','0',0, 0,-1]
                Variables.append(var)
                Nvar += 1
                ivar = Nvar - 1
                var = 'KBRECM'
                specvar = [-1,'oper',-1,'class']
                Specvar.append(specvar)
            #endif ivar == -1

            Mapping[Nmap].append(var)
            Mapping[Nmap].append(ivar)
            val = Getline(Fwvs)
            Mapping[Nmap].append(val)

            if iset == 1:

              cval = val
              cvar = Variables[ivar][1]

              if Variables[ivar][2] == 'C':
                cval = complex(val)
                cvar = complex(Variables[ivar][1])
              #endif Variables[ivar][3] != 'C':

              if cvar != cval:
                iset = 0
                Mapping[Nmap][2] = 0
                Mapping[Nmap][4] = 0 #previous value
                kvar = Mapping[Nmap][1][1]
                if kvar > 0:
                  Variables[kvar][1] = '0'
                else:
                  Variables[-kvar][1] = '0'
                #endif kvar > 0:
              #endif cvar != cval:
            #endif iset == 1:

            ient+= 1
          # endwhile ient <= ment:

          Wmenu[Nmenu].append(Nmap)
          Wmenu[Nmenu][0]+= 1

      #endif TEXT

      elif words[0].upper() == '$COMMENT':

        Wmenu[Nmenu].append('COMMENT')
        Wmenu[Nmenu][0]+= 1
        comment = getline(Fwvs)

        Wmenu[Nmenu].append(comment)
        Wmenu[Nmenu][0]+= 1

      elif words[0].upper() == '$CALC':

        Ncalc+= 1 #BreakCalc
        Calc.append([-1,'CONST','oper',-1,'CONST',0,'CONST'])

        op1 = Getline(Fwvs)
        op1.strip()
        oper = Getline(Fwvs)
        oper.strip()
        op2 = Getline(Fwvs)
        op2.strip()
        res = Getline(Fwvs)
        res.strip()

        ifound = -1
        i = 0
        while i <= Nmap and ifound == -1:
          if Debug > 1: print(i," ",Mapping[i][1])
          if op1 == Mapping[i][1]:
            ifound = i
            Calc[Ncalc][0] = ifound
            if Mapping[i][3] == 'MAPPING': Calc[Ncalc][1] = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': Calc[Ncalc][1] = 'BONDING'
          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if Debug > 1: print(i," ",Variables[i][0])
          if op1 == Variables[i][0]:
            ifound = i
            Calc[Ncalc][0] = ifound
            Calc[Ncalc][1] = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:
          if Wavein[i][1] == op1 and Wavein[i][4] == 'v':
            ifound = 1
            ii = 0
            iifound = -1
            while ii<=Nvar and iifound == -1:
              if op1 == Variables[ii][0]:
                ifound = ii
                iifound = ii
                Calc[Ncalc][0] = ifound
                Calc[Ncalc][1] = 'VARIABLE'
              ii+= 1

            if iifound == -1:

              Nvar += 1
              Specvar.append(specvar)
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=op1
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar

              Calc[Ncalc][0] = ifound
              Calc[Ncalc][1] = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad first operand ',op1,' for CALC ***')
          Wexit('')

        Calc[Ncalc][2] = oper

        ifound = -1

        if op2 == '1':
          Calc[Ncalc][3] = 1
          ifound = -2

        if op2 == '-1':
          Calc[Ncalc][3] = -1
          ifound = -2

        i = 0
        while i <= Nmap and ifound == -1:
          if Debug > 1: print(i," ",Mapping[i][1])
          if op2 == Mapping[i][1]:
            ifound = i
            Calc[Ncalc][3] = ifound
            if Mapping[i][3] == 'MAPPING': Calc[Ncalc][4] = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': Calc[Ncalc][4] = 'BONDING'
          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if Debug > 1: print(i," ",Variables[i][0])
          if op2 == Variables[i][0]:
            ifound = i
            Calc[Ncalc][3] = ifound
            Calc[Ncalc][4] = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:
          if Wavein[i][1] == op2 and Wavein[i][4] == 'v':
            ifound = 1
            ii = 0
            iifound = -1
            while ii<=Nvar and iifound == -1:
              if op2 == Variables[ii][0]:
                ifound = ii
                iifound = ii
                Calc[Ncalc][3] = ifound
                Calc[Ncalc][4] = 'VARIABLE'
              ii+= 1

            if iifound == -1:

              Nvar += 1
              Specvar.append(specvar)
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=op2
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar

              Calc[Ncalc][3] = ifound
              Calc[Ncalc][4] = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad second operand ',op2,' for CALC ***')
          Wexit('')

        ifound = -1
        i = 0
        while i <= Nmap and ifound == -1:
          if Debug > 1: print(i," ",Mapping[i][1])
          if res == Mapping[i][1]:
            ifound = i
            Calc[Ncalc][5] = ifound
            if Mapping[i][3] == 'MAPPING': Calc[Ncalc][6] = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': Calc[Ncalc][6] = 'BONDING'
          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if Debug > 1: print(i," ",Variables[i][0])
          if res == Variables[i][0]:
            ifound = i
            Calc[Ncalc][5] = ifound
            Calc[Ncalc][6] = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:
          if Wavein[i][1] == res and Wavein[i][4] == 'v':
            ifound = 1
            ii = 0
            iifound = -1
            while ii<=Nvar and iifound == -1:
              if res == Variables[ii][0]:
                ifound = ii
                iifound = ii
                Calc[Ncalc][5] = ifound
                Calc[Ncalc][6] = 'VARIABLE'
              ii+= 1

            if iifound == -1:

              Nvar += 1
              Specvar.append(specvar)
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=res
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar

              Calc[Ncalc][5] = ifound
              Calc[Ncalc][6] = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad second operand ',res,' for CALC ***')
          Wexit('')

      #endif CALC

      elif words[0].upper() == '$TRIGGER':

        #test()
        Ntrigger+= 1 #BreakTrigger
        trigger = ['trig','class','goal']
        Trigger.append(trigger)

        trig = Getline(Fwvs)
        trig.upper()

        goal = Getline(Fwvs)
        goal.upper()

        #Break TRIGGER

        ifound = -1
        i = 0
        while i <= Nmap and ifound == -1:
          if Debug > 1: print(i," ",Mapping[i][1][0])
          if trig == Mapping[i][1][0]:
            ifound = i
            Trigger[Ntrigger][0] = ifound
            if Mapping[i][3] == 'MAPPING': Trigger[Ntrigger][1] = 'MAPPING'
            elif Mapping[i][3] == 'BONDING': Trigger[Ntrigger][1] = 'BONDING'
          i+= 1

        i = 0
        while i <= Nvar and ifound == -1:
          if Debug > 1: print(i," ",Variables[i][0])
          if trig == Variables[i][0]:
            ifound = i
            Trigger[Ntrigger][0] = ifound
            Trigger[Ntrigger][1] = 'VARIABLE'
          i+= 1

        i = 0
        while i < Nwavein and ifound == -1:
          if Wavein[i][1] == trig and Wavein[i][4] == 'v':
            ifound = 1
            ii = 0
            iifound = -1
            while ii<=Nvar and iifound == -1:
              if trig == Variables[ii][0]:
                ifound = ii
                iifound = ii
                Trigger[Ntrigger][0] = ifound
                Trigger[Ntrigger][1] = 'VARIABLE'
              ii+= 1

            if iifound == -1:

              Nvar += 1
              Specvar.append(specvar)
              Variables.append(['name',-1,'type', -1,-1,0, -1,-1])

              Variables[Nvar][0]=trig
              Variables[Nvar][1]=Wavein[i][2]
              Variables[Nvar][2]=Wavein[i][5]
              Variables[Nvar][3]=Wavein[i][2]
              Variables[Nvar][4]=Wavein[i][2]
              Variables[Nvar][7]=i # line number

              iifound = Nvar
              ifound = Nvar

              Trigger[Ntrigger][0] = ifound
              Trigger[Ntrigger][1] = 'VARIABLE'

          i+= 1
        #end while i < Nwavein and ifound == -1:

        if ifound == -1:
          print('*** Bad trigger variable for ',trig,' ***')
          Wexit('')

        ifound = -1
        i = 0
        while i <= Nmenu and ifound == -1:
          if goal == Wmenu[i][1]:
            ifound = i
            Trigger[Ntrigger][2] = ifound
          i+= 1

        if ifound == -1:
          print('*** Bad target ',goal,' of trigger ',trig,' ***')
          Wexit('')

      # endif TRIGGER

      elif words[0].upper() == '$HELP':

        key = '' #BreakHelp

        while key!='$ENDHELP':

          line = Fwvs.readline()

          if len(line) > 1:
            words = line.split()
            key = words[0].upper()

          Help[Nmenu][0]+= 1
          Help[Nmenu].append(None)

          Help[Nmenu][Help[Nmenu][0]] = line

        #endwhile key!='$ENDHELP':


      # endif HELP

  Fwvs.close()

  Fwvs = open('waves_variables.lis','w')

  for iv in range(len(Variables)):
    var = Variables[iv]
    svar = str(iv) + ": "
    for i in range(len(var)-1):
      svar += str(var[i]) + ", "
    #endfor
    svar += str(var[i+1])
    Fwvs.write(svar + "\n")
  #endfor

  Fwvs.close()

  Fwvs = open('waves_calculations.lis','w')

  for iv in range(len(Calc)):
    var = Calc[iv]
    svar = str(iv) + ": "
    for i in range(len(var)-1):
      svar += str(var[i]) + ", "
    svar += str(var[i+1])
    Fwvs.write(svar + "\n")

  Fwvs.close()

  Fwvs = open('waves_menus.lis','w')

  for i in range(Nmenu):
    Fwvs.write(str(i) + "\n")
    Fwvs.write(str(Wmenu[i]) + "\n")
  #endfor i in range(len(Nmenu))

  Fwvs.close()

  setspecvar()

  Fmap = open("waves_mappings.lis","w")
  i = 0
  for m in Mapping:
    Fmap.write(str(i) + ": " + str(m) + "\n")
    i += 1
  #endfor
  Fmap.close()

  print("--- waves.wvs read ---")
  #exit("waves.wvs gelesen!")

#  for m in range(Nmenu):
#    print(m)
#    print(Wmenu[m][1])
#  #endfor
#end of readwvs --------------------------------------------------
#enddef readwvs()
def checkskip():
  global Wavein

  print('*** checkskip ***')
  i = 0
  for w in Wavein:
    try:
      s = w[0].split()[0]
      if s[:4] == 'SKIP':
        print(i,Wavein[i])
    except: pass
    i += 1
  #endfor
def wiwrite(fo,s):

  global iemptyline

  try:
    if iemptyline == 1 and s.strip() == '':
      return
    #endif
  except:
    pass
  #endtry

  fo.write(s)

  if s.strip() == '':
    iemptyline = 1
  else:
    iemptyline = 0
  #endif

#enddef

def Icheck_Array_Var(ivar):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  karr = -1
  for iarr in range(len(Arrays)):
    arr = Arrays[iarr]
    if arr[2] == ivar:
      karr = iarr
      break
  #endfor in range(len(Arrays))
  return karr
#enddef Icheck_Array_Var(ivar)

def debug(kmenu=None,kitem=None):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  pass
#  print("\n\n debug::kmenu,kitem",kmenu,kitem,SMitem[kmenu])
#  print("\n\n debug::kmenu,kitem",kmenu,kitem)
#enddef debug(kmenu,kitem)
def writewavein():

  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  NL = "\n"
  Debug = 0


  if kWaveinRead != 0:

    fo = open('wave.in','w')

#{update variables in Wavein
    ivar=0
    while ivar < Nvar:
      var = Variables[ivar][0]
      ifound = -1
      i = 0
      while i < Nwavein:
        if Wavein[i][1] == var and Wavein[i][4] == 'v':
          ifound = i
          Wavein[i][2] = Variables[ivar][1]
#+self,if=trace.
          if Debug != 0 and ivar >= 240 and ivar <= 250:
            print("---------- ivar, var, line: ", ivar, " ", var,Variables[ivar][7])
            #debug()
#+self.
          break
        #endwhile
        i += 1
      ivar+= 1
    #endwhile ivar < Nvar
#}update variables in Wavein

    #Debug = 0

    iline = 0
    karr = -1
    karro = -1
    isend = 0

    while iline < Nwavein:

      #if iline == 3008: #reakpoint()

      win = Wavein[iline]
#+self,if=trace.
      if Debug != 0 and iline >= 2963:
        print("\n")
        debug(iline,win)
        print("\n")
      #endif
#+self.

      if win[0] == 'SKIP':
        if Debug > 0: print(NL,iline,":",NL,win,NL)
        iline += 1
        continue
      #endif win[0] == 'SKIP'

      if karr > -1:
        if win[0].strip().upper() == '$END': isend = 1
      #endif karr > -1

      if isend:

        #reakpoint()

        if isend == 2:
          arr = Arrays[karro]
        else:
          arr = Arrays[karr]
        #endif isend == 2

        nactrl = arr[2]
        vctrl = Variables[nactrl]
        nout = int(vctrl[1])
        nvars = arr[3]
        varis = arr[5]

        wbuff = []

        for iv in range(nvars):

          debug(iv)

          var = varis[iv]
          varbasename = var[1]
          varitems = var[2]

          k = -1

          for varitem in varitems:

            k += 1

            varname = varbasename + '(' + str(varitem[0]) + ')'
            ivar = varitem[-1]

            if Debug > 0:
              print("k,varitem,ivar,nout:",k,varitem,ivar,nout)

            if ivar >= 0 and varitem[0] <= nout:

              iwin = Variables[ivar][-1]
              if Debug > 0:
                print("iwin:",iwin)

              if iwin < 0:

                cline = chblanks + varname + '=' + str(Variables[ivar][1]) + '\n'
                wbuff.append([k,cline])

              else:

                win = Wavein[iwin]
                if Debug > 0:
                  print("win:",win)
                cwin = '\n'

                if len(win[7]) > 0: cwin = '!' + win[7] + '\n'

                if win[5] == 'C':
                  sout = win[0] + win[1] + "='" + win[2] + "' " + win[3] + '\n'
                elif win[5] == 'I':
                  sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
                elif win[5] == 'R':
                  sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
                elif win[5] == 'D':
                  sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
                elif win[5] == 'CMPLX':
                  sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
                elif win[5] == 'DCMPLX':
                  sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
                #endif win[5] == 'C'

                wbuff.append([k,sout])

                Wavein[iwin][0] = 'SKIP'
                #print("ivar,Variables[ivar]:",ivar,Variables[ivar])
                #print("k,iwin,Wavein[iwin]:",k,iwin,Wavein[iwin])
                #Quit("Ende in writewavein")

            #endif ivar >= 0
          #endfor varitem in varitems
        #endfor iv in range(nvars)

        #reakpoint()
        for i in range(nout):
          wiwrite(fo,' \n')
          for k in range(len(wbuff)):
            if wbuff[k][0] == i: wiwrite(fo,wbuff[k][1])
          #endfor k in range(len(wbuff))
        #endfor i in range(nout)

        isend = 0
        karr = -1
        continue
      #iendf isend

      ivar = -1

      #print("\n\niline,win[4]:",iline,win[4],"\n\n")

      if win[4] == 'v':

        ivar = GetVarNum(win[1])
        if ivar < 0: Quit("Negative index for variable  found!!")
        var = Variables[ivar]
        #print("ivar,var:",ivar,var)

        iarr = Icheck_Array_Var(ivar)

        if iarr > -1:
          chblanks = win[0]
          Check_Array(iarr)
          karro = karr
          karr = iarr
          isend=2
        #endif is_actrl > -1

      #endif win[4] == 'v'

      if win[2] == win[8] or win[8] == 'varvalo':

        # Line is emtpy, comment, namelist, or unchanged variable

        if ivar > -1 and var[-1] > -1 or ivar == -1:

          wiwrite(fo,WaveinO[iline])

          kline = iline + 1
          winc = win

          # treat the lines following the current one, until variable line,
          #namelist line is found
          # flag written line to skip
          while kline < Nwavein:
            wc0 = winc[0].strip()
            if winc[0] == '\n' or len(wc0) > 0 and wc0[0] == '!':
              if Wavein[kline][0] != 'SKIP': break #happens after array
              #if Wavein[kline][0] != 'SKIP': wiwrite(fo,WaveinO[kline])
              Wavein[kline][0] = 'SKIP'
              kline += 1
              winc = Wavein[kline]
              if winc[4] == 'v': break
            else:
              break
            #endif
          #endwhile
        #endif

      else:

        sout = win[0]

        if win[4] == 'v':

          cwin = '\n'
          if len(win[7]) > 0: cwin = '!' + win[7] + '\n'

          if win[5] == 'C':
            sout = win[0] + win[1] + "='" + win[2] + "' " + win[3] + '\n'
          elif win[5] == 'I':
            sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
          elif win[5] == 'R':
            sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
          elif win[5] == 'D':
            sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
          elif win[5] == 'CMPLX':
            sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
          elif win[5] == 'DCMPLX':
            sout = win[0] + win[1] + "=" + win[2] + " " + win[3] + cwin
          #endif win[5] == 'C'

          if var[-1] > -1:
            wiwrite(fo,sout)
            kline = iline + 1
            winc = win
            while kline < Nwavein:
              win0 = winc[0].strip()
              if (len(win0) == 0 or win0[0] == '!') and winc[2] == winc[8]:
                wiwrite(fo,WaveinO[kline])
                Wavein[kline][0] = 'SKIP'
                kline += 1
                winc = Wavein[kline]
              else:
                break
              #endif
            #endwhile
          #endif

        #endif win[4] == 'v'

      #endif win[2] == win[8]

      iline+= 1

    #end while iline < Nwavein

    fo.close()
    print("\n --- wave.in written ---\n")

#    checkskip()
    readwavein() # work around for problems with ,,SKIP'' key-word
#    checkskip()

  #endif kWaveinRead != 0:

#enddef writewavein():
def runwave(ev=''):



  global Wdirs, Wfiles, Wfile, Wcode, Wrun \
  ,Webea ,Wcurr ,Wipin ,Wcir ,Wpiny ,Wpinx ,Wpinz ,Wpinw ,Wpinh ,Wpinr \
  ,Wmpiz ,Wmpiy ,Wmpir ,Wmpip ,Wicbr ,Wselx ,Wsely ,Wselz ,Wphax \
  ,Wsigz ,Wsigy ,Wsgzp ,Wsgyp ,Wespr ,Wif2p ,Wnfrq ,Wflow ,Wfhig \
  ,WflowExp, WfhigExp, WnfrqExp \
  ,Wispe ,Wispm ,Widip ,Wnlpo ,Wbw ,Wibun ,Wnbun ,Wneib ,Wiamp \
  ,Wielo ,Wifol ,Wiefo ,Wirun ,Widat ,Witim ,Wvers ,Wisto, Wbeta, Wibri, Koverview \
  ,Wnoby ,Wnobz ,Wwal1 ,Wwal2 ,Wxabs ,Wzab1 ,Wzab2, KCode, Kebeam, Kcurr \
  ,Wesel,Wener,Wfd,Wiesel, Vfd, IsameCanvas, TextIn, LastPlot,Lastwin \
  ,FiggeoEph, Ioverview,WclipE, Icallfromoverview,Kpreload
  global IzCut,IyCut

  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  Wnfrq = 0
  Wispe = 0
  Wibun = 0
  Wisto = 0
  Wbeta = 0
  Wibri = 0
  Wifol = 0
  Wiefo = 0
  Wipin = 0

  #print("runwave:",ev,kWaveinRead)
  if ev != 'RECOVER' and kWaveinRead !=0:
    writewavein()
  #endif kWaveinRead !=0:

  print("\nStarting WAVE, i.e. executing:\n",WAVECom,"\n")

  if not WAVECom:

    Fwvs = open(FWVS,'r')

    nline = 0
    for line in Fwvs:

      if line == '$EOF': break

      nline += 1

      line = line.strip()
      c = list(line)

      if len(line) > 0 and c[0] != '*':
        if nline == 1: WAVECom = line
        elif nline == 2: ROOTCom = line
        elif nline == 3: EWOUTCom = line
        elif nline == 4: EDICom = line
        elif nline == 5:
          WinDef = line
          break
      #endif len(line) > 0 and c[0] != '*'

    #endfor line in Fwvs

    Fwvs.close()
  #endif not WAVECom

  istat = os.system(WAVECom)
  if istat:
    print("*** Could not run WAVE, please check command\n",WAVECom,"\nand waves.wvs\n***")
  else:
    Mmenu_gray('black')
    mhb_to_pylist()
    Mmenu_gray()
  #endif

#enddef runwave(ev)
def waveplot(ev):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  if not WAVECom:

    Fwvs = open(FWVS,'r')

    nline = 0
    for line in Fwvs:

      if line == '$EOF': break

      nline += 1

      line = line.strip()
      c = list(line)

      if len(line) > 0 and c[0] != '*':
        if nline == 1: WAVECom = line
        elif nline == 2: ROOTCom = line
        elif nline == 3: EWOUTCom = line
        elif nline == 4: EDICom = line
        elif nline == 5:
          WinDef = line
          break
      #endif len(line) > 0 and c[0] != '*'

    #endfor line in Fwvs

    Fwvs.close()
  #endif not WAVECom

  print("\n",ROOTCom,"\n")
  os.system(ROOTCom)
def HelpText(kmenu):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  MSG = Toplevel()

  help = Help[kmenu]
  n = help[0]

  i=1
  htext = ''
  wid = 0
  while i < n:
    ht = help[i]
    if len(ht)*10 > wid: wid = len(ht)*10
    htext+= ht + '\n'
    i+= 1

  msg = Message(MSG,bg=Bg_color, font=('arial,18'), width=wid,text = htext)
  msg.pack( )

#enddef HelpText(kmenu):

global MyWavesFont
MyWavesFont = ('arial',11)

fwvs = open("waves.wvs","r")
line = fwvs.readline()
line = fwvs.readline()
line = fwvs.readline()
line = fwvs.readline()
line = fwvs.readline()
words = line.split()
fwvs.close()

if len(words) > 4: font = words[4]
else: font = MyWavesFont[0]
if len(words) > 5: size = words[5]
else: size = MyWavesFont[1]

MyWavesFont = [font,size]

Debug = 0

FWAVEIN = 'wave.in'
FWVS = 'waves.wvs'

WAVECom = ''
ROOTCom = ''
EDICom = ''
EWOUTCom = ''
WinDef = ''

Wavein = []
WaveinO = []

Wmenu = []
Mapping = []
MenuVeto = []
MenuActive = []
Nmenuactive = 0
MenuAllVeto = []
NMenuAllVeto = []
MappingVeto = []
Veto = []
InvVeto = []
Variables = []
Arrays = []
VarToWaveIn = []
Specvar = []
Trigger = []
Calc = []
MMitem = []
Iback = []

SFrame = []
SComment = []
SMitem = []
SMexist = []

Help = []

NHelp = 0
Nmenu = -1
Nmenuveto = -1
NmenuOld = -1

Mother = 0
MenuMother = ['',-1]
MenuOld = 0
Daughter = 0
IMother = -1
Istack = -1

Nveto = -1
Ninvveto = -1
Nmap = -1
Nvar = -1
Narr = -1
Ntrigger = -1
Ncalc = -1
Nwavein = -1
Nmitem = -1
Kmitem = -1
Lmitem = 0
FIOitem = -1
Kmitemold = -1
Nsitem = -1

NULL = 0
ONE = 1
MONE = -1
SNULL = '0'
SONE = '1'
SMONE = '-1'
I = 0+1j
ZONE = 1+0j

Ical=0
Ifocus = -1

kWaveinRead = 0

Lastvar = 0

Tcolor='white'
Vetocolor='black'

Fwvs = open(FWVS,'r')

Wave = Fwvs.readline()
Root = Fwvs.readline()
WaveOut = Fwvs.readline()
Editor = Fwvs.readline()
WinGeo = Fwvs.readline()

if os.path.exists('waves_geo_and_font.cnf'):
  Fgeo = open('waves_geo_and_font.cnf','r')
  WinGeo = Fgeo.readline()
  Fgeo.close()
#endif os.path.exists('.waves_geo_and_font.cnf')

Wave = Wave.strip()
Root = Root.strip()
WaveOut = WaveOut.strip()
Editor = Editor.strip()

Fwvs.close()

###########################################################

WGmain = 0


# +PATCH,//WAVES/PYTHON
# +KEEP,wgui,T=PYTHON.

def debugcalc():
  print("\n*** ISPEC: ",Variables[116])
  print("\n*** ISPECM: ",Variables[284])
#enddef

def Calculate():
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH


  i = 0
  while i <= Ncalc:

    calc = Calc[i]

    if Calc[i][1] == 'VARIABLE':
      op1 = Variables[Calc[i][0]][1]
      typ1 = Variables[Calc[i][0]][2]
    elif Calc[i][1] == 'MAPPING' or Calc[i][1] == 'BONDING':
      op1 = Mapping[Calc[i][0]][2]
      typ1 = 'I'
    else:
      print("*** Error in Calculate(), invalid first operand in $CALC item ",i)
      Wexit('')
    #endif Calc[i][1] == 'VARIABLE'

    if Calc[i][4] == 'VARIABLE':
      op2 = Variables[Calc[i][3]][1]
      typ2 = Variables[Calc[i][3]][2]
    elif Calc[i][4] == 'MAPPING' or Calc[i][4] == 'BONDING':
      op2 = Mapping[Calc[i][3]][2]
      typ2 = 'I'
    elif Calc[i][4] == 'CONST':
      op2 = Calc[i][3]
      typ2 = 'I'
    else:
      print("*** Error in Calculate(), invalid second operand in $CALC item ",i)
      Wexit('')
    #endif Calc[i][4] == 'VARIABLE'

    if Calc[i][6] == 'VARIABLE':
      typr = Variables[Calc[i][5]][2]
    elif Calc[i][6] == 'MAPPING' or Calc[i][6] == 'BONDING':
      typr = 'I'
    else:
      print("*** Error in Calculate(), invalid type of result in $CALC item ",i)
      Wexit('')
    #endif Calc[i][6] == 'VARIABLE'

      # Convert all numbers to complex

    if typ1 == 'I' or typ1 == 'R' or typ1 == 'D':
      z1 = complex(op1)
    else:
      z1 = op1

    if typ2 == 'I' or typ2 == 'R' or typ2 == 'D':
      z2 = complex(op2)
    else:
      z2 = op2

    op = Calc[i][2]

    if op == '*' or op == '/' or op == '+' or op == '-':
      cline = str(z1) + op + str(z2)
      if Debug > 1:
        print("----- Calc ---")
        print("\n***** ",cline,"\n")
      res = eval(cline)

    elif op == '.AND.' or op == '.NAND.' or op == '.OR.' or op == '.NOR.':

      z1r = z1.real
      z2r = z2.real

      if z1.real != 0: z1r = 1
      else: z1r = 0
      if z2.real != 0: z2r = 1
      else: z2r = 0

      if op == '.AND.':
        res = z1r * z2r
      elif op == '.NAND.':
        res = z1r * z2r
        if res: res = 0
        else: res = 1
      elif op == '.OR.':
        res = z1r + z2r
      elif op == '.NOR.':
        res = z1r + z2r
        if res: res = 0
        else: res = 1
      #endif

      res = res + 0j
    else:
      print("*** Error in calc: Operation ",op," is undefined. ***")
      Wexit('')

    if Calc[i][6] == 'VARIABLE':

      if typr == 'I':
        if res.real >= 0: res = np.floor(res.real)
        else: res = np.ceil(res.real)
        res = str(int(res))
      elif typr == 'CMPLX' or typr == 'DCMPLX':
        res = "(" + str(res.real) + "," + str(res.imag)
      else:
        res = str(res.real)
      #endif typr == 'I'

      ivar = Calc[i][5]
      var = Variables[ivar]
      var[3] = var[1]
      var[1] = res

    else:
      print("\n*** Error in Calculate(Calc[i][6]) is not a variable, but",Calc[i][6])
      print("*** THIS IS NOT YET IMPLEMENTED")
      print("Check calcution",i," for variables:")
      print(Variables[calc[0]])
      print(Variables[calc[3]])
      print(Variables[calc[5]],"\n")
      return
    #endif Calc[i][6] == 'VARIABLE'

    i+= 1
  #endwhile i <= Ncalc
def CheckVetos():
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  i=0
  while i<=Nveto:

    veto = Veto[i][0]
    val = Veto[i][1]
    goal = Veto[i][2]

    if Veto[i][3] == 'VARIABLE':

      if Veto[i][4] == 'VARIABLE':

        if Variables[veto][2] != 'C':
          cval = complex(val)
          cvet = complex(Variables[veto][1])
        else:
          cval = val
          cvet = Variables[veto][1]

        if cvet == cval:
          Variables[goal][5] = 1
        else:
          Variables[goal][5] = 0

        if Specvar[goal][0] >= 0:
          MappingVeto[Specvar[goal][0]] = Variables[goal][5]

      elif Veto[i][4] == 'MAPPING' or Veto[i][4] == 'BONDING':

        if Variables[veto][1] == val:
          MappingVeto[goal] = 1
        else:
          MappingVeto[goal] = 0

    elif Veto[i][3] == 'MAPPING' or Veto[i][3] == 'BONDING':

      val = Mapping[i][2]

      if Veto[i][4] == 'VARIABLE':
        if Mapping[veto][2] == val:
          Variables[goal][5] = 1
        else:
          Variables[goal][5] = 0

      elif Veto[i][4] == 'MAPPING' or Veto[i][4] == 'BONDING':

        if Mapping[veto][2] == val:
          MappingVeto[goal] = 1
        else:
          MappingVeto[goal] = 0

    #enif Veto[i][3] == 'VARIABLE':

    i+= 1
  #endwhile i<Nveto:

  i=0
  while i<=Ninvveto:

    veto = InvVeto[i][0]
    val = InvVeto[i][1]
    goal = InvVeto[i][2]

    if InvVeto[i][3] == 'VARIABLE':

      if InvVeto[i][4] == 'VARIABLE':
        if Variables[veto][2] != 'C':
          cval = complex(val)
          cvet = complex(Variables[veto][1])
        else:
          cval = val
          cvet = Variables[veto][1]

        if cvet == cval:
          Variables[goal][5] = 0
        else:
          Variables[goal][5] = 1

      elif InvVeto[i][4] == 'MAPPING' or InvVeto[i][4] == 'BONDING':

        if Variables[veto][1] == val:
          MappingVeto[goal] = 0
        else:
          MappingVeto[goal] = 1

    elif InvVeto[i][3] == 'MAPPING' or InvVeto[i][3] == 'BONDING':

      val = Mapping[i][2]

      if InvVeto[i][4] == 'VARIABLE':

        if Mapping[veto][2] == val:
          Variables[goal][5] = 0
        else:
          Variables[goal][5] = 1

      elif InvVeto[i][4] == 'MAPPING' or InvVeto[i][4] == 'BONDING':

        if Mapping[veto][2] == val:
          MappingVeto[goal] = 0
        else:
          MappingVeto[goal] = 1

    #enif InvVeto[i][3] == 'VARIABLE':

    i+= 1
  #endwhile i<Ninvveto:

  i=0
  while i <= Nmenu: #MenuVeto

    menu = Wmenu[i] #Bau
    menuv = MenuVeto[i]

    MenuVeto[i][2] = 0
    k = menuv[0]

    if k >= 0:

      val = menuv[1]
      l = menuv[2]

      if menuv[4] == 'VARIABLE':

        if Variables[k][2] != 'C':
          val = complex(val)
          vval = complex(Variables[k][1])
        else:
          val = val
          vval = Variables[k][1]

      elif menuv[4] == 'MAPPING' or  menuv[4] == 'BONDING':
        mapp = Mapping[k]
        vval = mapp[2]
      #endif menuv[4] == 'VARIABLE'

      if menuv[3] != 0:
        if vval != val:
          menuv[2] = 1
      else :
        if vval == val:
          menuv[2] = 1
      #endif menuv[3] != 0:

    #endif k >= 0:

    i += 1
  #endwhile i <= Nmenu:

  i=0
  while i <= Nmenu: #MenuAllVeto

    menuv = MenuAllVeto[i]

    if menuv[0] <= 0:
      i += 1
      continue

    menu = Wmenu[i]

    iset = 1
    ia = 1
    while ia <= menuv[0]:

      menuvia = menuv[ia]

      k = menuvia[0]
      val = menuvia[1]
      l = menuvia[2]
      typ = menuvia[4]

      if typ == 'VARIABLE':
        if Variables[k][2] != 'C':
          val = complex(val)
          vval = complex(Variables[k][1])
        else:
          val = val
          vval = Variables[k][1]
      elif typ == 'MAPPING' or typ == 'BONDING':
        val = complex(val)
        vval = complex(Mapping[k][2])
      else:
        print("*** Error in CheckVetos::MenuAllVeto: Wrong type: ",typ)
        Wexit('')
      #endif typ == 'VARIABLE':

      if vval != val:
        iset = 0
        break
      #endif vval != val:

      ia+= 1
    #endwhile ia <= menuv[0]:

    if MenuVeto[i][2] != 0 or iset == 1:
      MenuVeto[i][2] = 1
      if MenuVeto[i][0] == -1:
        MenuVeto[i][0] = -2
    #endif MenuVeto[i]eto[i][2] != 0 or iset == 1:

    i += 1
  #endwhile i <= Nmenu:

def debugmapping():
  global Variables
  print("\nCheckMapping:")
  print(Variables[284])
  print(Variables[116])

def CheckMapping(kmap=-1):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

  for mm in range(Nmap+1):

    if kmap == -1: m = mm
    else: m = kmap

    M = Mapping[m]
    nvar = M[5]
    iset = 1

    for i in range(nvar):
      idx = 5 + i*3
      vnam = M[idx+1]
      ivar = M[idx+2]
      var = Variables[ivar]
      soll = str(M[idx+3])
      #print("\nCheckMapping:",i,vnam,ivar,soll,var)
      if soll != var[1]:
        iset = 0
        break
    #for i in range(nvar+1)

    if not iset:

      typ = M[3]

      if typ == 'BONDING':

        M[4] = M[2]
        M[2] = 0

        i=0
        while i < M[5]:
          ivar = M[7+i*3]
          var = Variables[ivar]
          Variables[ivar][3] = Variables[ivar][1]
          if Debug > 1: print("CheckMapping:",i," ",ivar," ",var)
          if var[2] == 'C': Variables[ivar][1] = ''
          else: Variables[ivar][1] = '0'
          i+=1
        #endwhile i <= M[5]

      elif typ == 'MAPPING':

        M[4] = M[2]
        M[2] = 0

      #endif typ == 'MAPPING'

    #if not iset

    if kmap != -1: break

  #for mm in range(Nmap+1)
  #debugmapping()
  pass

#def CheckMapping()

def CheckAll(kmenu,kitem,kvar):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH

# Checks triggers, i.e. itrig set, and sets variables of mappings/bondings.
# Then Calculate(), CheckVetos(), CheckButtons() are called.
# If itrig is set, corresponding menu is triggered.


  itemt = kitem + 1
  if kitem+2 >= len(Wmenu[kmenu]): return

  typ = Wmenu[kmenu][kitem+1]

  if typ == 'MAPPING' or typ == 'BONDING':
    kmap = Wmenu[kmenu][kitem + 2]
    mp = Mapping[kmap]
    ivar = abs(mp[1][1])
    #print(Variables[ivar])
    Variables[ivar][3] = Variables[ivar][1]
    Variables[ivar][1] = mp[2]
    #print(Variables[ivar])

  itrig = -1
  Iback = -11
  i=0

  while i <= Ntrigger and itrig == -1:

    if Wmenu[kmenu][itemt] == 'MAPPING' and \
    str( Mapping[kmap][2]) == '1' and \
    Trigger[i][1] == 'MAPPING' and \
    Trigger[i][0] == kmap \
    or \
    Wmenu[kmenu][itemt] == 'BONDING' and \
    str(Mapping[kmap][2]) == '1' and \
    Trigger[i][1] == 'BONDING' and \
    Trigger[i][0] == kmap \
    or \
    Wmenu[kmenu][itemt] == 'VARIABLE' and \
    Variables[kvar][1] == '1' and \
    Trigger[i][1] == 'VARIABLE' and Trigger[i][0] == kvar \
    or \
    Wmenu[kmenu][itemt] == 'TOGGLE' and \
    Variables[kvar][1] == '1' and \
    Trigger[i][1] == 'VARIABLE' and Trigger[i][0] == kvar:

      itrig = i
      break

    i+= 1
  #endwhile i <= Ntrigger and itrig == -1:

  # { check bondings, i.e. depending variables are reset, if bonding is not set
  #   *** Attention: no recursion, i.e. sequentially check in loop

#+self,if=catrace.
#  print()
#+self.

  if Wmenu[kmenu][itemt] == 'MAPPING' or Wmenu[kmenu][itemt] == 'BONDING':
    if Mapping[kmap][2]:
      m = 0
      while m < Mapping[kmap][5]:
        # attention: all variables are set, even if single veto occurs,
        # i.e. mapping will be reset.
        isvar = Mapping[kmap][7+3*m]
        if Variables[isvar][5] == 0: # i.e. no veto
          Variables[isvar][3] = Variables[isvar][1]
          Variables[isvar][1] = Mapping[kmap][8+3*m]
          #Mapping[kmap][6+3*m] = Mapping[kmap][8+3*m]
        #endif Variables[kvar][5] == 0
        m+= 1
      #endwhile m < Mapping[kvar][5]:
    elif Mapping[kmap][2] == 0 and Mapping[kmap][3] == 'BONDING':
      m = 0
      while m < Mapping[kmap][5]:
        isvar = Mapping[kmap][7+3*m]
        if Variables[isvar][5] == 0: # i.e. no veto
          itype = Variables[isvar][2]
          if itype == 'C':
            #Mapping[kmap][6+3*m] = ' '
            Variables[isvar][3] = Variables[isvar][1]
            Variables[isvar][1] = ''
          else:
            Variables[isvar][3] = Variables[isvar][1]
            Variables[isvar][1] = '0'
            #Mapping[kmap][6+3*m] = '0'
          #endif itype == 'C':
        #endif Variables[kvar][5] == 0
        m+= 1
      #endwhile m < Mapping[kvar][5]:

    #endif Mapping[kvar][2]:

  #endif Wmenu[kmenu][itemt] == 'MAPPING' or Wmenu[kmenu][itemt] == 'BONDING':

  #Map_to_Var()

  Calculate()
  CheckMapping()
  CheckVetos()
  CheckButtons()

  if itrig != -1:
    if \
    (Trigger[itrig][1] == 'MAPPING'\
    and Mapping[Trigger[itrig][0]][2] != 0) \
    or \
    (Trigger[itrig][1] == 'MAPPING' and \
    Mapping[Trigger[itrig][0]][2] != 0) \
    or \
    (Trigger[itrig][1] == 'TOGGLE' and \
    Variables[Trigger[itrig][0]][1] != 0) \
    or \
    (Trigger[itrig][1] == 'VARIABLE' and \
    Variables[Trigger[itrig][0]][1] != 0) :
      #Istack+=1
      #$iback[$istack] = $ismenu
      #$posx = $Wmenu[$ipmenu]->x()
      #$posy = $Wmenu[$ipmenu]->y()
      #Wmenu[Ipmenu].destroy()
      Iback = Imenu
      Imenu = Trigger[itrig][2]
      pmenu(Imenu)
    #endif
  #endif itrig != -1

def GetFocusWidget(kmenu):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  widget = None
  if kmenu >= 0 and kmenu < len(Pmenu):
    widget = Pmenu[kmenu].focus_get()
  return widget

def CheckButtons():
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  global \
  T_color,Eb_color,F_color,Veto_color,Passiv_color,Bg_color,B_color,\
  Select_color,Select_color,Select_color,Deselect_color, \
  Fgcol,Bgcol,Afgcol,Abgcol,Hlcol,Hlbcol,Vfgcol,Vbgcol


  M = Imenu
  i = 1

  if len(SMitem[M]) == 0: return

  Nsitem = SMitem[M][0]

  if Nsitem != SMitem[M][0]:
    exit("*** CheckButtons: Nsitem != SMitem[Imenu][0]")

  while i <= Nsitem :

    if SMitem[M][i][1] == 'MENU' or SMitem[M][i][1] == 'SELECT':

      pass

    elif SMitem[M][i][1] == 'VARIABLE':

      ivar = SMitem[M][i][2]

      if Variables[ivar][5]:
        vcolor = Vfgcol
        vstate = DISABLED
        selcol = 'gray'
      else:
        vcolor = 'black'
        vstate = NORMAL
        selcol = 'white'
      #endif Variables[ivar][5]

    elif SMitem[M][i][1] == 'TOGGLE':

      ivar = SMitem[M][i][2]

      if Variables[ivar][5]:
        vcolor = Vfgcol
        vstate = DISABLED
        selcol = Deselect_color
      else:
        vcolor = Fgcol
        vstate = NORMAL
        selcol = Select_color

    elif SMitem[M][i][1] == 'MAPPING' or SMitem[M][i][1] == 'BONDING':

      imap = SMitem[M][i][2]

      fgcol = Fgcol
      bgcol = Select_color
      afgcol = Afgcol
      abgcol = Abgcol
      hlcol = Hlcol
      hlbcol = Hlbcol
      vfgcol = Vfgcol
      vbgcol = Deselect_color
      reli = RAISED

      if MappingVeto[imap] != 0: #veto
        fgcol = vfgcol
        bgcol = vbgcol
        abgcol = vbgcol
        vstate = 'disable'
      elif Mapping[imap][2] != 0: # Mapping/Bonding status is active
        bgcol = Select_color
        vstate = 'normal'
        reli = SUNKEN
      else:
        bgcol = Deselect_color
        vstate = 'normal'
      #if MappingVeto[imap] != 0:

    #endif SMitem[M][i][1] == 'MENU':

    i+= 1
  #endwhile i <= Nsitem:


def Up(event,kmenu,kitem):

  if kmenu == -1:
    if kitem == 2:
      WGmain.WaveIn.focus()
    elif kitem == 3:
      WGmain.WaveRun.focus()
    elif kitem == 4:
      WGmain.WavePlot.focus()
    elif kitem == 1:
      WGmain.Exit.focus()
  #endif kmenu == -1

  if kmenu >= 0:
    ki = kitem
    l = len(SMitem[kmenu])
    if ki == 0: ki = l - 1
    #print("kitem,len,ki",kitem,l,ki,"\n",SMitem[kmenu][ki][0])
    SMitem[kmenu][ki][0].focus()
    return
  #endif kmenu >= 0

#def Up(event,kmenu,kitem)

def Down(event,kmenu,kitem):

  if kmenu == -1:
    if kitem == 1:
      WGmain.WaveRun.focus()
    elif kitem == 2:
      WGmain.WavePlot.focus()
    elif kitem == 3:
      WGmain.Exit.focus()
    elif kitem == 4:
      WGmain.WaveIn.focus()
    return
  #endif kmenu == -1

  if kmenu >= 0:
    ki = kitem + 2
    l = len(SMitem[kmenu])
    if ki == l: ki = 1
    #print("kitem,len,ki",kitem,l,ki,"\n",SMitem[kmenu][ki][0])
    SMitem[kmenu][ki][0].focus()
    return
  #endif kmenu >= 0

#def Down(event,kmenu,kitem)

def MouseIn(event):
  event.widget.focus()
#enddef MouseIn(event)

def FocusIn(event,kmenu,kitem,kvar):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  global \
  T_color,Eb_color,F_color,Veto_color,Passiv_color,Bg_color,B_color,\
  Select_color,Select_color,Select_color,Deselect_color, \
  Fgcol,Bgcol,Afgcol,Abgcol,Hlcol,Hlbcol,Vfgcol,Vbgcol


  if kvar >= 0: quit("focusin: kvar>=0")

  Imenu = kmenu
  ew = event.widget
  key=event.keysym


  if kmenu == -1:
    if kitem == 0:
      WGmain.WaveIn.focus()
    elif kitem == 1:
      WGmain.WaveRun.focus()
    elif kitem == 2:
      WGmain.WavePlot.focus()
    elif kitem == 3:
      WGmain.Exit.focus()
  #endif kmenu == -1

  #if Ical == 17:  quit("Ende in FocusIn")

  ivar = 0
  if kmenu >= 0:
    l = len(SMitem[kmenu])
    if kitem + 1 <= l:
      if SMitem[kmenu][kitem+1][1] == 'VARIABLE':
        ivar = 1
    #endif kitem + 1 <= l
  #endif kmenu >= 0

  if kvar == -11:
    event.widget.focus()
    return
  elif kvar == -12:
    event.widget.focus()
    return
  #endif kvar == -12

  if kmenu < 0:
    return
  #endif kmenu < 0


  if Debug > 0:
    print("\n\n FocusIn: event, key, kmenu, kitem, kvar:")
    print(event," ",key," ",kmenu," ",kitem," ",kvar)
    print("ew :",ew)
    print("Kmitem, :",Kmitem)

    if Debug > 1:
      if kmenu >= 0:
        print("SMitem[kmenu]: \n",SMitem[kmenu])
        print("len(SMitem[kmenu]): ",len(SMitem[kmenu]))
      #endif kmenu >= 0
    #endif Debug > 1

  #endif Debug > 0

  if (kmenu >=0 and key == 'Return'):
#  if (kmenu >=0 and (key == 'Return' or key == 'Tab')):

    styp = SMitem[kmenu][kitem+1][1]
    if styp == "VARIABLE":
      ivar = SMitem[kmenu][kitem+1][2]
      var = Variables[ivar]
      val = ew.get()
      if var[1] != val:
        var[3] = var[1]
        var[1] = val
        Ifocus = kitem
        pmenu_update(kmenu)
        Ifocus = -1
        return
      #endif var[1] != var[3]
    #endif styp == "VARIABLE"

    menu = Wmenu[kmenu]
    #print("\n\nFocusIn:",menu)

    if styp == 'TOGGLE':
      jvar = SMitem[kmenu][kitem+1][2]
      for i in range(len(menu)):
        item = menu[i]
        if type(item) == str and item == styp and menu[i+2] == jvar:
          break
        #endif type(item) == str and item == styp and menu[i+2] == jvar
      #endfor i in range(len(menu)):
    elif styp == 'MAPPING':
      jvar = SMitem[kmenu][kitem+1][2]
      for i in range(len(menu)):
        item = menu[i]
        if type(item) == str and item == styp and menu[i+1] == jvar:
          break
        #endif type(item) == str and item == styp and menu[i+2] == jvar
      #endfor i in range(len(menu)):
      ToggleVar(kmenu,i-1,jvar)
    elif styp == 'BONDING':
      jvar = SMitem[kmenu][kitem+1][2]
      for i in range(len(menu)):
        item = menu[i]
        if type(item) == str and item == styp and menu[i+1] == jvar:
          #print("FocusIn: BONDING found")
          break
        #endif type(item) == str and item == styp and menu[i+2] == jvar
      #endfor i in range(len(menu)):
      ToggleVar(kmenu,i-1,jvar)
    #endif key == 'Return' and (styp == 'TOGGLE' or styp = 'MAPPING' or styp == 'BONDING'):

  # endif key == 'Return' or 'Tab':

  Kmitem=-1

  FIOitem = 0

  #end if kvar == -1:

  event.widget.focus()

  Debug=0


# enddef FocusIn(event,kmenu,kitem,kvar):

def noFocusInArray(event,kmenu,kitem,kvar):
  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  global \
  T_color,Eb_color,F_color,Veto_color,Passiv_color,Bg_color,B_color,\
  Select_color,Select_color,Select_color,Deselect_color, \
  Fgcol,Bgcol,Afgcol,Abgcol,Hlcol,Hlbcol,Vfgcol,Vbgcol

  FocusIn(event,kmenu,kitem,kvar)
#enddef FocusInArray(event,kmenu,kitem,kvar)

def FocusOut(event,kmenu,kitem,kvar):

# This function updates Variables[...]

  global \
  Wave, Root, WaveOut, Editor, WinGeo, \
  Debug,FWAVEIN,FWVS,Wavein,WaveinO,Wmenu,Mapping,MenuVeto,MenuAllVeto,NMenuAllVeto,\
  WAVECom, ROOTCom, EWOUTCom, EDICom, MenuActive, Nmenuactive, \
  MappingVeto,Veto,InvVeto,Variables,Arrays,Specvar,Trigger,Calc,Help,NHelp,Nmenu,\
  Nmenuveto,NmenuOld,Mother,MenuMother,MenuOld,Daughter,IMother,Nveto,\
  Ninvveto,Nmap,Nvar,Iarr,Narr,Ntrigger,Ncalc,NULL,ONE,MONE,SNULL,SONE,SMONE,Lastvar,\
  Nwavein,kWaveinRead, KWAVES,MMitem,Nmitem,Kmitem,Imenu,Ipmenu,\
  Kmitemold, Iback, Istak, Tcolor, Vetocolor, SFrame, SComment, SMitem, \
  VarToWaveIn, PosX, PosY, WinPos, Nsitem,Pmenu,PadX,PadY,SMexist, \
  I,ZONE,ZNULL,Ical,Lmitem,FIOitem,PMenuGeo, Nfocus, MyWavesFont,Ifocus, \
  ScreenW,ScreeH,WinX,WinY,CanW,CanH
  global \
  T_color,Eb_color,F_color,Veto_color,Passiv_color,Bg_color,B_color,\
  Select_color,Select_color,Select_color,Deselect_color, \
  Fgcol,Bgcol,Afgcol,Abgcol,Hlcol,Hlbcol,Vfgcol,Vbgcol


  ew = event.widget
  key=event.keysym

  Nfocus = 0


  if kvar == -12:
    return
  elif kvar == -11:
    return
  #endif kvar == -12

  ivar = 0
  if kmenu >= 0:

    menu = Wmenu[kmenu]

    l = len(SMitem[kmenu])

    if kitem + 1 <= l:

      if SMitem[kmenu][kitem+1][1] == 'VARIABLE':

        ivar = 1

        jvar = SMitem[kmenu][kitem+1][2]
        for i in range(len(menu)):
          item = menu[i]
          if type(item) == str and item == 'VARIABLE' and menu[i+2] == jvar:
            break
          #endif type(item) == str and item == styp and menu[i+2] == jvar
        #endfor i in range(len(menu)):

        #print("focusout:: jvar, Variables[jvar]",jvar,Variables[jvar])
        val = str(ew.get())
        prev = Variables[jvar][1]
        Variables[jvar][3] = Variables[jvar][1]
        Variables[jvar][1] = val
        is_arr_ctrl = Variables[jvar][6]
        if is_arr_ctrl > -1 and val != prev:
          pmenu_update(kmenu)
        #endif is_arr_ctrl > -1
      elif SMitem[kmenu][kitem+1][1] == 'ARRAY':
        Quit("Array in FocusOut")
      #endif SMitem[kmenu][kitem+1][1] == 'VARIABLE'
    #endif kitem + 1 <= l:

  #endif kmenu >= 0

  Debug = 0

  if Debug > 0:

    print("\n\n event, key, kmenu, kitem, kvar:")
    print(event," ",key," ",kmenu," ",kitem," ",kvar)
    print("ew :",ew)

    if Debug > 1:
      if kmenu >= 0:
        print("SMitem[kmenu]: \n",SMitem[kmenu])
        print("len(SMitem[kmenu]): ",len(SMitem[kmenu]))
      #endif kmenu >= 0
    #endif Debug > 1

  #endif Debug > 0

  if key != '??' and kmenu >= 0:

    if key == 'Return' or key == 'Tab':

      val = ew.get()
      print("FocusOut, key, val:",key,val)
      Wmenu[kmenu][kitem] = val

      Variables[kvar][3] = Variables[kvar][1]
      Variables[kvar][1] = Wmenu[kmenu][kitem]

      CheckAll(kmenu,kitem,kvar)

    # endif key == 'Return':

  #endif key != '??':

#  Kmitem=-1

  if kvar == -1:

    # Widgets of main menu {

    i = 0
    while i <= Nmitem:
      widg = MMitem[i][0]
#      if widg == ew:
#        Kmitem = i
#        widg.configure(fg = Bg_color)
#      else:
      widg.configure(fg = F_color)
      i = i + 1
    #endwhile i <= Nmitem:

    # }Widgets of main menu
  elif kvar == -2:

#focuskvar2{
    Debug = 0


    if Lmitem == 1:
        ew = 0
    #endif Lmitem == 1:

    if Debug > 1:
      print("\n focuskvar2: \n kmenu, kvar, FIOitem: ",kmenu," ",kvar," ",FIOitem)
      print("Wmenu[kmenu]: \n",Wmenu[kmenu])
      print("ew: ",ew,"\n")
    #endif Debug > 1:

    if Lmitem == 0: CheckButtons()

#    typ = SMitem[kmenu][kitem]
#    if type(typ) == list and typ[1] == 'MENU':
#      typ[0].configure(fg='yellow')

    i = 1
    while i < len(SMitem[kmenu]):

      widg = SMitem[kmenu][i][0]
      #if widg == 0: break

      if widg.winfo_exists() == 0:
        i+= 1
        continue
      #endif widg.winfo_exists() == 0

      typ = SMitem[kmenu][i][1]

      if Debug > 0: print("FocusIn2: i, widg, typ: ",i," ",widg," ",typ)

      if typ == 'OkButton' \
      or typ == 'VARIABLE' \
      or typ == 'MENU' \
      or typ == 'HelpButton':
        i+= 1
        continue
      #endif typ == 'OkButton'

      if widg == ew:
        Kmitem = i
      #endif widg == ew:

      if typ == 'MAPPING' or typ == 'BONDING':

        imap = SMitem[kmenu][i][2]
        mapp = Mapping[imap]
        istat = mapp[2]
        iveto = MappingVeto[imap]
        fgcol = F_color
        reli = RAISED

        if iveto:
          bgcol = Deselect_color
          fgcol = Vfgcol
        else:
          if istat:
#            fgcol = Hlcol
            bgcol = Select_color
            reli = SUNKEN
          else:
            bgcol = Deselect_color
          #endif istat:
        #endif iveto:

        widg.configure(relief=reli)
      elif typ == 'TOGGLE':

        ivar = SMitem[kmenu][i][2]
        if Debug > 1: print("ivar: ",ivar)
        var = Variables[ivar]

        if Debug > 1: print(Variables[ivar])

        iveto = var[5]
        fgcol = F_color

        bgcol = Deselect_color
        reli = RAISED
        if var[1] == '1':
          bgcol = Select_color
          reli = SUNKEN
        #endif var[1] == '1':

        if iveto:
          fgcol = Vfgcol
        #endif iveto:

        if Debug > 1:
          print(kmenu," ",ivar," ",Variables[ivar])
          print(fgcol," ",bgcol," ",reli)
        #endif Debug > 1:

        widg.configure(relief=reli, \
        )

      else:
        print("*** Error in FocusIn/Out: Unknown typ: ",typ)
      #endif typ == 'MAPPING' or typ == 'BONDING':

      i+= 1
    #endwhile i <= Nmitem:
    Debug = 0
#}focuskvar2

  FIOitem = 1

  #end if kvar == -1:

  Debug = 0

# enddef FocusOut(event,kmenu,kitem,kvar):

#################################################################

#begin of wgui

def WAVESexit(ev):
  sys.exit()
#enddef


Pmenu = []
PMenuGeo = []

WGmain = Tk()
WGmain.bind("<Enter>",lambda event, kmenu=-1 : MenuEnter(event,kmenu))
WGmain.bind("<FocusIn>",lambda event, kmenu=-1 : MenuEnter(event,kmenu))

T_color = 'white'
Eb_color = 'blue'
F_color = 'black'
Veto_color = 'gray'
Passiv_color = 'gray'
Bg_color = 'yellow'
B_color = 'blue'
Select_color = 'magenta'
Select_color = 'cyan'

Select_color = 'red'
Deselect_color = 'blue'

Fgcol = 'black'
Bgcol = 'blue'
Afgcol = 'yellow'
Abgcol = 'blue'
Hlcol = 'yellow'
Hlbcol = 'red'
Vfgcol = 'gray'
Vbgcol = 'blue'


Nsitem = -1
Nfocus = 0

screen_width = WGmain.winfo_screenwidth()
screen_height = WGmain.winfo_screenheight()

wingeo = WinGeo.split()

wid = int(float(wingeo[0])*screen_width)
hei = int(float(wingeo[1])*screen_width)
posx = int(float(wingeo[2])*screen_width)
posy = int(float(wingeo[3])*screen_height)

WinPos = '+' + str(int(posx-0.2*screen_width)) + '+' + str(int(posy-0.5*screen_height))
#wgeo = str(wid) + 'x' + str(hei) + '+' + str(posx) + '+' + str(posy)
wgeo = '+' + str(posx) + '+' + str(posy)

readwavein()
readwvs()

kWaveinRead += 1

PadX = 5
PadY = 3

Istack = 0

Iback.append(Istack)

WGmain.configure(background = Bg_color)
WGmain.title('WAVES')
WGmain.geometry(wgeo)

WGmain.WaveIn = Button(WGmain,
                       name = 'bWaveIn',
                       text = 'Setup input',
                       fg = F_color, bg = B_color,
                       activeforeground = Bg_color, activebackground = B_color,
                       highlightcolor = Bg_color, highlightbackground = B_color,
                       font = MyWavesFont)

WGmain.WaveIn.bind('<Return>',lambda event: setup_input(event))
WGmain.WaveIn.bind('<Button-1>',lambda event: setup_input(event))
WGmain.WaveIn.bind('<Up>',lambda event, kmenu=-1, kitem=1: Up(event,kmenu,kitem))
WGmain.WaveIn.bind('<Down>',lambda event, kmenu=-1, kitem=1: Down(event,kmenu,kitem))

WGmain.WaveIn.focus()

WGmain.WaveRun = Button(WGmain,
                        name = 'bWaveRun',
                        text = 'Run WAVE',
                        fg = F_color, bg = B_color,
                        activeforeground = Bg_color, activebackground = 'blue',
                        highlightcolor = Bg_color, highlightbackground = 'blue',
                        font = MyWavesFont)
                        #font = ('arial', 16, 'bold'))

WGmain.WaveRun.bind('<Return>',lambda event: runwave(event))
WGmain.WaveRun.bind('<Button-1>',lambda event: runwave(event))
WGmain.WaveRun.bind('<Up>',lambda event, kmenu=-1, kitem=2: Up(event,kmenu,kitem))
WGmain.WaveRun.bind('<Down>',lambda event, kmenu=-1, kitem=2: Down(event,kmenu,kitem))


WGmain.WavePlot = Button(WGmain,
                         name = 'bWavePlot',
                         text = 'Plot results',
                         fg = F_color, bg = B_color,
                         activeforeground = Bg_color, activebackground = 'blue',
                         highlightcolor = Bg_color, highlightbackground = 'blue',
                         font = MyWavesFont)
                        #font = ('arial', 16, 'bold'))

WGmain.WavePlot.bind('<Return>',lambda event: waveplot(event))
WGmain.WavePlot.bind('<Button-1>',lambda event: waveplot(event))
WGmain.WavePlot.bind('<Up>',lambda event, kmenu=-1, kitem=3: Up(event,kmenu,kitem))
WGmain.WavePlot.bind('<Down>',lambda event, kmenu=-1, kitem=3: Down(event,kmenu,kitem))

WGmain.Exit = Button(WGmain,
                     name = 'bWaveExit',
                     text = 'Exit', fg = F_color, bg = 'red',
                     activeforeground = Bg_color, activebackground = 'red',
                     highlightcolor = Bg_color, highlightbackground = 'red',
                     font = MyWavesFont)
                    #font = ('arial', 16, 'bold'))

WGmain.Exit.bind('<Button-1>',lambda event: Wexit(event))
WGmain.Exit.bind('<Return>',lambda event: Wexit(event))
WGmain.Exit.bind('<Up>',lambda event, kmenu=-1, kitem=4: Up(event,kmenu,kitem))
WGmain.Exit.bind('<Down>',lambda event, kmenu=-1, kitem=4: Down(event,kmenu,kitem))

WGmain.WaveIn.pack(fill = 'x', pady = PadY, padx = PadX)
WGmain.WaveRun.pack(fill = 'x', pady = PadY, padx = PadX)
WGmain.WavePlot.pack(fill = 'x', pady = PadY, padx = PadX)
WGmain.Exit.pack(fill = 'x', pady = PadY, padx = PadX)

mmitem=[WGmain.WaveIn,'bWaveIn']
Nmitem = Nmitem + 1
MMitem.append(mmitem)

mmitem=[WGmain.WaveRun,'bWaveRun']
Nmitem = Nmitem + 1
MMitem.append(mmitem)


mmitem=[WGmain.WavePlot,'bWavePlot']
Nmitem = Nmitem + 1
MMitem.append(mmitem)

mmitem=[WGmain.Exit,'bExit']
Nmitem = Nmitem + 1
MMitem.append(mmitem)

kmitem=0
while kmitem <= Nmitem :
    MMitem[kmitem][0].bind('<FocusIn>',lambda event, kmenu=-1, kitem=kmitem, kvar=-1: FocusIn(event,kmenu,kitem,kvar))
    MMitem[kmitem][0].bind('<FocusOut>',lambda event, kmenu=-1, kitem=kmitem, kvar=-1: FocusOut(event,kmenu,kitem,kvar))
    MMitem[kmitem][0].bind('<Enter>',lambda event, kmenu=-1, kitem=kmitem, kvar=-1: FocusIn(event,kmenu,kitem,kvar))
    MMitem[kmitem][0].bind('<Leave>',lambda event, kmenu=-1, kitem=kmitem, kvar=-1: FocusOut(event,kmenu,kitem,kvar))
    kmitem = kmitem + 1

WGmain.attributes('-topmost', 1)
WGmain.mainloop()
