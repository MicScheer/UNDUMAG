
# +PATCH,//RADIA/PYTHON
# +DECK,mshradia,T=PYTHON.

from __future__ import print_function #Python 2.7 compatibility
import radia as rad

import sys,os,platform
import numpy as np
from numpy import *
from copy import *

from scipy.spatial.transform import Rotation as ROT
from scipy.spatial import ConvexHull

#from pyhull import qconvex

import matplotlib as mpl
mpl.use('TkAgg')

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits import mplot3d
from matplotlib import cm #color maps

global  mshObs,mshTrf,mshNames,mshColors,mshCnt,mshCntMaster,mshCntMembers,mshMagPols
global Ax,Fig
global MagVoxelField, PolVoxelField

mshObs = {}
mshTrf = {}
mshMagPols = {}
mshNames = {}
mshColors = {}
mshCnt = {}
mshCntMembers = {}
mshCntMaster = []

MagVoxelField = []
PolVoxelField = []

def mshabort(text='\n*** Aborted ***'):
  print(text)
  if platform.system() == 'Windows': stat = os.system("taskkill /F /PID " + str(os.getpid()))
  else: stat = os.system("kill " + str(os.getpid()))
#enddef abort

def mshObjAddToCnt(cnt,obj):
  global mshCntMaster,mshCntMembers

  if obj in mshCnt: obj = rad.ObjCntStuf(obj)

  if type(obj) == list:
      rad.ObjAddToCnt(cnt,obj)
      for o in obj:
        if not o in mshCntMaster: mshCntMaster.append(o)
        if not o in mshCntMembers[cnt]: mshCntMembers[cnt].append(o)
      #endfor
    #endif
  else:
    rad.ObjAddToCnt(cnt,[obj])
    if not obj in mshCntMembers[cnt]: mshCntMembers[cnt].append(obj)
    if not obj in mshCntMaster: mshCntMaster.append(obj)
    #endif
  #endif
#enddef

def mshIsPole(obj):
  global mshObs,mshCnt
  if obj in mshCnt: return -1
  else: return mshObs[obj][-2]
#enddef

def mshGetObjName(obj):
  global mshObs,mshCnt
  breakpoint()
  if obj in mshCnt: return mshCnt[obj]
  else: return -1
#enddef

def mshObjDpl(obj, sopt='FreeSym->False',nam=''):

  global  mshObs,mshTrf,mshNames,mshColors,mshCntMembers

  dpl = rad.ObjDpl(obj,sopt)

  if obj in mshCnt:
    if nam == '': nam = mshCnt[obj] + '_Dpl'
    mshCnt[dpl] = nam
    mshCntMembers[dpl] = []
    mshNames[dpl] = nam
    for o in mshCntMembers[obj]:
      d = mshObjDpl(o)
      mshCntMembers[dpl].append(d)
    #endfor
  else:
    mshObs[dpl] = deepcopy(mshObs[obj])
    mshColors[dpl] = mshColors[obj]
    if nam == '':
      nam = mshNames[obj] + '_Dpl'
    #endif
    mshObs[dpl][2] = nam
    mshNames[dpl] = nam
    mshCntMaster.append(dpl)
  #endif

  return dpl
#enddef

def mshObjDrw(obj=0,facecolor='b',edgecolor='black',alpha=0.2,scale='xyz',
              modus='cen',msize=4.):

  global  mshObs,mshTrf,mshNames,mshColors,mshCnt,mshCntMembers
  global Fig,Ax,Xmin,Xmax,Ymin,Ymax,Zmin,Zmax,Xcen,Ycen,Zcen

  Xmin = 1.e30
  Xmax = -1.e30
  Ymin = 1.e30
  Ymax = -1.e30
  Zmin = 1.e30
  Zmax = -1.e30

  if obj == 0: obj = mshCntMaster

  if type(obj) == list:
    tit = ''
    isame = 0
    iob = 0
    for ob in obj:
      try:
        tit += '_' + mshNames[ob]
        if iob == 0:
          tit = mshNames[ob]
          iob +=1
        #endif
      except: pass
      _mshObjDrw(ob,facecolor,edgecolor,alpha,scale,tit,isame,modus,msize)
      isame = 1
    #endfor
  elif obj in mshCnt:
    try: tit = mshCnt[obj]
    except: tit = ''
    isame = 0
    for ob in mshCntMembers[obj]:
      bounds = mshObs[ob][0][3]
      xmin = bounds[0]
      xmax = bounds[1]
      ymin = bounds[2]
      ymax = bounds[3]
      zmin = bounds[4]
      zmax = bounds[5]
      Xmin = min(Xmin,xmin)
      Xmax = max(Xmax,xmax)
      Ymin = min(Ymin,ymin)
      Ymax = max(Ymax,ymax)
      Zmin = min(Zmin,zmin)
      Zmax = max(Zmax,zmax)
    #endfor

    for ob in mshCntMembers[obj]:
      _mshObjDrw(ob,facecolor,edgecolor,alpha,scale,tit,isame,modus,msize)
      isame = 1
    #endfor

  else:
    try: tit = mshNames[obj]
    except: tit = ''
    isame = 0
    bounds = mshObs[obj][0][3]
    Xmin = bounds[0]
    Xmax = bounds[1]
    Ymin = bounds[2]
    Ymax = bounds[3]
    Zmin = bounds[4]
    Zmax = bounds[5]
    _mshObjDrw(obj,facecolor,edgecolor,alpha,scale,tit,isame,modus,msize)
  #endif

  Fig.savefig("undumag_radia.pdf")

#enddef _mshObjDrw(obj,facecolor='b',edgecolor='black',alpha=0.5,scale='xyz')

def _mshObjDrw(obj,facecolor='b',edgecolor='black',alpha=0.1,scale='xyz',tit='',isame=0,
               modus='cen',msize=4):

  global  mshObs,mshTrf,mshNames,mshColors,mshCnt
  global Fig,Ax,Xmin,Xmax,Ymin,Ymax,Zmin,Zmax,Xcen,Ycen,Zcen
  global MagVoxelField, PolVoxelField

  #reakpoint()

  if obj in mshCnt:
    print(rad.ObjCntStuf(obj))
    return
  #endif

  faces = mshObs[obj][0][2]
  fpl =faces

  #reakpoint()

  xmin = Xmin
  xmax = Xmax
  ymin = Ymin
  ymax = Ymax
  zmin = Zmin
  zmax = Zmax

  for itr in mshObs[obj][1]:
    if mshTrf[itr][0] == 'Trsl':
      dxyz = mshTrf[itr][1]
      for ifpl in range(len(fpl)):
        for ip in range(len(fpl[ifpl])):
          fpl[ifpl][ip][0] += dxyz[0]
          fpl[ifpl][ip][1] += dxyz[1]
          fpl[ifpl][ip][2] += dxyz[2]
          xmin = min(xmin,fpl[ifpl][ip][0])
          xmax = max(xmax,fpl[ifpl][ip][0])
          ymin = min(ymin,fpl[ifpl][ip][1])
          ymax = max(ymax,fpl[ifpl][ip][1])
          zmin = min(zmin,fpl[ifpl][ip][2])
          zmax = max(zmax,fpl[ifpl][ip][2])
        #endfor
      #endfor
    elif mshTrf[itr][0] == 'Rot':
      rota = mshTrf[itr][2][0]
      for ifpl in range(len(fpl)):
        for ip in range(len(fpl[ifpl])):
          fpl[ifpl][ip] = list(rota.apply(fpl[ifpl][ip])[0])
          xmin = min(xmin,fpl[ifpl][ip][0])
          xmax = max(xmax,fpl[ifpl][ip][0])
          ymin = min(ymin,fpl[ifpl][ip][1])
          ymax = max(ymax,fpl[ifpl][ip][1])
          zmin = min(zmin,fpl[ifpl][ip][2])
          zmax = max(zmax,fpl[ifpl][ip][2])
        #endfor
      #endfor
    #endif
  #endfor

  #reakpoint()

  if isame == 0:
    Fig = plt.figure('MSHRADIA')
    Ax = Fig.add_subplot(111,projection='3d',visible=True,label='111')
    plt.show(block=False)
    #Fig = plt.gcf()
    #Ax = Axes3D(Fig)
    #null3d(xmin,xmax,ymin,ymax,zmin,zmax)
  else:
    Ax = plt.gca()
  #endfor

  Xmin = xmin
  Xmax = xmax
  Ymin = ymin
  Ymax = ymax
  Zmin = zmin
  Zmax = zmax

  dx = (xmax-xmin)*0.55
  dy = (ymax-ymin)*0.55
  dz = (zmax-zmin)*0.55

  Xcen = (xmax+xmin)/2.
  Ycen = (ymax+ymin)/2.
  Zcen = (zmax+zmin)/2.

  #reakpoint()
  if scale == 'xz':
    xzmin = min(xmin,zmin)
    xzmax = max(xmax,zmax)
    dxz = (xzmax-xzmin)*0.55
    Ax.set_xlim3d(Xcen-dxz,xcen+dxz)
    Ax.set_ylim3d(Ycen-dy,ycen+dy)
    Ax.set_zlim3d(Zcen-dxz,Zcen+dxz)
  elif scale == 'xyz':
    xyzmin = min(xmin,ymin,zmin)
    xyzmax = max(xmax,ymax,zmax)
    dxyz = (xyzmax-xyzmin)*0.55
    Ax.set_xlim3d(Xcen-dxyz,Xcen+dxyz)
    Ax.set_ylim3d(Ycen-dxyz,Ycen+dxyz)
    Ax.set_zlim3d(Zcen-dxyz,Zcen+dxyz)
  else:
    Ax.set_xlim3d(Xcen-dx,Xcen+dx)
    Ax.set_ylim3d(Ycen-dy,Ycen+dy)
    Ax.set_xlim3d(Zcen-dz,Zcen+dz)
  #endif

  pfaces = mplot3d.art3d.Poly3DCollection(fpl)

  try:
    col = mshColors[obj]
  except:
    col = facecolor
  #endtry

  pfaces.set_color(col)
  pfaces.set_edgecolor(edgecolor)
  pfaces.set_alpha(alpha)

  Ax.add_collection3d(pfaces)

  if modus == 'cen':

    for vox in MagVoxelField:
      col = mshColors[vox]
      points = []
      for ipoi in range(len(MagVoxelField[vox])):
        points.append(MagVoxelField[vox][ipoi][0])
      #endfor
      points=np.array(points).T
      plt.plot(points[0],points[1],points[2],marker='o',fillstyle='full',
               ls='',mec='black',mfc=col,mew=1,c='black',markersize=msize)
    #endfor

    for vox in PolVoxelField:
      points = []
      col = mshColors[vox]
      for ipoi in range(len(PolVoxelField[vox])):
        points.append(PolVoxelField[vox][ipoi][0])
      #endfor
      points=np.array(points).T
      plt.plot(points[0],points[1],points[2],marker='o',fillstyle='full',
               ls='',mec='black',mfc=col,mew=1,c='black',markersize=msize)
    #endfor

  #endif

  txyz(tit,"x [mm]","y [mm]","z [mm]")

#enddef mshObjDrw(obj)

def mshObjCnt(nam=''):
  global mshCnt,mshCntMembers,mshNames
  if nam == '': nam = 'Cnt_' + str(len(mshCnt))
  cnt = rad.ObjCnt([])
  mshCnt[cnt] = nam
  mshNames[cnt] = nam
  mshCntMembers[cnt] = []
  return cnt
#endif

def mshObjPolyhdr(verts, ifaces, faces, bounds, Br, nam='',color='b',IsPole=-1):

  global  mshObs,mshTrf,mshNames,mshColors,mshMagPols

  if IsPole == -1:
    if Br[0]*Br[0]+Br[1]*Br[1]+Br[2]*Br[2] == 0: IsPole = 1
    else: IsPole = 0
  #endif

  poly = rad.ObjPolyhdr(verts, ifaces, Br)

  if nam == '': nam = 'obj_index_' + str(poly)

  mshNames[poly] = nam
  mshObs[poly] = [[verts,ifaces,faces,bounds,Br],[],IsPole,nam]
  mshColors[poly] = color
  mshMagPols[nam] = poly

  return poly
#enddef

def mshTrfOrnt(obj,trf):
  global  mshObs,mshTrf
  rad.TrfOrnt(obj,trf)
  mshObs[obj][1].append(trf)
#enddef mshTrfOrnt(obj,trf)

def mshTrfTrsl(dxyz):
  global  mshObs,mshTrf
  trl = rad.TrfTrsl(dxyz)
  mshTrf[trl] = ['Trsl',dxyz]
  return trl
#enddef mshTrfTrsl(dxyz)

def mshTrfRot(crot,vrot,ang):
  global  mshObs,mshTrf
  rot = rad.TrfRot(crot,vrot,ang)
  mshTrf[rot] = ['Rot',[crot, vrot, ang]]
  Rot = ROT.from_rotvec(ang * np.array([vrot]))
  Rmat = Rot.as_matrix()
  mshTrf[rot] = ['Rot',[crot,vrot,ang],[Rot,Rmat]]
  return rot
#enddef mshTrfTrsl(dxyz)

def mshqhull2d(x,y):

  points = np.array([x,y]).T

  Hull2D = ConvexHull(points)

  nfaces = Hull2D.nsimplex
  ifaces = Hull2D.simplices
  iverts = Hull2D.vertices

  return nfaces,ifaces,iverts

#enddef mshqhull2d(x,y)

def mshhull3d(points, modus='merge'):

  if type(points) == list: points = np.array(points)

  Hull3D = ConvexHull(points,qhull_options="")

  nface = Hull3D.nsimplex
  ifaces = Hull3D.simplices
  iverts = Hull3D.vertices

  xmin = 1.0e30
  xmax = -1.0e30
  ymin = 1.0e30
  ymax = -1.0e30
  zmin = 1.0e30
  zmax = -1.0e30

  gcen = [0.0,0.0,0.0]

  for i in range(len(iverts)):
    p = points[iverts[i]]
    if p[0] < xmin: xmin = p[0]
    if p[0] > xmax: xmax = p[0]
    if p[1] < ymin: ymin = p[1]
    if p[1] > ymax: ymax = p[1]
    if p[2] < zmin: zmin = p[2]
    if p[2] > zmax: zmax = p[2]
    gcen += p
  #endfor

  gcen /= len(iverts)
  bounds = [xmin,xmax,ymin,ymax,zmin,zmax]

  vn = np.zeros([nface,6])

  mface = 0

  for i in range(nface):

    if vn[i][0] != 0: continue

    mface += 1

    pois = points[ifaces[i]]
    dp1 = pois[1]-pois[0]
    dp2 = pois[2]-pois[1]

    vnx = dp1[1]*dp2[2] - dp1[2]*dp2[1]
    vny = dp1[2]*dp2[0] - dp1[0]*dp2[2]
    vnz = dp1[0]*dp2[1] - dp1[1]*dp2[0]

    vnn = (vnx*vnx+vny*vny+vnz*vnz)**0.5
    vnx /= vnn
    vny /= vnn
    vnz /= vnn

    u = pois[0] - gcen

    if u[0]*vnx+u[1]*vny+u[2]*vnz < 0:
      ifis = ifaces[i]
      if1 = ifaces[i][1]
      ifis[1] = ifis[2]
      ifis[2] = if1
      ifaces[i] = ifis
      vnx = -vnx
      vny = -vny
      vnz = -vnz
    #endif

    vn[i][0] = i + 1
    vn[i][1] = vnx
    vn[i][2] = vny
    vn[i][3] = vnz
    vn[i][5] = mface

    ax = abs(vnx)
    ay = abs(vny)
    az = abs(vnz)

    if ax >= ay and ax >= az:
      vn[i][4] = 1
    elif ay >= ax and ay >= az:
      vn[i][4] = 2
    else:
      vn[i][4] = 3
    #endif

    if modus == 'merge':

      for j in range(i+1,nface):
        if vn[j][0] != 0.0: continue
        pois = points[ifaces[j]]
        dp1 = pois[1]-pois[0]
        dp2 = pois[2]-pois[1]
        wnx = dp1[1]*dp2[2] - dp1[2]*dp2[1]
        wny = dp1[2]*dp2[0] - dp1[0]*dp2[2]
        wnz = dp1[0]*dp2[1] - dp1[1]*dp2[0]
        wnn = (wnx*wnx+wny*wny+wnz*wnz)**0.5
        wnx /= wnn
        wny /= wnn
        wnz /= wnn
        u = pois[0] - gcen
        if u[0]*wnx+u[1]*wny+u[2]*wnz < 0:
          ifis = ifaces[j]
          if1 = ifaces[j][1]
          ifis[1] = ifis[2]
          ifis[2] = if1
          ifaces[j] = ifis
          wnx = -wnx
          wny = -wny
          wnz = -wnz
        #endif
        if wnx*vnx+wny*vny+wnz*vnz > 0.999:
          vn[j] = vn[i]
        #endif
      #endfor j
    #endif if modus == 'merge'

  #endfor i

  facets = []

  for m in range(mface):

    if modus == 'merge':

      x = []
      y = []
      lface = []

      for i in range(nface):
        if vn[i][5] == m+1:
          lface.append(ifaces[i][0])
          lface.append(ifaces[i][1])
          lface.append(ifaces[i][2])
          if vn[i][4] == 1:
            x.append(points[ifaces[i][0]][1])
            y.append(points[ifaces[i][0]][2])
            x.append(points[ifaces[i][1]][1])
            y.append(points[ifaces[i][1]][2])
            x.append(points[ifaces[i][2]][1])
            y.append(points[ifaces[i][2]][2])
          elif vn[i][4] == 2:
            x.append(points[ifaces[i][0]][0])
            y.append(points[ifaces[i][0]][2])
            x.append(points[ifaces[i][1]][0])
            y.append(points[ifaces[i][1]][2])
            x.append(points[ifaces[i][2]][0])
            y.append(points[ifaces[i][2]][2])
          else:
            x.append(points[ifaces[i][0]][0])
            y.append(points[ifaces[i][0]][1])
            x.append(points[ifaces[i][1]][0])
            y.append(points[ifaces[i][1]][1])
            x.append(points[ifaces[i][2]][0])
            y.append(points[ifaces[i][2]][1])
          #endif
        #endif
      #endfor nface

      nfaces2,ifaces2,iverts2 = mshqhull2d(x,y)

      p1 = points[lface[iverts2[0]]]
      p2 = points[lface[iverts2[1]]]
      p3 = points[lface[iverts2[2]]]

      dp1 = p2 - p1
      dp2 = p3 - p2

      vnx = dp1[1]*dp2[2] - dp1[2]*dp2[1]
      vny = dp1[2]*dp2[0] - dp1[0]*dp2[2]
      vnz = dp1[0]*dp2[1] - dp1[1]*dp2[0]

      vnn = (vnx*vnx+vny*vny+vnz*vnz)**0.5

      vnx /= vnn
      vny /= vnn
      vnz /= vnn

      u = p1 - gcen

      lv = len(iverts2)
      kface = []
      if u[0]*vnx+u[1]*vny+u[2]*vnz < 0:
        for i in range(lv):
          kface.append(lface[iverts2[lv-1-i]])
        #endfor
      else:
        for i in range(lv):
          kface.append(lface[iverts2[i]])
        #endfor
      #endif
    else:
      kface = ifaces[m]
      lv = len(kface)
    #endif modus == 'merge'

    fac = []
    for i in range(lv):
      ipoi = kface[i]
      fac.append([points[ipoi][0],points[ipoi][1],points[ipoi][2]])
    #endfor
    facets.append(fac)

  #endfor mface

  verts =[]

  for pois in points[iverts]: verts.append(list(pois))

  ifaces = []

  for f in facets:
    ifac = []
    for p in f: ifac.append(verts.index(p)+1) #RADIA counts indices from one
    ifaces.append(ifac)
  #endfor

  return verts,ifaces,facets,bounds

#enddef mshhull3d(vertices)

def mshrect(x,y,z,lx,ly,lz):
    xm = x - lx/2
    xp = x + lx/2
    ym = y - ly/2
    yp = y + ly/2
    zm = z - lz/2
    zp = z + lz/2
    verts = [\
    [xm,ym,zm],[xp,ym,zm],[xm,yp,zm],[xp,yp,zm], \
    [xm,ym,zp],[xp,ym,zp],[xm,yp,zp],[xp,yp,zp] \
    ]
    return np.array(verts)
#enddef mshrect

def txyz(pltit='Title',xtit='', ytit='', ztit='', tfs='12', xyzfs='10',
        titx=0.5, tity=1.0, titlesize='14'):

  global Ax

  if not Ax: return

  if pltit != '': Ax.set_title(pltit,fontsize=tfs,x=titx,y=tity)

  if xtit != '': Ax.set_xlabel(xtit,fontsize=xyzfs)
  if ytit != '': Ax.set_ylabel(ytit,fontsize=xyzfs)

  txexp = Ax.xaxis.get_offset_text()
  tyexp = Ax.yaxis.get_offset_text() # for the exponent

  if hasattr(Ax,'zaxis') and ztit != '':

    tzexp = Ax.zaxis.get_offset_text()

    # Bug in matplotlib:
    #tzexp.set_size(AxisLabelSize)
    #tzexp.set_position(xy=(LexpX,LexpY))
    #tzexp.set_rotation(LexpRot)

    # Workaround

    tzexp.set(visible=False)
    zticks = Ax.get_zticks()
    zamax = max(abs(zticks[:-1]))

    zexp = np.log10(zamax)

    if zexp >= 0: izexp = int(zexp)
    else: izexp = int(zexp) - 1

    if abs(izexp) >= 2:
      ztit += '       1.e' + str(izexp)
    #endif

    Ax.set_zlabel(ztit,fontsize=xyzfs,linespacing=0.5)

  #endif hasattr(Ax,'zaxis') and ztit != ''

  plt.show(block=False)

#enddef txyz
