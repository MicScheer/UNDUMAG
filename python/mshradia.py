
# +PATCH,//RADIA/PYTHON
# +DECK,mshradia,T=PYTHON.

from __future__ import print_function #Python 2.7 compatibility
import radia as rad

import numpy as np
from numpy import *
from copy import *

from scipy.spatial.transform import Rotation as ROT
from pyhull import qconvex

import matplotlib as mpl
mpl.use('TkAgg')

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits import mplot3d
from matplotlib import cm #color maps

global  mshObs,mshTrf,mshNames,mshColors,mshCnt
global Ax,Fig
global MagVoxelField, PolVoxelField

mshObs = {}
mshTrf = {}
mshNames = {}
mshColors = {}
mshCnt = {}
mshCntMembers = {}
mshCntMaster = []

MagVoxelField = []
PolVoxelField = []

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
  if obj in mshCnt: return mshCnt[obj]
  else: return mshObs[obj][-1]
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

  #breakpoint()

  if obj in mshCnt:
    print(rad.ObjCntStuf(obj))
    return
  #endif

  Fig = plt.gcf()
  if isame == 0:
    Ax = Axes3D(Fig)

  faces = mshObs[obj][0][2]

  fpl =faces

  #breakpoint()

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
          fpl[ifpl][ip] = rota.apply(fpl[ifpl][ip])
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

  if scale == 'xz':
    xzmin = min(xmin,zmin)
    xzmax = max(xmax,zmax)
    dxz = (xzmax-xzmin)*0.55
    Ax.set_xlim3d(Xcen-dxz,xcen+dxz)
    Ax.set_xlim3d(Ycen-dy,ycen+dy)
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
    Ax.set_xlim3d(Ycen-dy,Ycen+dy)
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
  global  mshObs,mshTrf,mshNames,mshColors
  if IsPole == -1:
    if Br[0]*Br[0]+Br[1]*Br[1]+Br[2]*Br[2] == 0: IsPole = 1
    else: IsPole = 0
  #endif
  poly = rad.ObjPolyhdr(verts, ifaces, Br)
  if nam == '': nam = 'obj_index_' + str(poly)
  mshNames[poly] = nam
  mshObs[poly] = [[verts,ifaces,faces,bounds,Br],[],IsPole,nam]
  mshColors[poly] = color
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

def mshhull3d(vertices):

  vt = vertices.T
  bounds = [vt[0].min(),vt[0].max(),vt[1].min(),vt[1].max(),vt[2].min(),vt[2].max()]
  lhull = qconvex('i p',vertices)

  nface = int(lhull[0])
  ivert = nface + 2
  nvert = int(lhull[ivert])

  ifaces = []
  faces = []

  for i in range(1,nface+1):
    iface = np.fromstring(lhull[i],dtype=np.int,sep=' ')
    faces.append(vertices[iface])
    for i in range(len(iface)): iface[i] += 1
    ifaces.append(list(iface))
  #endfor

  verts = []

  for i in range(ivert+1,ivert+1+nvert):
    dv = np.fromstring(lhull[i],sep=' ')
    verts.append(list(dv))
  #endfor

  return verts,ifaces,faces,bounds

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
