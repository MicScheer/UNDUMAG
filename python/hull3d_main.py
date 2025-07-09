#+PATCH,//WAVES/PYTHON
#+DECK,hull3d_main,T=PYTHON.

import sys
import numpy as np
from scipy.spatial import ConvexHull

global Tnone
global Hull3D,Tnpa,Tnone,THull3D, Hull3DList

Tnpa = type(np.array([]))
Tnone = type(None)

def plotfaces(faces, isame=0,
                facecolor='b',edgecolor='black',alpha=0.5,
                linewidth=-1,markercolor='!',ishow=1):

  global Isame,Iso,Ax

  Iso = Isame

  if not isame:

    xmn = 1.e30
    xmx = -1.e30
    ymn = 1.e30
    ymx = -1.e30
    zmn = 1.e30
    zmx = -1.e30

    for f in faces:
      xmn = min(f.T[0].min(),xmn)
      xmx = max(f.T[0].max(),xmx)
      ymn = min(f.T[1].min(),ymn)
      ymx = max(f.T[1].max(),ymx)
      zmn = min(f.T[2].min(),zmn)
      zmx = max(f.T[2].max(),zmx)
    #endfor

    dx = (xmx-xmn)*0.1
    dy = (ymx-ymn)*0.1
    dz = (zmx-zmn)*0.1

    xmin = xmn - dx
    xmax = xmx + dx
    ymin = ymn - dy
    ymax = ymx + dy
    zmin = zmn - dz
    zmax = zmx + dz

    null3d(xmin,xmax,ymin,ymax,zmin,zmax)
    Isame = 1

  #endif

  getzone('3d')

  ax = Ax

  if edgecolor == '!': edgecolor = getlinecolor()
  if linewidth < 0: linewidth = getlinewidth()

  face = mplot3d.art3d.Poly3DCollection(faces)

  face.set_color(facecolor)
  face.set_linewidth(linewidth)
  face.set_edgecolor(edgecolor)
  face.set_alpha(alpha)

  ax.add_collection3d(face)

  if ishow: showplot()

  Isame = Iso

#enddef plotfaces

def read_faces(fname,cs='xyz'):

  F=open(fname,'r')
  fread = F.readlines()
  F.close()

  faces = []
  voxels = []

  l=0
  nface=-1
  #reakpoint()
  nmag = int(fread[l].strip())
  l += 1
  for imag in range(nmag):
    npoi = int(fread[l].split()[0])
    voxels.append(fread[l].split())
    nface += 1
    l += 1
    fac = []
    for ipoi in range(npoi):
      p = np.fromstring(fread[l].strip(),dtype=float,sep=' ')
      if cs.lower() == 'xzy':
        fac.append([p[0],p[2],p[1]])
      else:
        fac.append([p[0],p[1],p[2]])
      #endif
      l += 1
    #endfor npoi
    faces.append(np.array(fac))
  #endfor nmag

  return faces,voxels
#enddef read_faces(fname)

def plotqhull3d(vertices,ifaces,faces, isame=0,
                facecolor='b',edgecolor='black',alpha=0.5,
                linewidth=-1,markercolor='!',modus='facets',ishow=1):

  global Isame,Iso,Ax

  Iso = Isame

  if not isame or modus != 'facets': xyz = vertices.T

  if not isame:

    xmn = xyz[0].min()
    xmx = xyz[0].max()
    ymn = xyz[1].min()
    ymx = xyz[1].max()
    zmn = xyz[2].min()
    zmx = xyz[2].max()

    dx = (xmx-xmn)*0.1
    dy = (ymx-ymn)*0.1
    dz = (zmx-zmn)*0.1

    xmin = xmn - dx
    xmax = xmx + dx
    ymin = ymn - dy
    ymax = ymx + dy
    zmin = zmn - dz
    zmax = zmx + dz

    null3d(xmin,xmax,ymin,ymax,zmin,zmax)
    Isame = 1

  #endif

  getzone('3d')

  ax = Ax

  if edgecolor == '!': edgecolor = getlinecolor()
  if linewidth < 0: linewidth = getlinewidth()

  if modus == 'points' or modus == 'vertices':

    setcolor(markercolor)
    if markercolor == '!': markercolor = getmarkercolor()
    setmarkercolor(markercolor)

    vplxyz(xyz[0],xyz[1],xyz[2])

  else:

    face = mplot3d.art3d.Poly3DCollection(faces)

    face.set_color(facecolor)
    face.set_linewidth(linewidth)
    face.set_edgecolor(edgecolor)
    face.set_alpha(alpha)

    ax.add_collection3d(face)

  #endif

  if ishow: showplot()

  Isame = Iso

#enddef plotqhull3d

def qhull3d(x,y=None,z=None,modus='merge'):

  global Hull3D,Tnpa,Tnone,THull3D, Hull3DList

  if type(y) == Tnone:
    xyz = np.array(x).T
    x = xyz[0]
    y = xyz[1]
    z = xyz[2]
  #endif

  points = np.array([x,y,z]).T
  #reakpoint()

  x = np.array(x)
  y = np.array(y)
  z = np.array(z)

  xmin = x.min()
  xmax = x.max()
  ymin = y.min()
  ymax = y.max()
  zmin = z.min()
  zmax = z.max()

  bounds = [xmin,xmax,ymin,ymax,zmin,zmax]

  Hull3D = ConvexHull(points,qhull_options="")

  nface = Hull3D.nsimplex
  ifaces = Hull3D.simplices
  iverts = Hull3D.vertices

  gcen = [0.0,0.0,0.0]
  for i in range(len(iverts)):
    gcen += points[iverts[i]]
  #endfor
  gcen /= len(iverts)

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

      nfaces2,ifaces2,iverts2 = qhull2d(x,y)

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
  Hull3DList = []
  for f in facets:
    Hull3DList.append(f)
    ifac = []
    for p in f: ifac.append(verts.index(p))
    ifaces.append(ifac)
  #endfor

  return verts,ifaces,facets,bounds

#enddef qhull3d(x,y,z)

def qhull2d(x,y=None):

  global Hull2D,Tnpa,Tnone

  if type(y) == Tnone:
    xy = np.array(x).T
    x = xy[0]
    y = xy[1]
  #endif

  points = np.array([x,y]).T
  #Quit(points)

  if type(y) == Tnone:
    xyz = np.array(x).T
    x = xyz[0]
    y = xyz[1]
  #endif

  points = np.array([x,y]).T

  Hull2D = ConvexHull(points)
  nfaces = Hull2D.nsimplex
  ifaces = Hull2D.simplices
  iverts = Hull2D.vertices

  return nfaces,ifaces,iverts

#enddef qhull2d(x,y)

def qhull2d_old(x,y=None):

  global Hull2D,Tnpa,Tnone

  if type(y) == Tnone:
    xy = np.array(x).T
    x = xy[0]
    y = xy[1]
  #endif

  points = np.array([x,y]).T
  lhull = qconvex('i p',points)

  nface = int(lhull[0])
  ivert = nface + 2
  nvert = int(lhull[ivert])

  ifaces = []
  faces = []
  for i in range(1,nface+1):
    iface = np.fromstring(lhull[i],dtype=int,sep=' ')
    ifaces.append(iface)
    faces.append(points[iface])
  #endfor

  verts = []
  xmin = 1.0e30
  xmax = -1.0e30
  ymin = 1.0e30
  ymax = -1.0e30

  for i in range(ivert+1,ivert+1+nvert):
    dv = np.fromstring(lhull[i],sep=' ')
    if dv[0] < xmin: xmin = dv[0]
    if dv[0] > xmax: xmax = dv[0]
    if dv[1] < ymin: ymin = dv[1]
    if dv[1] > ymax: ymax = dv[1]
    verts.append(dv)
  #endfor
  verts = np.array(verts)

  Hull2D = faces

  return verts,ifaces,faces,[xmin,xmax,ymin,ymax]

#enddef qhull2d_old(x,y)

def nqhull3d(nt='?',varlis='x:y:z',select='', plopt='',iplot=1, iretval=0,linewidth=1,
             facecolor='blue',mcolor='',edgecolor='black',alpha=0.3,ishow=1,modus='merge'):

  global Isame,Hull3D,THull3D,Hull3DList

  if type(nt) == str and nt == '?':
    print("\nUsage: hull = nqhull3d(nt,varlis,select,iplot=1)")
    return
  #endif type(nt) == str and nt == '?'

  if type(nt) == Tdf:
    pass
  elif type(nt) == str:
    ind = GetIndexN(nt)
    if ind == -1:
      print("*** Error in nqhull3d: Unknown Ntuple ***")
      return -1
    #endif
    nt = Ntup[ind]
  elif type(nt) == int and nt > 0:
    nt = Ntup[idn]
  else:
    print("*** Error in nqhull3d: Unknown Ntuple ***")
    return -2
  #endif nt >= 0:

  if len(select):
    N = nt.query(select)
    nt = N
  #endif len(select)

  if not len(nt):
    print("*** Error in nqhull3d: No data, check ntuple and selection ***")
    return -1
  #endif

  if mcolor != '!' and mcolor != '':
    setmarkercolor(mcolor)
  #endif

  varl = nlistcolon(varlis)

  sx = eval(nparse(nt,varl[0]))
  sy = eval(nparse(nt,varl[1]))
  sz = eval(nparse(nt,varl[2]))

  ntd = ncre("ntd","ntd","x:y:z",1)
  var = nlistcolon(varlis)
  ntd.x = sx
  ntd.y = sy
  ntd.z = sz
  ntd = ntd.drop_duplicates()
  ntd.index = range(len(ntd))

  points = np.array([ntd.x,ntd.y,ntd.z]).T

  Hull3D = ConvexHull(points,qhull_options="")

  nface = Hull3D.nsimplex
  ifaces = Hull3D.simplices
  iverts = Hull3D.vertices

  gcen = [0.0,0.0,0.0]
  for i in range(len(iverts)):
    gcen += points[iverts[i]]
  #endfor
  gcen /= len(iverts)


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

  data = []
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

      nfaces2,ifaces2,iverts2 = qhull2d(x,y)

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
      data.append([ipoi+1,m+1,points[ipoi][0],points[ipoi][1],points[ipoi][2],vnx,vny,vnz])
      fac.append([points[ipoi][0],points[ipoi][1],points[ipoi][2]])
    #endfor
    facets.append(fac)
    ipoi = kface[0]
    data.append([ipoi+1,m+1,points[ipoi][0],points[ipoi][1],points[ipoi][2],vnx,vny,vnz])

  #endfor mface

  npd = pd.DataFrame(data,columns=['ipoi','iplan','x','y','z','nx','ny','nz'])
  nhull = ncre("Nhull3d","Nhull3d","ipoi:iplan:x:y:z:nx:ny:nz",ioverwrite=1)
  nhull = nfill("Nhull3d",npd)

  xmin = ntd.x.min()
  xmax = ntd.x.max()
  ymin = ntd.y.min()
  ymax = ntd.y.max()
  zmin = ntd.z.min()
  zmax = ntd.z.max()

  if iplot:

    plotoptions(plopt)

    if not Isame:

      dx = (xmax-xmin)*0.1
      dy = (ymax-ymin)*0.1
      dz = (zmax-zmin)*0.1

      xmin -= dx
      xmax += dx
      ymin -= dy
      ymax += dy
      zmin -= dz
      zmax += dz

      null3d(xmin,xmax,ymin,ymax,zmin,zmax)

    #endif isame

    ax = Ax

    faces = mplot3d.art3d.Poly3DCollection(facets)
    faces.set_color(facecolor)
    faces.set_edgecolor(edgecolor)
    faces.set_alpha(alpha)
    ax.add_collection3d(faces)

    if mcolor:
      if mcolor != '!':
        setmarkercolor(mcolor)
      #endif
      npl("Nhull3d","x:y:z",plopt="same")
    #endif

    if ishow: showplot()

  #endif iplot

  #if iretval: return vert,ifaces,faces,bounds
  #if iretval: return points,iverts,ifaces,xmin,xmax,ymin,ymax,zmin,zmax

  bounds = [xmin,xmax,ymin,ymax,zmin,zmax]

  verts =[]
  for pois in points[iverts]: verts.append(list(pois))

  ifaces = []
  Hull3DList = []
  for f in facets:
    Hull3DList.append(f)
    ifac = []
    for p in f: ifac.append(verts.index(p))
    ifaces.append(ifac)
  #endfor

  if iretval: return verts,ifaces,facets,bounds

#enddef nqhull3d(nt='?')

global Narg,Argv
Narg = len(sys.argv)
Argv = sys.argv

#print(Narg,Argv)

if Narg > 1: fin = Argv[1]
else : fin = 'hull3d.in'

if Narg > 2: fout = Argv[1]
else : fout = 'hull3d.out'

Fin = open(fin,'r')
pin = Fin.readlines()
Fin.close()

#reakpoint()
x = []
y = []
z = []

for p in pin:
    w = p.split()
    if w[0] == '%' or w[0] == '#' or w[0] == '!' or w[0] == '*': continue
    x.append(float(w[0]))
    y.append(float(w[1]))
    z.append(float(w[2]))
#endfor

x = np.array(x)
y = np.array(y)
z = np.array(z)

verts,ifaces,facets,bounds = qhull3d(x,y,z,'merge')

Fout = open(fout,'w')

Fout.write(str(bounds[0]) + ' ' + str(bounds[1]) + ' ! xmin, xmax\n')
Fout.write(str(bounds[2]) + ' ' + str(bounds[3]) + ' ! ymin, ymax\n')
Fout.write(str(bounds[4]) + ' ' + str(bounds[5]) + ' ! zmin, zmax\n')

Fout.write(str(len(verts)) + '  !number of vertices\n')
Fout.write(str(len(ifaces)) + '  !number of faces\n')

khull = []
for v in verts:
    for k in range(len(x)):
        if abs(v[0]-x[k]) > 1.0e-12: continue
        if abs(v[1]-y[k]) > 1.0e-12: continue
        if abs(v[2]-z[k]) > 1.0e-12: continue
        khull.append(k)
    #endfor
    Fout.write(str(v[0]) + ' ' + str(v[1]) + ' ' + str(v[2]) + str(k) + ' ' + '\n')
#endfor

for k in khull:
    Fout.write(str(k) + '\n')
#endfor

for ifa  in ifaces:
    Fout.write(str(len(ifa)) + '\n')
    for k  in range(len(ifa)):
        Fout.write(str(khull[ifa[k]]) + '\n')
    #endfor
#endfor

for fa  in facets:
    Fout.write(str(len(fa)) + '\n')
    for k  in range(len(fa)):
        Fout.write(str(fa[k][0]) + ' ' + str(fa[k][1]) + ' ' + str(fa[k][2]) + ' ' + '\n')
    #endfor
#endfor

Fout.close()
