
# +PATCH,//RADIA/PYTHON
# +DECK,msh_radia_example,T=PYTHON.

from mshradia import *

NL = '\n'
pi = np.pi

Br = [0.,0.,1.]

vertices = mshrect(0.0,0.,0.0,10.,100.,10.)
verts,ifaces,faces,bounds = mshhull3d(vertices)
Mag = mshObjPolyhdr(verts, ifaces, faces, bounds, Br,"Mag","r")

MagCopy = mshObjDpl(Mag,nam='MagCopy')

dxyz = [100,0,0]
trl = mshTrfTrsl(dxyz)

ang = pi/180.*45.
crot = dxyz
vrot = [0,1,0]
rot = mshTrfRot(crot, vrot, ang)

mshTrfOrnt(Mag,trl)
mshTrfOrnt(Mag,rot)

C = mshObjCnt('C')
C = rad.ObjAddToCnt(C,[Mag])
C = rad.ObjAddToCnt(C,[MagCopy])

mshObjDrw(C)

