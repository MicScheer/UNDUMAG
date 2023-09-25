
# +PATCH,//RADIA/PYTHON
# +DECK,mshrpl,T=PYTHON.

from mshradia import *

NL = '\n'
pi = np.pi

Br = [0.,0.,1.]

Chamf = 0.0

ChUsBot = Chamf
ChDsBot = ChUsBot
ChUsLeft = ChUsBot

vertices = mshrect(0.0,0.,0.0,10.,100.,10.)
verts,ifaces,faces,bounds = mshhull3d(vertices)
Mag = mshObjPolyhdr(verts, ifaces, faces, bounds, Br,"Mag","r")

dxyz = [100,0,0]
trl = mshTrfTrsl(dxyz)

ang = pi/180.*45.
crot = dxyz
vrot = [0,1,0]
rot = mshTrfRot(crot, vrot, ang)

mshTrfOrnt(Mag,trl)
mshTrfOrnt(Mag,rot)

C = mshObjCnt('C')
mshObjAddToCnt(C,Mag)

Ccopy = mshObjDpl(C,nam='Ccopy')

mshObjDrw(Ccopy)

