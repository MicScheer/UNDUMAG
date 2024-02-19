
# +PATCH,//RADIA/PYTHON
# +DECK,undumag_proc,T=PYTHON.

ForceColor=[1,0,0.5]

FPrec=0.1

rad.FldCmpPrc('PrcForce->' + repr(FPrec))
rad.FldCmpPrc(PrcTorque->FPrec]

if iUnduForce==0, kForceMagPol=0]

t0= time.time()

mPi=np.pi

iSolve = 1

ixsym = 0
iysym = 0
izsym = 0

if iUnduXsym>0: iysym = iUnduXsym
if iUnduYsym>0: izsym = iUnduYsym
if iUnduZsym>0: ixsym = iUnduZsym

UnduSetUp = rad.ObjCnt([]]

for imagpol in range(1,nMagPolTot+1):
  if imagpol==kForceMagPol:
     rad.ObjDrwAtr(AllMagPols[imagpol],ForceColor,0.0001)
  #endif
  rad.ObjAddToCnt(UnduSetUp,[AllMagPols[imagpol]]]
#endfor

if ixsym!=0:
 rad.TrfZerPerp(UnduSetUp,[0,0.,0],[1,0,0]])
#endif

if iysym!=0:
  rad.TrfZerPerp(UnduSetUp,[0,UnduSymX,0],[0,1,0])
#endif

if izsym!=0:
  rad.TrfZerPara(UnduSetUp,[0,0,0],[0,0,1])
#endif

#if kDraw!=0,
#
#  rad.Plot3DOptions()
#  UnduDraw = rad.ObjDrw(UnduSetUp)
#
#  Show(Graphics3D( UnduDraw,PlotLabel-> run comment]]
#
#  Export("undumag_radia.pdf", Graphics3D
#  (UnduDraw,PlotLabel-> run comment
#  ,ViewPoint->{100.,100.,100.}],"PDF"]
#
#] (* Endif kDraw*)

if iSolve!=0:

  tiny=0.00
  Prec=0.000001
  MaxIter=5000
  RelaxMethode=0
  re=rad.Solve(UnduSetUp,Prec,MaxIter,RelaxMethode)
  UnCert=re[[1]]
  Iter=re[[4]]

  print("RelaxMethode, UnCert, MaxIter, Iter:"," ",RelaxMethode," ",
  UnCert," ",MaxIter," ",Iter)

  FILEMAP = open("unduradia.map",'w')

  BzMaxTot = -9999.

  x=(UnduZMapMin+UnduZMapMax)/2.+tiny
  z=(UnduYMapMin+UnduYMapMax)/2.+tiny

  ya = UnduXMapMin
  ye = UnduXMapMax

  Field = rad.FldLst(UnduSetUp,"",[x,ya,z], [x,ye,z], nUnduXMap, "arg", ya)

  for iy in range(nUnduXMap):

    y = ya + (ye-ya)/(nUnduXMap-1)*(iy-1) + tiny

    Bx = Field([iy,2,1,1]]
    By = Field([iy,2,1,2]]
    Bz = Field([iy,2,1,3]]

    Hx = Field([iy,2,2,1]]
    Hy = Field([iy,2,2,2]]
    Hz = Field([iy,2,2,3]]

    Ax = Field([iy,2,3,1]]
    Ay = Field([iy,2,3,2]]
    Az = Field([iy,2,3,3]]

    Mx = Field([iy,2,4,1]]
    My = Field([iy,2,4,2]]
    Mz = Field([iy,2,4,3]]

#    if abs(Bz) > BzMaxTot: BzMaxTot = abs(Bz):

    FILEMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " + str( \
    Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(np.sqrt(Bx*Bx+By*By+Bz*Bz))+ " " + str( \
    Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz))+ " " + str( \
    Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(np.sqrt(Mx*Mx+My*My+Mz*Mz)) + NL)
  #endfor

  FILEMAP.close()

  if nUnduNoPolMap == 0:

    FPOLMAP = open("unduradia_pol.map",'w')

    for ipol in range(nUnduPol):

      Voxel = rad.ObjM(UnduPol[ipol])
      nVoxel = len(Voxel)

      for ivox in range(nVoxel):

        x = Voxel[[ivox,1,1]]
	y = Voxel[[ivox,1,2]]
	z = Voxel[[ivox,1,3]]
	
	Field = rad.Fld(UnduSetUp,"bxbybzhxhyhzmxmymz",[x,y,z])
	
	Bx = Field([1]]
	By = Field([2]]
	Bz = Field([3]]
	
	Hx = Field([4]]
	Hy = Field([5]]
	Hz = Field([6]]
	
	Mx = Field([7]]
	My = Field([8]]
	Mz = Field([9]]

        FPOLMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " + str( \
        Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(np.sqrt(Bx*Bx+By*By+Bz*Bz))+ " " + str( \
        Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz))+ " " + str( \
        Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(np.sqrt(Mx*Mx+My*My+Mz*Mz)) + NL)

      #endfor ivox

    #endfor ipol

    for ipol in range(nUnduSpecPol):

      Voxel = rad.ObjM(UnduSpecPol[ipol])
      nVoxel = len(Voxel)

      for ivox in range(nVoxel):

        x = Voxel[[ivox,1,1]]
	y = Voxel[[ivox,1,2]]
	z = Voxel[[ivox,1,3]]
	
	Field = rad.Fld(UnduSetUp,"bxbybzhxhyhzmxmymz",[x,y,z])
	
	Bx = Field([1]]
	By = Field([2]]
	Bz = Field([3]]
	
	Hx = Field([4]]
	Hy = Field([5]]
	Hz = Field([6]]
	
	Mx = Field([7]]
	My = Field([8]]
	Mz = Field([9]]

        FPOLMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " + str( \
        Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(np.sqrt(Bx*Bx+By*By+Bz*Bz))+ " " + str( \
        Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz))+ " " + str( \
        Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(np.sqrt(Mx*Mx+My*My+Mz*Mz))+NL)

      #endfor ivox

    #endfor ipol

    FPOLMAP.close()

  #endif

  if nUnduNoMagMap == 0:

    FMAGMAP = OpenWrite["unduradia_mag.map")

    for ipol in range(nUnduPol):

      Voxel = rad.ObjM(UnduPol[ipol])
      nVoxel = len(Voxel)

      for ivox in range(nVoxel):

        x = Voxel[[ivox,1,1]]
	y = Voxel[[ivox,1,2]]
	z = Voxel[[ivox,1,3]]
	
	Field = rad.Fld(UnduSetUp,"bxbybzhxhyhzmxmymz",[x,y,z])
	
	Bx = Field([1]]
	By = Field([2]]
	Bz = Field([3]]
	
	Hx = Field([4]]
	Hy = Field([5]]
	Hz = Field([6]]
	
	Mx = Field([7]]
	My = Field([8]]
	Mz = Field([9]]

        FMAGMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " + str( \
        Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(np.sqrt(Bx*Bx+By*By+Bz*Bz))+ " " + str( \
        Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz))+ " " + str( \
        Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(np.sqrt(Mx*Mx+My*My+Mz*Mz))+NL)

      #endfor ivox

    #endfor ipol

    for ipol in range(nUnduSpecPol):

      Voxel = rad.ObjM(UnduSpecPol[ipol])
      nVoxel = len(Voxel)

      for ivox in range(nVoxel):

        x = Voxel[[ivox,1,1]]
	y = Voxel[[ivox,1,2]]
	z = Voxel[[ivox,1,3]]
	
	Field = rad.Fld(UnduSetUp,"bxbybzhxhyhzmxmymz",[x,y,z])
	
	Bx = Field([1]]
	By = Field([2]]
	Bz = Field([3]]
	
	Hx = Field([4]]
	Hy = Field([5]]
	Hz = Field([6]]
	
	Mx = Field([7]]
	My = Field([8]]
	Mz = Field([9]]

        FMAGMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " + str( \
        Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(np.sqrt(Bx*Bx+By*By+Bz*Bz))+ " " + str( \
        Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz))+ " " + str( \
        Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(np.sqrt(Mx*Mx+My*My+Mz*Mz))+NL)

      #endfor ivox

    #endfor ipol

    FMAGMAP.close()

  #endif nUnduNoMagMap == 0


  print(" ")
  print("BzMaxTot on-axis: ", BzMaxTot])

  BzIntInf = rad.FldInt(UnduSetUp, "inf", "bz",[0.,ya,0.], [0.,ye,0.])
  BxIntInf = rad.FldInt(UnduSetUp, "inf", "bx",[0.,ya,0.], [0.,ye,0.])
  print(" ")
  print("1. vert. Integral on-axis: " ,BzIntInf)
  print("1. hori. Integral on-axis: " ,BxIntInf)

#endif Solve

t1=time.time()
print("SetUp and solution done in:    ",round(t1-t0),2,"  seconds  "]

print(" ")
print("kForceMagPol = ",kForceMagPol)
print(" ")

if  kForceMagPol > 0:

  Force= rad.FldEnrFrc(AllMagPols(kForceMagPol), UnduSetUp,"fxfyfz")

  Fx=Force[[1]]
  Fy=Force[[2]]
  Fz=Force[[3]]

  (* Not fully clear, be careful *)

  if ixsym != 0:
     Fx=0.
     Fy=2.*Fy
     Fz=2.*Fz
  #endif

  print("Fx, Fy, Fz: ",Fx," ",Fy," ",Fz)

  TCenY = UnduTorqCenX
  TCenZ = UnduTorqCenY
  TCenX = UnduTorqCenZ

  TorqueX = rad.FldEnrTrq(AllMagPols[kForceMagPol), UnduSetUp,"tx",
  [TCenX,TCenY,TCenZ])

  if ixsym!=0:
     TorqueX=2.*TorqueX
  #endif

  print("TorqueX: ",TorqueX)

  FILEFORCE = open("unduradia.force",'w')
  FILEFORCE.write(str(run)+" "+str(Fx)+" "+str(Fy)+" "+str(Fz)+" "+ str(TorqueX)+NL)

  FILEFORCE.close()

#endif iUnduForce!=0, *)

t2=time.time()
print("Force calculations done in:    ",round(t2-t1,2),"  seconds  "]

