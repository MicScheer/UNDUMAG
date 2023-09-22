
# +PATCH,//RADIA/PYTHON
# +DECK,undumag_proc,T=PYTHON.

ForceColor=[1,0,0.5]

FPrec=0.1

rad.FldCmpPrc('PrcForce->' + repr(FPrec))

if iUnduForce==0: kForceMagPol=0

t0= time.time()

mPi=np.pi

ixsym = 0
iysym = 0
izsym = 0

if iUnduXsym>0: iysym = iUnduXsym
if iUnduYsym>0: izsym = iUnduYsym
if iUnduZsym>0: ixsym = iUnduZsym

UnduSetUp = rad.ObjCnt([])

# concept of wires of UNDUMAG:
# 1: type of coil
# 2: curr
# 3: x1
# 4: y1
# 5: z1
# 6: x2
# 7: y2
# 8: z2
# 9: color
# 10: number coil in group
# 11: absolute coil number

for imagpol in range(nMagPolTot):
  if imagpol==kForceMagPol:
     rad.ObjDrwAtr(AllMagPols[imagpol],ForceColor,0.0001)
  #endif
#endfor

rad.ObjAddToCnt(UnduSetUp,AllMagPols)

if ixsym!=0:
 rad.TrfZerPerp(UnduSetUp,[0,0.,0],[1,0,0])
#endif

if iysym!=0:
  rad.TrfZerPerp(UnduSetUp,[0,UnduSymX,0],[0,1,0])
#endif

if izsym!=0:
  rad.TrfZerPara(UnduSetUp,[0,0,0],[0,0,1])
#endif

#print(rad.ObjGeoLim(AllMagPols[0]))

if nUnduFilaments > 0:
  wires = []
  Coils = rad.ObjCnt([])
  FWIRE = open("undumag.fil",'r')
  fwires = FWIRE.readlines()
  FWIRE.close()
  for wlin in fwires:
    if wlin[0] == '*': continue
    swlin = wlin.split()
    ctype = int(swlin[0])
    curr = float(swlin[1])
    x1 = float(swlin[2])
    y1 = float(swlin[3])
    z1 = float(swlin[4])
    x2 = float(swlin[5])
    y2 = float(swlin[6])
    z2 = float(swlin[7])
    icolor = int(swlin[8])
    igroup = int(swlin[9])
    icoil = int(swlin[10])
    wires.append([ctype,curr,[x1,y1,z1],[x2,y2,z2],icolor,igroup,icoil])
    w = rad.ObjFlmCur([[x1,y1,z1],[x2,y2,z2]],curr)
    rad.ObjAddToCnt(Coils,w)
  #endfor

  rad.ObjAddToCnt(UnduSetUp,Coils)
#endif

#endif

if kDraw !=0:

  if iSolve == 0 and nUnduNoPolMap == 0:

    FPOLMAP = open("unduradia_pol.map",'w')

    for ipol in range(nUnduPol):

      Voxel = rad.ObjM(UnduPol[ipol])
      nVoxel = len(Voxel)

      for ivox in range(nVoxel):

        x = Voxel[ivox][0][0]
        y = Voxel[ivox][0][1]
        z = Voxel[ivox][0][2]

        FPOLMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ "0 0 0 0  0 0 0 0  0 0 0 0\n ")

      #endfor ivox

    #endfor ipol

    FPOLMAP.close()

  #endif

  if iSolve == 0 and nUnduNoMagMap == 0:

    FMAGMAP = open("unduradia_mag.map",'w')

    for mag in range(nUnduMag):

      Voxel = rad.ObjM(UnduMag[mag])
      nVoxel = len(Voxel)

      for ivox in range(nVoxel):

        x = Voxel[ivox][0][0]
        y = Voxel[ivox][0][1]
        z = Voxel[ivox][0][2]

        FMAGMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ "0 0 0 0  0 0 0 0  0 0 0 0\n ")

      #endfor ivox

    #endfor ipol

    FMAGMAP.close()

  #endif

#  U = rad.ObjM(UnduSetUp)
#  for iu in range(len(U)):
#    print(iu,U[iu])
#  #endfor

#  UnduDmp = rad.UtiDmp(AllMagPols[0], 'asc')
#
#  for ud in UnduDmp.split():
#    print(ud)
#  #endfor
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
#endif

if iSolve !=0 :

  tiny=0.00
  Prec=0.000001
  MaxIter=5000
  RelaxMethode=0

  re=rad.Solve(UnduSetUp,Prec,MaxIter,RelaxMethode)

  #print("RelaxMethode and results:",re)

  FILEMAP = open("unduradia.map",'w')

  BzMaxTot = -9999.

  x=(UnduZMapMin+UnduZMapMax)/2. #+tiny
  z=(UnduYMapMin+UnduYMapMax)/2. #+tiny

  ya = UnduXMapMin
  ye = UnduXMapMax

  Field = rad.FldLst(UnduSetUp,"bxbybzhxhyhzmxmymz",[x,ya,z], [x,ye,z], nUnduXMap, "arg", ya)

  for iy in range(nUnduXMap):

    y = Field[iy][0]

    Bx = Field[iy][1]
    By = Field[iy][2]
    Bz = Field[iy][3]
    B = np.sqrt(Bx*Bx+By*By+Bz*Bz)

    Hx = Field[iy][4]
    Hy = Field[iy][5]
    Hz = Field[iy][6]
    H = np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz)

    Mx = Field[iy][7]
    My = Field[iy][8]
    Mz = Field[iy][9]
    M = np.sqrt(Mx*Mx+My*My+Mz*Mz)

    FILEMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " + str( \
    Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(B)+ " " + str( \
    Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(H)+ " " + str( \
    Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(M) + NL)

  #endfor

  FILEMAP.close()

  if nUnduNoPolMap == 0:

    FPOLMAP = open("unduradia_pol.map",'w')

    for ipol in range(nUnduPol):

      Voxel = rad.ObjM(UnduPol[ipol])
      nVoxel = len(Voxel)

      for ivox in range(nVoxel):

        x = Voxel[ivox][0][0]
        y = Voxel[ivox][0][1]
        z = Voxel[ivox][0][2]

        Field = rad.Fld(UnduSetUp,"bxbybzhxhyhzmxmymz",[x,y,z])

        Bx = Field[0]
        By = Field[1]
        Bz = Field[2]	
	
        Hx = Field[3]
        Hy = Field[4]
        Hz = Field[5]
	
        Mx = Field[6]
        My = Field[7]
        Mz = Field[8]

        FPOLMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " + str( \
        Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(np.sqrt(Bx*Bx+By*By+Bz*Bz))+ " " + str( \
        Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz))+ " " + str( \
        Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(np.sqrt(Mx*Mx+My*My+Mz*Mz)) + NL)

      #endfor ivox

    #endfor ipol

    FPOLMAP.close()

  #endif

  if nUnduNoMagMap == 0:

    FMAGMAP = open("unduradia_mag.map",'w')

    for ipol in range(nUnduMag):

      Voxel = rad.ObjM(UnduMag[ipol])
      nVoxel = len(Voxel)

      for ivox in range(nVoxel):

        x = Voxel[ivox][0][0]
        y = Voxel[ivox][0][1]
        z = Voxel[ivox][0][2]

        Field = rad.Fld(UnduSetUp,"bxbybzhxhyhzmxmymz",[x,y,z])

        Bx = Field[0]
        By = Field[1]
        Bz = Field[2]	
	
        Hx = Field[3]
        Hy = Field[4]
        Hz = Field[5]
	
        Mx = Field[6]
        My = Field[7]
        Mz = Field[8]

        FMAGMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " + str( \
        Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(np.sqrt(Bx*Bx+By*By+Bz*Bz))+ " " + str( \
        Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz))+ " " + str( \
        Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(np.sqrt(Mx*Mx+My*My+Mz*Mz)) + NL)

      #endfor ivox

    #endfor ipol

    FMAGMAP.close()

  #endif nUnduNoMagMap == 0

  #  print(" ")
  #  print("BzMaxTot on-axis: ", BzMaxTot)

  BzIntInf = rad.FldInt(UnduSetUp, "inf", "bz",[0.,ya,0.], [0.,ye,0.])
  BxIntInf = rad.FldInt(UnduSetUp, "inf", "bx",[0.,ya,0.], [0.,ye,0.])

  print(" ")
  print("1. vert. Integral on-axis: " ,BzIntInf)
  print("1. hori. Integral on-axis: " ,BxIntInf)

  t1=time.time()
  print(NL,"SetUp and solution done in:    ",round(t1-t0,2),"  seconds  ")

  if  kForceMagPol > 0:

    Force= rad.FldEnrFrc(AllMagPols(kForceMagPol), UnduSetUp,"fxfyfz")

    Fx=Force[0]
    Fy=Force[1]
    Fz=Force[2]

    # Not fully clear, be careful

    if ixsym != 0:
      Fx=0.
      Fy=2.*Fy
      Fz=2.*Fz
    #endif

    print("Fx, Fy, Fz: ",Fx," ",Fy," ",Fz)

    TCenY = UnduTorqCenX
    TCenZ = UnduTorqCenY
    TCenX = UnduTorqCenZ

    TorqueX = rad.FldEnrTrq(AllMagPols[kForceMagPol], UnduSetUp,"tx",[TCenX,TCenY,TCenZ])

    if ixsym!=0:
      TorqueX=2.*TorqueX
    #endif

    print("TorqueX: ",TorqueX)

    FILEFORCE = open("unduradia.force",'w')
    FILEFORCE.write(str(run)+" "+str(Fx)+" "+str(Fy)+" "+str(Fz)+" "+ str(TorqueX)+NL)

    FILEFORCE.close()

    t2=time.time()
    print("Force calculations done in:    ",round(t2-t1,2),"  seconds  ")

  #endif iUnduForce!=0, *)

else:

  t1=time.time()
  print(NL,"SetUp done in:    ",round(t1-t0,2),"  seconds  ")

#endif Solve

