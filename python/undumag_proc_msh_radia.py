
# +PATCH,//RADIA/PYTHON
# +DECK,undumag_proc_msh_radia,T=PYTHON.

UnduSetUp = msh.mshObjCnt('UnduSetup')
msh.mshObjAddToCnt(UnduSetUp,AllMagPols)

ForceColor=[1,0,0.5]

FPrec=0.1

rad.FldCmpPrc('PrcForce->' + repr(FPrec))

t0= time.time()

mPi=np.pi

ixsym = 0
iysym = 0
izsym = 0

if iUnduXsym>0: iysym = iUnduXsym
if iUnduYsym>0: izsym = iUnduYsym
if iUnduZsym>0: ixsym = iUnduZsym

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

#global MagVoxelField, PolVoxelField

#breakpoint()

msh.MagVoxelField = {}
msh.PolVoxelField = {}

if iSolve !=0 :

  tiny=0.00
  Prec=0.000001
  MaxIter=5000
  RelaxMethode=0

  re=rad.Solve(UnduSetUp,Prec,MaxIter,RelaxMethode)

  #print("RelaxMethode and results:",re)

  FILEMAP = open("unduradia.map",'w')

  BzMaxTot = -9999.

  if nUnduZMap <= 1:
    UnduZMapMax = (UnduZMapMax+UnduZMapMin)/2.
    UnduZMapMin = UnduZMapMax
  #endif

  if nUnduYMap <= 1:
    UnduYMapMax = (UnduYMapMax+UnduYMapMin)/2.
    UnduYMapMin = UnduYMapMax
  #endif

  dx = (UnduZMapMax-UnduZMapMin)/max(1,nUnduZMap-1) #+tiny
  dz = (UnduYMapMax-UnduYMapMin)/max(1,nUnduYMap-1) #+tiny

  xmin = UnduZMapMin
  xmax = UnduZMapMax
  zmin = UnduYMapMin
  zmax = UnduYMapMax

  z = zmin - dz
  for iz in range(nUnduYMap):
    z += dz
    x = xmin - dx
    for ix in range(nUnduZMap):
      x += dx

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
    #endfor ix in range(nUnduZMap):
  #endfor iz in range(nUnduYMap):

  FILEMAP.close()

  FILEMAPINTINF = open("unduradia_int_inf.map",'w')

  ya = UnduXMapMin
  ye = UnduXMapMax

  z = zmin - dz
  for iz in range(nUnduYMap):
    z += dz
    x = xmin - dx
    for ix in range(nUnduZMap):
      x += dx

      BzIntInf = rad.FldInt(UnduSetUp, "inf", "bz",[x,ya,z], [x,ye,z])
      BxIntInf = rad.FldInt(UnduSetUp, "inf", "bx",[x,ya,z], [x,ye,z])

      FILEMAPINTINF.write(str(x) + " " + str(z) + " " + " " + str(BxIntInf) + " " \
      + str(BzIntInf) + '\n')

    #endfor
  #endfor

  FILEMAPINTINF.close()

  if nUnduNoPolMap == 0:

    FPOLMAP = open("unduradia_pol.map",'w')

    for po in msh.mshCntMembers[UnduPol]:

      Voxel = rad.ObjM(po)
      nVoxel = len(Voxel)

      msh.PolVoxelField[po] = []

      for ivox in range(nVoxel):

        if nVoxel == 2:
          x = Voxel[0][0]
          y = Voxel[0][1]
          z = Voxel[0][2]
        else:
          x = Voxel[ivox][0][0]
          y = Voxel[ivox][0][1]
          z = Voxel[ivox][0][2]
        #endif

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

        B = np.sqrt(Bx*Bx+By*By+Bz*Bz)
        H = np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz)
        M = np.sqrt(Mx*Mx+My*My+Mz*Mz)

        msh.PolVoxelField[po].append([[x,y,z],[Bx,By,Bz],[Hx,Hy,Hz,H],[Mx,My,Mz]])

        FPOLMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " \
        + str(Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(B)+ " " \
        + str(Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(H)+ " " \
        + str(Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(M) + NL)

        if nVoxel == 2: break

      #endfor ivox

    #endfor ipol

    FPOLMAP.close()

  #endif

  if nUnduNoMagMap == 0:

    FMAGMAP = open("unduradia_mag.map",'w')

    for mg in msh.mshCntMembers[UnduMag]:

      Voxel = rad.ObjM(mg)
      nVoxel = len(Voxel)

      msh.MagVoxelField[mg] = []

      for ivox in range(nVoxel):

        if nVoxel == 2:
          x = Voxel[0][0]
          y = Voxel[0][1]
          z = Voxel[0][2]
        else:
          x = Voxel[ivox][0][0]
          y = Voxel[ivox][0][1]
          z = Voxel[ivox][0][2]
        #endif

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

        B = np.sqrt(Bx*Bx+By*By+Bz*Bz)
        H = np.sqrt(Hx*Hx+Hy*Hy+Hz*Hz)
        M = np.sqrt(Mx*Mx+My*My+Mz*Mz)

        msh.MagVoxelField[mg].append([[x,y,z],[Bx,By,Bz],[Hx,Hy,Hz,H],[Mx,My,Mz]])

        FMAGMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ " " \
        + str(Bx)+ " " + str(By)+ " " + str(Bz)+ " " + str(B)+ " " \
        + str(Hx)+ " " + str(Hy)+ " " + str(Hz)+ " " + str(H)+ " " \
        + str(Mx)+ " " + str(My)+ " " + str(Mz)+ " " + str(M) + NL)

        if nVoxel == 2: break

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

  if  ForceMagPol > 0:

    Force= rad.FldEnrFrc(ForceMagPol, UnduSetUp,"fxfyfz")

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

    TorqueX = rad.FldEnrTrq(ForceMagPol, UnduSetUp,"tx",[TCenX,TCenY,TCenZ])

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

if kDraw !=0:

  if iSolve == 0:

   if nUnduNoPolMap == 0:

    FPOLMAP = open("unduradia_pol.map",'w')

    for po in msh.mshCntMembers[UnduPol]:

     Voxel = rad.ObjM(po)
     nVoxel = len(Voxel)

     msh.PolVoxelField[po] = []

     for ivox in range(nVoxel):

      if nVoxel == 2:
       x = Voxel[0][0]
       y = Voxel[0][1]
       z = Voxel[0][2]
      else:
       x = Voxel[ivox][0][0]
       y = Voxel[ivox][0][1]
       z = Voxel[ivox][0][2]
      #endif

      msh.PolVoxelField[po].append([[x,y,z],[0,0,0,0],[0,0,0,0],[0,0,0,0]])
      FPOLMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ "0 0 0 0  0 0 0 0 0 0 0 0\n ")

      if nVoxel == 2: break

     #endfor ivox

    #endfor ipol

    FPOLMAP.close()

   #endif

   if nUnduNoMagMap == 0:

    FMAGMAP = open("unduradia_mag.map",'w')

    for mg in msh.mshCntMembers[UnduMag]:

     Voxel = rad.ObjM(mg)
     nVoxel = len(Voxel)

     msh.MagVoxelField[mg] = []

     for ivox in range(nVoxel):

      if nVoxel == 2:
       x = Voxel[0][0]
       y = Voxel[0][1]
       z = Voxel[0][2]
      else:
       x = Voxel[ivox][0][0]
       y = Voxel[ivox][0][1]
       z = Voxel[ivox][0][2]
      #endif

      msh.MagVoxelField[mg].append([[x,y,z],[0,0,0,0],[0,0,0,0],[0,0,0,0]])
      FMAGMAP.write(str(x)+ " " + str(y)+ " " + str(z)+ "0 0 0 0  0 0 0 0  0 0 0 0\n ")

      if nVoxel == 2: break

     #endfor ivox

    #endfor ipol

    FMAGMAP.close()

   #endif

  #endif iSolve

#endif

msh.mshObjDrw(UnduSetUp)

#breakpoint()

