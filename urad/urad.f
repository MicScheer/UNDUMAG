*CMZ :  4.01/01 14/03/2023  18.56.24  by  Michael Scheer
*CMZ :  4.00/15 21/04/2022  12.17.12  by  Michael Scheer
*CMZ :  4.00/13 16/11/2021  17.32.24  by  Michael Scheer
*CMZ :  4.00/09 15/08/2020  08.51.05  by  Michael Scheer
*CMZ :  3.05/05 10/07/2018  09.19.31  by  Michael Scheer
*CMZ :  3.05/04 05/07/2018  11.10.09  by  Michael Scheer
*CMZ :  3.05/00 27/04/2018  15.22.16  by  Michael Scheer
*CMZ :  3.03/04 13/10/2017  09.16.28  by  Michael Scheer
*CMZ :  3.03/02 19/11/2015  13.32.35  by  Michael Scheer
*CMZ :  3.02/04 13/03/2015  10.38.25  by  Michael Scheer
*CMZ :  2.69/02 02/11/2012  16.40.18  by  Michael Scheer
*CMZ :  2.68/05 04/09/2012  13.30.58  by  Michael Scheer
*CMZ :  2.68/04 03/09/2012  11.52.24  by  Michael Scheer
*CMZ :  2.68/03 31/08/2012  09.45.28  by  Michael Scheer
*-- Author : Michael Scheer
      subroutine urad(icharge,
     &  gammai,dgamtot,
     &  xelec,yelec,zelec,vxelec,vyelec,vzelec,
     &  xf,yf,zf,efxn,efyn,efzn,
     &  xexit,yexit,zexit,vnxex,vnyex,vnzex,texit,ds,
     &  nthstep,nstep,ndim,traxyz,
     &  xobsv,yobsv,zobsv,phelow,phehig,
     &  nphener,phener,aradx,arady,aradz,stokes,powden,
     &  ieneloss,ivelofield
     &  ,istatus)
c123456789123456789_123456789_123456789_123456789_123456789_123456789_12
c Author: Michael Scheer, Michael.Scheer@Helmholtz-Berlin.de

c NO WARRANTY

*KEEP,GPLHINT.
*KEND.

c This subroutine calculates the trajectory and the synchrotron radiation
c of an electron passing a magnetic and electric field. The electric field
c part is very preliminary and not yet fully tested. The FORTRAN random
c generator is used in uradrndm. It should be replaced by a better one
c provided by the user.

c The fields B=(bx,by,bz) and E=(ex,ey,ez) are calculated in the routine
c uradfield(x,y,z,bx,by,bz,ex,ey,ez,gamma,istatus) provided by the user.
c As an example uradbmap.f may be used, which reads a 3D map of the mag. field

c Coordinate system (right handed):
c -----------------------------------

c      x: longitudinal direction
c      y: transversal vertical direction
c      z: transversal horizontal direction

c Units:
c ------

c SI-units: m, Tesla, sec, V etc., but eV for the photon energy
c The flux-density is given in Nph/s/mm**2/0.1%BW, the power-density in W/mm**2

c Input:
c --------

c integer icharge: Particle charge ( +/- 1)
c real*8 gammai: Gamma factor of the e-

c real*8 xelec:  Initial x of e-
c real*8 yelec:  Initial y of e-
c real*8 zelec:  Initial z of e-

c real*8 vxelec:  Initial velocity in x of e-
c real*8 vyelec:  Initial velocity in y of e-
c real*8 vzelec:  Initial velocity in z of e-
c The velocity is internally normalized, so the input norm does not matter

c real*8 xf: x of point in exit plane
c real*8 yf: y of point in exit plane
c real*8 zf: z of point in exit plane

c real*8 [efxn,efyn,efzn]: Normal vector of exit plane

c real*8 vnxex: x component of normal vector of exit plane
c real*8 vnyex: y component of normal vector of exit plane
c real*8 vnzex: z component of normal vector of exit plane

c real*8 ds : step size for tracking

c The tracking stops, when the electron hits the exit plane. The size of the last
c step is corrected such that the plane is hit.

c integer nthstep: If nstep > 0, the trajectory array traxyz is filled,
c                  see below
c integer ndim: Dimension of traxyz, see below

c real*8 phelow: Lowest photon energy / eV
c real*8 phehig: Higest photon energy / eV

c integer nphener: Number of equidistant photon energies

c integer ieneloss:  0: no energy loss due to synchotron radiation
c                    1: continous energy loss due to synchotron radiation
c integer ieneloss: -1: no energy loss due to synchotron radiation with quantum
c                       fluctuations

c integer ivelofield: Contral flag for the calculation of the velocity field:
c                    0: the spectrum includes the velocity field
c                    1: the specrum does not include the velocity field
c                    2: the spectrum includes only the velocity field

c Output:
c -------

c integer: istatus: Status flag:
c  0: no error found
c -1: initial gamma or velocity zero
c -2: dimension ndim of traxyz exceeded
c -3: bad value of ivelofield
c  else: status of uradfield

c real*8 xexit: x of last point of the trajectory
c real*8 yexit: y of last point of the trajectory
c real*8 zexit: z of last point of the trajectory
c real*8 texit: t of last point of the trajectory

c real*8 vnxex: x component of norm. velocity vector of last point
c real*8 vnyex: y component of norm. velocity vector of last point
c real*8 vnzex: z component of norm. velocity vector of last point

c real*8 phener(nphener): Array of equidistant photon energies

c integer nstep: Number of tracking steps done, i.e. used length of traxyz

c real*8 traxyz(1:14,i): Array:
c        traxyz(1,i):  x
c        traxyz(2,i):  y
c        traxyz(3,i):  z
c        traxyz(4,i):  tracking time
c        traxyz(5,i):  x-comp. of norm. velocity vector
c        traxyz(6,i):  y-comp. of norm. velocity vector
c        traxyz(7,i):  z-comp. of norm. velocity vector
c        traxyz(8,i):  x-comp. of mag. field in the center of the step
c        traxyz(9,i):  y-comp. of mag. field in the center of the step
c        traxyz(10,i): z-comp. of mag. field in the center of the step
c        traxyz(11,i): gamma
c        traxyz(12,i): x-comp. of elec. field in the center of the step
c        traxyz(13,i): y-comp. of elec. field in the center of the step
c        traxyz(14,i): z-comp. of elec. field in the center of the step

c The phase is calculated by phase=phase0+n*dt*dphase,
c where dphase is the phase difference of the nth step dt. Phase0=
c (xobsv-xelec)/clight. The phase factor of the integrand is
c exp(i*omega*phase),where omega referes to the considered photon energy.
c complex*16 aradx(i): x-comp. of amplitude of radiation field of phener(i)
c complex*16 arady(i): y-comp. of amplitude of radiation field of phener(i)
c complex*16 aradz(i): z-comp. of amplitude of radiation field of phener(i)

c real*8 array of Stokes parameters of ith photon energy:
c        stokes(1,i): S0, i.e. total intensity
c        stokes(2,i): S1, Stokes parameter of linear +/- 90 degree polarisation
c        stokes(3,i): S2, Stokes parameter of linear +/- 45 degree polarisation
c        stokes(4,i): S3, Stokes parameter of circular polarisation
c        S0 = sqrt(S1**2+S2**2+S3**2)

c Compilation:
c ------------

c For uradbmap at least F90 is required.
c The line length exceeds 72 characters, please use an appropriate
c compiler option. It is recommended to use compiler options to initialize all
c variables to zero and to treat them as ,,saved''

      implicit none

      complex*16 aradx(nphener),arady(nphener),aradz(nphener),
     &  ziom,zi,zidom,zone,ziomr1,zicr1,zic,
     &  expom1,expom,dexpomph1,dexpomph,ddexpomph,dexpom,
     &  expomv2,vstokes(4,3),
     &  apolh,apolr,apoll,apol45,dum3

      double precision
     &  gammai,dgamtot,dt2,powden,t,phase,
     &  xelec,yelec,zelec,vxelec,vyelec,vzelec,
     &  xexit,yexit,zexit,vnxex,vnyex,vnzex,texit,
     &  xobsv,yobsv,zobsv,phelow,phehig,
     &  phener(nphener),dom2,c,rspn
     &  ,traxyz(14,ndim),stokes(4,nphener),x1,y1,z1,vx1,vy1,
     &  vz1,x2,y2,z2,vx2,vy2,vz2
     &  ,ds,dtim,bshift,x2b,y2b,z2b,bx1,by1,bz1,bx2,by2,bz2
     &  ,dgamsum,gamma,dt
     &  ,x2int,y2int,z2int,ddt,dddt,ddt2,dddt2
     &  ,vx2int,vy2int,vz2int,vxpint,vypint,vzpint
     &  ,vxp,vyp,vzp
     &  ,x3int,y3int,z3int,vx3int,vy3int,vz3int,ddddt,ddddt2
     &  ,efx,efy,efz,xf,yf,zf,dist1,dist2,disti
     &  ,dtim0,beta,vn,efx2,efy2,efz2,t1,t2,clight,c1,
     &  dgamma,vxsign,bx,by,bz,bpx,bpy,bpz,rarg(5),px,py,pz,
     &  dphase,r,rx,ry,rz
     &  ,dom1,rnbx,rnby,rnbz,dum11,rnr4,br4,b3,rnr2,br2,bet1n,
     &  rnx,rny,rnz,r1,banwid,specnor,pownor,current,
     &  stok1,stok2,stok3,stok4,om,dom,hbarev,echarge,eps0,pi,vsto,dph,
     &  r0,efxn,efyn,efzn

      integer ieneloss,istatus,icharge,nphener,ivelofield,
     &  nthstep,izaehl,nstep,ndim,kstep,lstep,ifreq,isto,ifail

      integer :: ical=0

      save

      data bshift/0.5d0/
      data clight/2.99792458d8/
      data hbarev/6.58211889D-16/
      data banwid/1.0d-3/
      data current/0.10d0/ ! Be care, so also ucur in URADBUE etc.
      data eps0/8.854187817D-12/
      data echarge/1.602176462D-19/
      data pi/3.14159265358979d0/

      data zi/(0.0d0,1.0d0)/
      data zone/(1.0d0,0.0d0)/

      dph=0.0d0
      ical=ical+1

      if (nphener.gt.0) phener(1)=phelow
      if (nphener.gt.1) dph=(phehig-phelow)/(nphener-1)

      do ifreq=2,nphener
        phener(ifreq)=phener(ifreq-1)+dph
      enddo

      istatus=0
      ifail=0
      if (icharge.le.0) icharge=-1
      if (icharge.gt.0) icharge=1

      vn=norm2([efxn,efyn,efzn])
      efx=efxn/vn
      efy=efyn/vn
      efz=efzn/vn

      x1=xelec
      y1=yelec
      z1=zelec
      vx1=vxelec
      vy1=vyelec
      vz1=vzelec
      t1=0.0d0

      gamma=gammai
      beta=dsqrt((1.d0-1.d0/gamma)*(1.d0+1.d0/gamma))
      vn=sqrt(vx1*vx1+vy1*vy1+vz1*vz1)
      vx1=vx1/vn*clight*beta
      vy1=vy1/vn*clight*beta
      vz1=vz1/vn*clight*beta
      vn=beta*clight

c vxsign takes care for the direction of flight, since particle must gain
c energy if tracked back

      if (vx1.lt.0) then
        vxsign=-1.0d0
      else
        vxsign=1.0d0
      endif

      dgamsum=0.0d0
      dgamtot=0.0d0
      powden=0.0d0
      aradx=(0.0d0,0.0d0)
      arady=(0.0d0,0.0d0)
      aradz=(0.0d0,0.0d0)

      dtim=ds/vn
      dt=dtim
      dt2=dtim*bshift
      dtim0=dtim

      x2=x1
      y2=y1
      z2=z1
      t2=t1

      vx2=vx1
      vy2=vy1
      vz2=vz1

      x2b=x1+vx1*dt2
      y2b=y1+vy1*dt2
      z2b=z1+vz1*dt2

      call uradfield(x2b,y2b,z2b,bx2,by2,bz2,efx2,efy2,efz2,gamma,istatus)
      if (istatus.ne.0) ifail=ifail+abs(istatus)
      istatus=0

      nstep=0
      vn=sqrt(vx2*vx2+vy2*vy2+vz2*vz2)

      if (gamma.le.0.0d0.or.vn.le.0.0d0) then
        istatus=-1
        return
      endif

      kstep=-1
      nstep=0

      if(nthstep.gt.0) then

        nstep=nstep+1
        if (nstep.gt.ndim) then
          nstep=nstep-1
          istatus=-2
          goto 9000
        endif

        kstep=kstep+1
        if (kstep.eq.nthstep) kstep=0

        traxyz(1,nstep)=x2
        traxyz(2,nstep)=y2
        traxyz(3,nstep)=z2
        traxyz(4,nstep)=t2
        traxyz(5,nstep)=vx2/vn
        traxyz(6,nstep)=vy2/vn
        traxyz(7,nstep)=vz2/vn
        traxyz(8,nstep)=bx2
        traxyz(9,nstep)=by2
        traxyz(10,nstep)=bz2
        traxyz(11,nstep)=gamma
        traxyz(12,nstep)=efx2
        traxyz(13,nstep)=efy2
        traxyz(14,nstep)=efz2

      endif !nthstep.gt.0

      dom=0.0d0
      om=0.0d0
      if (nphener.gt.1) then
        om=phener(1)/hbarev
        dom=(phener(2)-phener(1))/hbarev
      else if (nphener.eq.1) then
        om=phener(1)/hbarev
      endif

      c=clight
      c1=1.0d0/clight

      zidom=zi*dom
      ziom=zi*om
      zic=zi*c

      lstep=0
      t=-dt
      r0=xobsv-xelec
      r=sqrt((xobsv-x1)**2+((yobsv-y1)**2+(zobsv-z1)**2))
      PHASE=(r-r0)*c1
      expom1=zone
      dexpomph1=zone

c--- Loop der Trajektorie

      izaehl=0
1000  continue

      izaehl=izaehl+1

      if (lstep.eq.1) then
        dtim=abs(dist2)/vn
        dt=dtim
        dt2=dtim/2.0d0
      endif

      x1=x2
      y1=y2
      z1=z2

      t1=t2

      vx1=vx2
      vy1=vy2
      vz1=vz2

      bx1=bx2
      by1=by2
      bz1=bz2

      x2b=x1+vx1*dt2
      y2b=y1+vy1*dt2
      z2b=z1+vz1*dt2

      call uradfield(x2b,y2b,z2b,bx2,by2,bz2,efx2,efy2,efz2,gamma,istatus)
      if (istatus.ne.0) ifail=ifail+abs(istatus)
      istatus=0

      call uradstep(x1,y1,z1,vx1,vy1,vz1,bx2,by2,bz2,efx2,efy2,efz2,
     &  dtim,x2,y2,z2,vx2,vy2,vz2,vxp,vyp,vzp,gamma,icharge,ieneloss,
     &  dgamma)

      if (ieneloss.ne.0) then
        dgamsum=dgamsum+vxsign*dgamma
        if (abs(dgamsum).gt.gamma*1.0d-8) then
          gamma=gamma+dgamsum
          dgamtot=dgamtot+dgamsum
          dgamsum=0.0d0
        endif
        beta=dsqrt((1.d0-1.d0/gamma)*(1.d0+1.d0/gamma))
        vn=sqrt(vx2*vx2+vy2*vy2+vz2*vz2)
        vx2=vx2/vn*clight*beta
        vy2=vy2/vn*clight*beta
        vz2=vz2/vn*clight*beta
      endif

      BX=VX2*C1
      BY=VY2*C1
      BZ=VZ2*C1

      BPX=VXP*C1
      BPY=VYP*C1
      BPZ=VZP*C1

C CONTRIBUTION OF TIME STEP TO SYNCHROTRON RADIATION {

C REAL PART OF INTEGRAND {

      RX=XOBSV-X2
      RY=YOBSV-Y2
      RZ=ZOBSV-Z2

      R=SQRT(RX*RX+RY*RY+RZ*RZ)
      R1=1.D0/R
      ZICR1=ZIC*R1

      RNX=RX*R1
      RNY=RY*R1
      RNZ=RZ*R1

C--- THE DISTANCE R IS INTRODUCED HERE EXPLICITLY (S. PROGRAM OF CHAOEN WANG

      BET1N=(1.D0-BX*RNX)-BY*RNY-BZ*RNZ

      br2=by**2+bz**2
      rnr2=rny**2+rnz**2
      b3=beta**3
      br4=br2**2
      rnr4=rnr2**2

      if(br2.lt.1.0d-4.and.rnr2.lt.1.0d-4) then
        bet1n=
     &    1.0d0/(1+beta)/gamma**2
     &    +beta*(rnr2/2.0d0
     &    +rnr4/8.0d0)
     &    +(br2/2.0d0
     &    -br2*rnr2/4.0d0
     &    -br2*rnr4/16.0d0)/beta
     &    +b3*br4*(1.0d0/8.0d0
     &    -rnr2/16.0d0
     &    -rnr4/64.0d0)
     &    -by*rny
     &    -bz*rnz
      endif

      DUM11=1.D0/BET1N
      DOM1=1.D0/(R*BET1N*BET1N)

      RNBX=RNX-BX
      RNBY=RNY-BY
      RNBZ=RNZ-BZ

      PX=(RNBY*BPZ-RNBZ*BPY)
      PY=(RNBZ*BPX-RNBX*BPZ)
      PZ=(RNBX*BPY-RNBY*BPX)

      IF (IVELOFIELD.EQ.0.OR.IVELOFIELD.EQ.2) THEN !2 WEGEN POWER
        DOM2=C*DOM1*R1/GAMMA**2
        RARG(1)=(RNY*PZ-RNZ*PY)*DOM1+(RNX-BX)*DOM2
        RARG(2)=(RNZ*PX-RNX*PZ)*DOM1+(RNY-BY)*DOM2
        RARG(3)=(RNX*PY-RNY*PX)*DOM1+(RNZ-BZ)*DOM2
      ELSE IF (IVELOFIELD.EQ.1) THEN
        RARG(1)=(RNY*PZ-RNZ*PY)*DOM1
        RARG(2)=(RNZ*PX-RNX*PZ)*DOM1
        RARG(3)=(RNX*PY-RNY*PX)*DOM1
      ELSE IF (IVELOFIELD.LT.0) THEN
        DOM2=C*DOM1*R1/GAMMA**2
        RARG(1)=(RNX-BX)*DOM2
        RARG(2)=(RNY-BY)*DOM2
        RARG(3)=(RNZ-BZ)*DOM2
      ELSE  !IVELOFIELD
        istatus=-3
        return
      ENDIF !IVELOFIELD

C DO NOT USE, RESULTS IN NUMERICAL PROBLEMS      RARG(4)=T+R*C1

      DPHASE=BET1N*DT

      RARG(5)=(RARG(1)*RARG(1)+RARG(2)*RARG(2)+RARG(3)*RARG(3))*DUM11

      powden=powden+rarg(5)*dt

C REAL PART OF INTEGRAND }

C COMPLEX PART OF INTEGRAND {

C    ASSUMES phener(I+1)=2*phener(I)   FOR IFREQ2P=2
C    OR phener(I+1)=phener(I)+DELTA    FOR IFREQ2P>2

C--- LOOP OVER ALL FREQUENCES

      if (nphener.gt.0) then

        IFREQ=1

        OM=phener(IFREQ)/hbarev
        ZIOM=ZI*OM

        EXPOM=EXPOM1
        DEXPOMPH1=EXP(ZIOM*DPHASE)
        DEXPOMPH=DEXPOMPH1

        IF(nphener.GT.1) THEN
          DEXPOM=EXP(ZIDOM*PHASE)
          DDEXPOMPH=EXP(ZIDOM*DPHASE)
        ENDIF  !IFREQ2P

        IF (IVELOFIELD.NE.2) THEN

          dum3=expom*(zone-dexpomph)/om/bet1n

          aradx(ifreq)=aradx(ifreq)+rarg(1)*dum3
          arady(ifreq)=arady(ifreq)+rarg(2)*dum3
          aradz(ifreq)=aradz(ifreq)+rarg(3)*dum3

        ELSE !IVELOFIELD

          EXPOMV2=R1/BET1N*EXPOM*(ZONE-DEXPOMPH)
          ZIOMR1=ZONE+ZICR1/OM

          aradx(ifreq)=aradx(ifreq)+EXPOMV2*(BX-RNX*ZIOMR1)
          arady(ifreq)=arady(ifreq)+EXPOMV2*(BX-RNX*ZIOMR1)
          aradz(ifreq)=aradz(ifreq)+EXPOMV2*(BX-RNX*ZIOMR1)

        ENDIF !IVELOFIELD

        IF (IVELOFIELD.NE.2) THEN

          DO IFREQ=2,nphener

            OM=OM+DOM
            EXPOM=EXPOM*DEXPOM
            DEXPOMPH=DEXPOMPH*DDEXPOMPH

            EXPOMV2=1.0D0/BET1N/OM*EXPOM*(ZONE-DEXPOMPH)

            aradx(ifreq)=aradx(ifreq)+RARG(1)*EXPOMV2
            arady(ifreq)=arady(ifreq)+RARG(2)*EXPOMV2
            aradz(ifreq)=aradz(ifreq)+RARG(3)*EXPOMV2

          ENDDO   !LOOP OVER ALL FREQUENCES

        else

          DO IFREQ=2,nphener

            OM=OM+DOM
            EXPOM=EXPOM*DEXPOM
            DEXPOMPH=DEXPOMPH*DDEXPOMPH

            EXPOMV2=R1/BET1N*EXPOM*(ZONE-DEXPOMPH)
            ZIOMR1=ZONE+ZICR1/OM

            aradx(ifreq)=aradx(ifreq)+EXPOMV2*(BX-RNX*ZIOMR1)
            arady(ifreq)=aradx(ifreq)+EXPOMV2*(BX-RNX*ZIOMR1)
            aradz(ifreq)=aradx(ifreq)+EXPOMV2*(BX-RNX*ZIOMR1)

          ENDDO   !LOOP OVER ALL FREQUENCES

        ENDIF !IVELOFIELD


C COMPLEX PART OF INTEGRAND }

C CONTRIBUTION OF TIME STEP TO SYNCHROTRON RADIATION }

        PHASE=PHASE+DPHASE
        EXPOM1=EXPOM1*DEXPOMPH1

      endif !(nphener.gt.0) then

      t2=t1+dtim

c ef is normal vector of perpendiculare plane at the end of the reference orbit
c dist is distance of electron to this plane
c tracking stops if trajectory hits this plane

      dist2=(x2-xf)*efx+(y2-yf)*efy+(z2-zf)*efz

      if (lstep.eq.0.and.dist2.lt.0.0d0.and.dist2.gt.-2.0d0*ds)  then

        lstep=1
        goto 1000

      else

        kstep=kstep+1
        if (kstep.eq.nthstep) kstep=0

        if(kstep.eq.0) then

          nstep=nstep+1

          if (nstep.gt.ndim) then
            nstep=nstep-1
            istatus=-2
            goto 9000
          endif

          traxyz(1,nstep)=x2
          traxyz(2,nstep)=y2
          traxyz(3,nstep)=z2
          traxyz(4,nstep)=t2
          traxyz(5,nstep)=vx2/vn
          traxyz(6,nstep)=vy2/vn
          traxyz(7,nstep)=vz2/vn
          traxyz(8,nstep)=bx2
          traxyz(9,nstep)=by2
          traxyz(10,nstep)=bz2
          traxyz(11,nstep)=gamma
          traxyz(12,nstep)=efx2
          traxyz(13,nstep)=efy2
          traxyz(14,nstep)=efz2

        endif

        if (lstep.eq.1) goto 9000
        goto 1000

      endif !lstep and dist2

9000  continue

      xexit=x2
      yexit=y2
      zexit=z2

      vn=sqrt(vx2*vx2+vy2*vy2+vz2*vz2)
      vnxex=vx2/vn
      vnyex=vy2/vn
      vnzex=vz2/vn

      texit=t2

      specnor=
     &  banwid
     & /(4.0d0*pi**2*clight*hbarev)
     & /(4.0d0*pi*eps0)
     &  *current/1.0d6 !per mm**2

      pownor=echarge/16.0d0/pi/pi/eps0/clight*current/1.0d6 !W/mm**2

      rspn=sqrt(specnor)

      vstokes(1,1)=( 0.0d0,        0.0d0)      !horizontal polarization
      vstokes(1,2)=( 0.0d0,        0.0d0)
      vstokes(1,3)=(-1.0d0,       -1.0d0)

      vstokes(2,1)=( 0.0d0,        0.0d0)      !right handed polarization
      vstokes(2,2)=( 0.0d0,       -1.0d0)
      vstokes(2,3)=(+1.0d0,        0.0d0)

      vstokes(3,1)=( 0.0d0,        0.0d0)      !left handed polarization
      vstokes(3,2)=( 0.0d0,       -1.0d0)
      vstokes(3,3)=(-1.0d0,        0.0d0)

      vstokes(4,1)=( 0.0d0,        0.0d0)      !45 degree linear polarization
      vstokes(4,2)=( 1.0d0,        0.0d0)
      vstokes(4,3)=(-1.0d0,        0.0d0)

      do isto=1,4
        vsto=dsqrt
     &    (cdabs(vstokes(isto,1))**2
     &    +cdabs(vstokes(isto,2))**2
     &    +cdabs(vstokes(isto,3))**2)
        vstokes(isto,1)=vstokes(isto,1)/vsto
        vstokes(isto,2)=vstokes(isto,2)/vsto
        vstokes(isto,3)=vstokes(isto,3)/vsto

      enddo

      do ifreq=1,nphener

        aradx(ifreq)=aradx(ifreq)*rspn
        arady(ifreq)=arady(ifreq)*rspn
        aradz(ifreq)=aradz(ifreq)*rspn

        apolh=
     &    aradx(ifreq)*conjg(vstokes(1,1))
     &    +arady(ifreq)*conjg(vstokes(1,2))
     &    +aradz(ifreq)*conjg(vstokes(1,3))

        apolr=
     &    aradx(ifreq)*conjg(vstokes(2,1))
     &    +arady(ifreq)*conjg(vstokes(2,2))
     &    +aradz(ifreq)*conjg(vstokes(2,3))

        apoll=
     &    aradx(ifreq)*conjg(vstokes(3,1))
     &    +arady(ifreq)*conjg(vstokes(3,2))
     &    +aradz(ifreq)*conjg(vstokes(3,3))

        apol45=
     &    aradx(ifreq)*conjg(vstokes(4,1))
     &    +arady(ifreq)*conjg(vstokes(4,2))
     &    +aradz(ifreq)*conjg(vstokes(4,3))

        stok1=
     &    apolr*conjg(apolr)+
     &    apoll*conjg(apoll)

        stok2=-stok1+
     &    2.*apolh*conjg(apolh)

        stok3=
     &    2.*apol45*conjg(apol45)-
     &    stok1

        stok4=
     &    apolr*conjg(apolr)-
     &    apoll*conjg(apoll)

        stokes(1,ifreq)=stok1
        stokes(2,ifreq)=stok2
        stokes(3,ifreq)=stok3
        stokes(4,ifreq)=stok4

      enddo !nphener

      powden=powden*pownor

      if (istatus.ge.0.and.ifail.ne.0) istatus=ifail

      return
      end
