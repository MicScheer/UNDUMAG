*CMZ :  3.05/20 31/10/2018  10.51.37  by  Michael Scheer
*CMZ :  3.05/04 04/07/2018  14.02.25  by  Michael Scheer
*CMZ :  3.05/00 02/05/2018  11.33.05  by  Michael Scheer
*CMZ :  3.04/00 01/03/2018  15.00.40  by  Michael Scheer
*CMZ :  3.02/04 11/03/2015  13.19.06  by  Michael Scheer
*CMZ :  3.01/04 20/05/2014  12.49.15  by  Michael Scheer
*CMZ :  2.70/00 12/11/2012  11.53.09  by  Michael Scheer
*CMZ :  2.68/05 06/09/2012  15.53.22  by  Michael Scheer
*CMZ :  2.68/04 03/09/2012  09.07.11  by  Michael Scheer
*CMZ :  2.68/03 27/08/2012  09.35.31  by  Michael Scheer
*-- Author :    Michael Scheer
c*******************************************************************************
      subroutine uradstep(xin,yin,zin,vx1,vy1,vz1,bxin,byin,bzin,
     &  efieldx,efieldy,efieldz,
     &  dtim,
     &  x2,y2,z2,vx2,vy2,vz2,vxp,vyp,vzp,gamma,icharge,
     &  ieneloss,dgamma)
c*******************************************************************************

c Author: Michael Scheer, Michael.Scheer@Helmholtz-Berlin.de

c NO WARRANTY

*KEEP,gplhint.
*KEND.

      implicit none

      double precision bbet1,vsenk1,vsenk2,tz,tz2,tz3,tz4,tz5,
     &  vsenkzyk,acc,
     &  efieldx,efieldy,efieldz,
     &  dk,z12,y12,vz12,vy12,vx12,xin,yin,zin

      double precision bx2,by2,bz2,bsq,bbet,bux,buy,buz,v0sq,
     &  vx1,vy1,vz1,
     &  v0bet,vpar,vparx,vpary,vparz,vparsq,vsenk,dtim,bxin,byin,bzin
      double precision x1n,y1n,z1n,x2n,y2n,z2n

      double precision x2,y2,z2,vx2,vy2,vz2,x1,y1,z1,vxp,vyp,vzp,zyk,
     &  gamma,sz,cz,zp,yp,dy,dz,brho
     &  ,dgamma

      double precision bmovecut,dgammao,v0,v12n(3),emomgev,pel(3),
     &  dpphoton(3)

      double precision erad1,cgam1,pdum,echarge1,emasskg1,emassg1,
     &  clight1,pi1

      save

      integer icharge,ieneloss,ical,icorrz,icorry,isbig

      data ical/0/

      data pi1/3.14159265358979d0/
      data emasskg1/9.10938188D-31/
      data emassg1/0.510998902D-3/
      data clight1/2.99792458d8/
      data echarge1/1.602176462D-19/

      data bmovecut/1.0d-6/

      if (ical.eq.0) then
        erad1=2.8179380D-15
        cgam1=4.0d0/3.0d0*pi1*erad1/emassg1**3
        pdum=cgam1/2.0d0/pi1*clight1*(clight1/1.0d9)**2*emassg1
        ical=1
      endif

      dgamma=0.0d0

      isbig=0
      x1=0.0d0
      y1=0.0d0
      z1=0.0d0

      if (icharge.gt.0) then
        bx2=-bxin
        by2=-byin
        bz2=-bzin
      else
        bx2=bxin
        by2=byin
        bz2=bzin
      endif

      if(dabs(bx2).lt.bmovecut.and.dabs(by2).lt.bmovecut
     &    .and.dabs(bz2).lt.bmovecut) then

        vxp=0.d0
        vyp=0.d0
        vzp=0.d0

        x2=x1+vx1*dtim
        y2=y1+vy1*dtim
        z2=z1+vz1*dtim

        vx2=vx1
        vy2=vy1
        vz2=vz1

        goto 999

      endif !b-cut

      bsq=bx2*bx2+by2*by2+bz2*bz2
      bbet=dsqrt(bsq)
      bbet1=1.0d0/bbet

      bux=bx2*bbet1
      buy=by2*bbet1
      buz=bz2*bbet1

c
c   Betrag von v0 paralell und senkrecht
c
      v0sq=vx1*vx1+vy1*vy1+vz1*vz1
      v0bet=dsqrt(v0sq)

c
c  vpar
c
      vpar=vx1*bux+vy1*buy+vz1*buz
      vparsq=vpar*vpar
      vparx=vpar*bux
      vpary=vpar*buy
      vparz=vpar*buz

c      vsenk2=(v0sq-vparsq)
      vsenk2=(v0bet-vpar)*(v0bet+vpar)

      if (vsenk2.le.0.0d0) then

c
c  Zeitableitung der Geschwindigkeit
c
        vxp=0.d0
        vyp=0.d0
        vzp=0.d0

c
c v(dtim),x(dtim) berechnen
c

        x2=x1+vx1*dtim
        y2=y1+vy1*dtim
        z2=z1+vz1*dtim

        vx2=vx1
        vy2=vy1
        vz2=vz1

        goto 999

      else    !(vsenk2.lt.0.0)

        vsenk=dsqrt(vsenk2)
        vsenk1=1.0d0/vsenk

c
c   vektor n1 berechnen
c

        x1n=(vx1-vpar*bux)*vsenk1
        y1n=(vy1-vpar*buy)*vsenk1
        z1n=(vz1-vpar*buz)*vsenk1

c
c  Vektor n2=(bux,buy,buz) kreuz n1
c

        x2n = buy*z1n - buz*y1n
        y2n = buz*x1n - bux*z1n
        z2n = bux*y1n - buy*x1n

c
c Zyklotronfrequenz
c
        zyk=(echarge1/(gamma*emasskg1))*bbet
c
c

        tz=zyk*dtim

        if (tz.le.0.03d0) then
          tz2=tz*tz
          tz3=tz2*tz
          tz4=tz3*tz
          tz5=tz4*tz
          cz=1.0d0-tz2/2.0d0+tz4/24.0d0
          sz=tz-tz3/6.0d0+tz5/120.0d0
        else
          cz=cos(tz)
          sz=sin(tz)
          isbig=1
        endif

c
c  Zeitableitung der Geschwindigkeit
c

cerror 4.7.2018        vxp=vsenk*zyk*x2n
cerror 4.7.2018        vyp=vsenk*zyk*y2n
cerror 4.7.2018        vzp=vsenk*zyk*z2n

c
c v(dtim),x(dtim) berechnen
c

        vx2=vparx + vsenk*(x1n*cz+x2n*sz)
        vy2=vpary + vsenk*(y1n*cz+y2n*sz)
        vz2=vparz + vsenk*(z1n*cz+z2n*sz)

c
c x(dtim) berechnen
c

        vsenkzyk=vsenk/zyk

        x2=x1+vsenkzyk*(x2n+x1n*sz-x2n*cz)+vparx*dtim
        y2=y1+vsenkzyk*(y2n+y1n*sz-y2n*cz)+vpary*dtim
        z2=z1+vsenkzyk*(z2n+z1n*sz-z2n*cz)+vparz*dtim

        emomgev=emassg1*dsqrt((gamma-1.0d0)*(gamma+1.0d0)) !GeV
        brho=emomgev*1.0d9/clight1

        if (ieneloss.ne.0) then
          dgamma=-pdum*bsq*vsenk2/v0sq*gamma**2*dtim
          if (ieneloss.eq.-1) then
            v0=sqrt(v0sq)
            v12n(1)=vx2/v0
            v12n(2)=vy2/v0
            v12n(3)=vz2/v0
            call uradphoton(v12n,gamma,bx2,by2,bz2,
     &        dgamma,dtim,dpphoton) !dgamma will be overwritten
            if (dgamma.ne.0.0d0) then
              emomgev=emassg1*dsqrt((gamma-1.0d0)*(gamma+1.0d0)) !Gev
              pel=emomgev*v12n+dpphoton
              dgamma=
     &          sqrt(1.0d0+(pel(1)**2+pel(2)**2+pel(3)**2)/emassg1**2)-
     &          gamma
              emomgev=sqrt(pel(1)**2+pel(2)**2+pel(3)**2)
              vx2=pel(1)/((gamma+dgamma)*emassg1)*clight1
              vy2=pel(2)/((gamma+dgamma)*emassg1)*clight1
              vz2=pel(3)/((gamma+dgamma)*emassg1)*clight1
            endif !dgamma
          endif !ieneloss .eq. -1
        endif !ieneloss

      endif   !(vsenk.lt.0.0)

      acc=icharge*echarge1/(gamma*emasskg1)
cerror 4.7.2018, due to error above, now here
      vx12=(vx1+vx2)/2.0d0
      vy12=(vy1+vy2)/2.0d0
      vz12=(vz1+vz2)/2.0d0

      vxp=acc*(vy12*bzin-vz12*byin)
      vyp=acc*(vz12*bxin-vx12*bzin)
      vzp=acc*(vx12*byin-vy12*bxin)

      if (isbig.eq.0) then

        dk=(clight1*dtim)**2/brho

        dy=abs(bz2*dk)
        y12=abs(y1-y2)
        icorry=0
        if (dy.ge.2.0d0*y12
     &      .or.gamma.gt.1.0d10
     &      ) then
          y2=y1+vy12*dtim
          icorry=1
        endif

        dz=abs(by2*dk)
        z12=abs(z1-z2)
        icorrz=0
        if (dz.ge.2.0d0*z12
     &      .or.gamma.gt.1.0d10
     &      ) then
          z2=z1+vz12*dtim
          icorrz=1
        endif

        if (icorry.eq.1.and.icorrz.eq.1) then
          x2=x1+vx1*dtim
        endif

      endif !(isbig.eq.0) then

999   continue

      dgammao=dgamma
      if (efieldx.ne.0.0d0.or.efieldy.ne.0.0d0.or.efieldz.ne.0.0d0) then
        call uradestep(icharge,x2,y2,z2,vx2,vy2,vz2,
     &    efieldx,efieldy,efieldz,dtim,gamma,dgamma)
        dgamma=dgammao+dgamma
        vxp=(vx2-vx1)/dtim
        vyp=(vy2-vy1)/dtim
        vzp=(vz2-vz1)/dtim
      endif

      x2=x2+xin
      y2=y2+yin
      z2=z2+zin

      return
      end
