*CMZ :  2.02/00 26/10/2020  14.56.48  by  Michael Scheer
*CMZ :  2.01/02 27/04/2018  15.48.43  by  Michael Scheer
*CMZ :  1.22/00 05/07/2017  09.56.06  by  Michael Scheer
*CMZ :  1.20/01 22/06/2017  12.00.01  by  Michael Scheer
*CMZ :  1.20/00 22/06/2017  11.20.28  by  Michael Scheer
*CMZ :  1.15/02 31/03/2017  11.58.51  by  Michael Scheer
*CMZ :  1.13/01 07/03/2017  16.25.11  by  Michael Scheer
*CMZ :  1.11/06 22/02/2017  16.40.57  by  Michael Scheer
*CMZ :  1.11/03 16/01/2017  12.22.22  by  Michael Scheer
*CMZ :  1.10/02 30/11/2016  15.09.58  by  Michael Scheer
*CMZ :  0.00/06 16/06/2016  14.14.37  by  Michael Scheer
*CMZ :  0.00/00 20/04/2016  12.41.34  by  Michael Scheer
*CMZ :  1.17/14 13/04/2016  10.30.41  by  Michael Scheer
*CMZ :  0.99/07 16/02/2004  15.18.01  by  Michael Scheer
*CMZ :  0.99/03 12/02/2004  10.50.28  by  Michael Scheer
*CMZ :  0.99/02 12/02/2004  10.20.35  by  Michael Scheer
*CMZ :  0.99/01 11/02/2004  13.54.20  by  Michael Scheer
*CMZ :  0.99/00 29/01/2004  14.14.29  by  Michael Scheer
*CMZ :  0.00/08 23/01/2004  15.31.25  by  Michael Scheer
*CMZ :  0.00/06 14/01/2004  16.32.20  by  Michael Scheer
*CMZ :  0.00/05 23/12/2003  16.08.27  by  Michael Scheer
*CMZ :  0.00/04 19/12/2003  18.32.08  by  Michael Scheer
*-- Author :    Michael Scheer   19/12/2003
      subroutine undumag_bpeq(x1,x2,a,b,zin,qx,qy,qz,tiny,
     &  reverse,iwarn)

      use commandlinef90m

      implicit none

*KEEP,seqdebug.
      include 'seqdebug.cmn'
*KEND.

      double precision x1,x2,a,b,zin,z,tiny,twopi,pi,f1,f2,dum1,dum2
      double precision qx,qy,qz,xi1,xi2,abz2,a2z2b2,
     &  are1,aim1,are2,aim2,arei1,aimi1,arei2,aimi2,dphi,dphi1,dphi2,phi1,phi2,
     &  sdum,dum,xpi(5),reverse,x1r,x2r,y1,y2,a21,ab,z2,a2,b2,az,a2z2,
     &  arem,aimm,arep,aimp,phim1,phip1,phim2,phip2,xm1,xp1,xm2,xp2

      double precision ra21,rho1,rho2,x2rxz,x1rxz,
     &  arg1,arg2,arg31,arg32,arg4

      integer iwarn
      parameter (pi=3.1415926535897932385d0)
      parameter (twopi=6.2831853071795864769d0)
c      if (zin.lt.0.d0) write(lunwarn,*),'neg. z: ',zin

      iwarn=0

      xm1=1.d30
      xp1=1.d30
      xm2=1.d30
      xp2=1.d30
      dphi1=0.d0
      dphi2=0.d0
      iwarn=0

      if (abs(zin).lt.tiny) then
        z=sign(tiny,zin)
        iwarn=1
      else
        z=zin
      endif

      if (x1.gt.x2) then
        x1r=x2
        x2r=x1
        reverse=-1.d0
      else
        x1r=x1
        x2r=x2
        reverse=1.d0
      endif

c-------------------------------------------------------------------------

      y1=a*x1+b
      y2=a*x2+b
      z2=z*z

      ab=a*b
      a2=a*a
      b2=b*b
      az=a*z
      a2z2=a2*z2
      abz2=ab*z2
      a2z2b2=a2z2+b2

      a21=1.d0+a2
      ra21=Sqrt(a21)

      rho1=Sqrt(x1**2+y1**2+z2)
      rho2=Sqrt(x2**2+y2**2+z2)

      arg1=(ab+x1*a21)/ra21+rho1
      arg2=(ab+x2*a21)/ra21+rho2

      arg31=((b*(y1+rho1)+z2)**2 + (z*(a21*x1+a*(b+rho1)))**2)/(x1**2+z**2)
      arg32=((b*(y2+rho2)+z2)**2 + (z*(a21*x2+a*(b+rho2)))**2)/(x2**2+z**2)

      if (
     &    ra21.eq.0.d0.or.arg2.eq.0.d0.or.arg32.eq.0.d0
     &    .or.sign(1.d0,arg1).ne.sign(1.d0,arg2)
     &    .or.sign(1.d0,arg31).ne.sign(1.d0,arg32)
     &   ) then
        iwarn=10
        goto 9999
      endif

      arg4=Log(arg1/arg2)/ra21

      qx=Log(arg31/arg32)/2.d0-a*arg4

c-------------------------------------------------------------------------

      x2rxz=x2+Sqrt(x2**2+z2)
      x1rxz=x1+Sqrt(x1**2+z2)

      if (x2rxz.eq.0.0d0.or.x1rxz.eq.0.0d0.or.sign(1.d0,x2rxz).ne.sign(1.d0,x1rxz)) then
        iwarn=11
        goto 9999
      endif

      qy=Log(x2rxz/x1rxz)+arg4

c-------------------------------------------------------------------------

      f1=((az+b)*(az-b))**2*a2z2b2-4.0d0*abz2**2
      f2=a21*z2+b2;

      sdum=f1*f2

      dum=(((1.0d0+3.0d0*a21)*z2+b2)*a2z2+(a2z2-b2)*b2)*b2

      if (sdum.gt.0.d0.and.dum.ne.0.d0) then

        dum1=2.0d0*sqrt(sdum)*abs(ab)*z2
        dum2=abz2*a2z2b2**2

        xi1=(+dum1-dum2)/dum
        xi2=(-dum1-dum2)/dum

      else

        xi1=-1.d30
        xi2=-1.d30

      endif

c--------------

      if (xi1.gt.xi2) then
        xpi(1)=xi1
        xi1=xi2
        xi2=xpi(1)
      else if (xi1.eq.xi2) then
        xi2=-1.d30
      endif

c On the way from x1 to x2, the atan2 may be not continuous. So we check,
c xi1, and xi2 and add +/- pi, respectivly

      if (xi1.lt.x1r.or.xi1.gt.x2r) then
        xi1=-1.d30
      endif

      if (xi2.lt.x1r.or.xi2.gt.x2r) then
        xi2=-1.d30
      endif

c----------

      if (xi1.ne.-1.d30) then

c is it real null or pi?

        xm1=xi1-tiny
        xp1=xi1+tiny

        call areim(xm1,a,b,z,arem,aimm)
        call areim(xi1,a,b,z,arei1,aimi1)
        call areim(xp1,a,b,z,arep,aimp)

        if (arem.eq.0.d0.and.aimm.eq.0.d0) then
c          write(lun6,*)'*** Error 21 in undumag_bpeq: 0/0' !increase tiny...
c          stop
          iwarn=21
          goto 9999
        endif

        if (arep.eq.0.d0.and.aimp.eq.0.d0) then
c          write(lun6,*)'*** Error 22 in undumag_bpeq: 0/0' !increase tiny...
c          stop
          iwarn=21
          goto 9999
        endif

        phim1=atan2(aimm,arem)
        phip1=atan2(aimp,arep)

        if (sign(1.d0,phim1).eq.sign(1.d0,phip1)) then
          xi1=-1.d30
        else
          dphi1=pi*nint((phim1-phip1)/pi)
        endif
        if (arei1.gt.tiny) then
          xi1=-1.d30
        endif

      endif !xi1.ne.-1.d30

c--------------

      if (xi2.ne.-1.d30) then

        xm2=xi2-tiny
        xp2=xi2+tiny

        call areim(xm2,a,b,z,arem,aimm)
        call areim(xi2,a,b,z,arei2,aimi2)
        call areim(xp2,a,b,z,arep,aimp)

        if (arem.eq.0.d0.and.aimm.eq.0.d0) then
          write(lun6,*)'*** Error 23 in undumag_bpeq: 0/0' !increase tiny...
c          stop
          iwarn=23
          goto 9999
        endif

        if (arep.eq.0.d0.and.aimp.eq.0.d0) then
          write(lun6,*)'*** Error 24 in undumag_bpeq: 0/0' !increase tiny...
c          stop
          iwarn=24
          goto 9999
c          stop '*** Error in undumag_bpeq: 0/0'
        endif

        phim2=atan2(aimm,arem)
        phip2=atan2(aimp,arep)

        if (sign(1.d0,phim2).eq.sign(1.d0,phip2)) then
          xi2=-1.d30
        else
          dphi2=pi*nint((phim2-phip2)/pi)
        endif

        if (arei2.eq.0.d0.and.aimi2.eq.0.d0) then
c          write(lun6,*)'*** Error 25 in undumag_bpeq: 0/0' !increase tiny...
c          stop
          iwarn=25
          goto 9999
c          stop '*** Error in undumag_bpeq: 0/0'
        endif


        if (arei2.gt.tiny) then
          xi2=-1.d30
        endif

      endif !xi2.ne.-1.d30

      call areim(x1r,a,b,z,are1,aim1)

      if (are1.eq.0.d0.and.aim1.eq.0.d0) then
        iwarn=4
        phi1=0.d0
      else
        phi1=atan2(aim1,are1)
      endif


      call areim(x2r,a,b,z,are2,aim2)

      if (are2.eq.0.d0.and.aim2.eq.0.d0) then
        iwarn=5
        phi2=0.d0
      else
        phi2=atan2(aim2,are2)
      endif


      dphi=dphi1+dphi2

      if (abs(dphi).gt.twopi) then

c probably two nulls detected due to numerical problems, but actually only one


        if (dphi.gt.twopi) dphi=dphi-twopi
        if (dphi.lt.-twopi) dphi=dphi+twopi

        iwarn=6

      endif

      dphi=phi2-phi1+dphi
      if (dphi.gt.twopi) then
        dphi=dphi-twopi
      else if (dphi.lt.-twopi) then
        dphi=dphi+twopi
      endif

      qz=-reverse*dphi

9999  continue

c      if (iseqdebug.ne.0.and.iwarn.ne.0.and.iwarn.ne.1) then
c        write(lun6,*)x1,x2,a,b,zin,qx,qy,qz,dphi,reverse,iwarn
c        iseqdebug=2
c      endif

      return
      end
