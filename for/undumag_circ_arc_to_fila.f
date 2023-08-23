*CMZ :  2.02/02 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/01 10/11/2021  10.13.19  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  14.01.51  by  Michael Scheer
*CMZ :  1.25/02 20/03/2018  18.02.01  by  Michael Scheer
*CMZ :  1.25/01 20/03/2018  15.53.53  by  Michael Scheer
*CMZ :  1.25/00 15/03/2018  21.53.23  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_circ_arc_to_fila(k,icoil)

      use undumagf90m

      use commandlinef90m

      implicit none

*KEEP,PHYCONparam,T=F77.
c-----------------------------------------------------------------------
c     phyconparam.cmn
c-----------------------------------------------------------------------

      complex*16, parameter :: zone1=(1.0d0,0.0d0), zi1=(0.0d0,1.0d0)

      complex*16, dimension(4,3), parameter ::
     &  vstokes=reshape([
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0, -0.70710678118654746d0),
     &  ( 0.0000000000000000d0, -0.70710678118654746d0),
     &  ( 0.70710678118654746d0, 0.0000000000000000d0),
     &  (-0.70710678118654746d0,-0.70710678118654746d0),
     &  ( 0.70710678118654746d0, 0.0000000000000000d0),
     &  (-0.70710678118654746d0, 0.0000000000000000d0),
     &  (-0.70710678118654746d0, 0.0000000000000000d0)
     &  ],[4,3])

c      vstokes(1,1)=( 0.0d0,        0.0d0)      !horizontal polarization
c      vstokes(1,2)=( 0.0d0,        0.0d0)
c      vstokes(1,3)=(-sqrt(1./2.),       -sqrt(1./2.))
c
c      vstokes(2,1)=( 0.0d0,        0.0d0)      !right handed polarization
c      vstokes(2,2)=( 0.0d0,       -sqrt(1./2.))
c      vstokes(2,3)=(+sqrt(1./2.),        0.0d0)
c
c      vstokes(3,1)=( 0.0d0,        0.0d0)      !left handed polarization
c      vstokes(3,2)=( 0.0d0,       -sqrt(1./2.))
c      vstokes(3,3)=(-sqrt(1./2.),        0.0d0)
c
c      vstokes(4,1)=( 0.0d0,        0.0d0)      !45 degree linear polarization
c      vstokes(4,2)=( sqrt(1./2.),        0.0d0)
c      vstokes(4,3)=(-sqrt(1./2.),        0.0d0)

      double precision, parameter ::
     &  HBAREV1=6.58211889D-16
     &  ,CLIGHT1=2.99792458D8
     &  ,EMASSKG1=9.10938188D-31
     &  ,EMASSE1=0.510998902D6
     &  ,EMASSG1=0.510998902D-3
     &  ,ECHARGE1=1.602176462D-19
     &  ,ERAD1=2.8179380D-15
     &  ,EPS01=8.854187817D-12
     &  ,PI1=3.141592653589793D0
     &  ,rmu04pi1=1.0D-7
     &  ,dnull1=0.0d0
     &  ,done1=1.0d0
     & ,HPLANCK1=6.626176D-34

      double precision, parameter ::
     & GRARAD1=PI1/180.0d0
     & ,RADGRA1=180.0d0/PI1
     & ,HBAR1=HBAREV1*ECHARGE1
     & ,WTOE1=CLIGHT1*HPLANCK1/ECHARGE1*1.0d9
     & ,CQ1=55.0d0/32.0d0/DSQRT(3.0D0)*HBAR1/EMASSKG1/CLIGHT1
     & ,CGAM1=4.0d0/3.0d0*PI1*ERAD1/EMASSG1**3
     & ,POL1CON1=8.0d0/5.0d0/DSQRT(3.0D0)
     & ,POL2CON1=8.0d0/5.0d0/DSQRT(3.0D0)/2.0d0/PI1/3600.0d0
     &  *EMASSKG1/HBAR1/ERAD1*EMASSG1**5
     & ,TWOPI1=2.0D0*PI1
     & ,HALFPI1=PI1/2.0D0
     & ,sqrttwopi1=sqrt(twopi1)
     & ,rmu01=4.0D0*PI1/1.0D7
     & ,alpha1=echarge1**2/(4.0d0*pi1*eps01*hbar1*clight1)
     & ,gaussn1=1.0d0/sqrt(twopi1)
     & ,cK934=ECHARGE1/(2.0d0*PI1*EMASSKG1*CLIGHT1)/100.0d0
     & ,powcon1=cgam1/2.0d0/pi1*clight1*(clight1/1.0d9)**2*emassg1
     &  ,gamma1=1.0d0/emassg1
     &  ,emom1=emasse1*dsqrt((gamma1-1.0d0)*(gamma1+1.0d0))
     &  ,rho1=emom1/clight1
     &  ,omegac1=1.5d0*gamma1**3*clight1/rho1
     &  ,ecdipev1=omegac1*hbar1/echarge1
     &  ,ecdipkev1=ecdipev1/1000.0d0

c-----------------------------------------------------------------------
c     end of phyconparam.cmn
c-----------------------------------------------------------------------
*KEND.

      double precision xx,yy,zz,curr,xc,yc,zc,rc,r,rmat(3,3),phi,
     &  phi0,dphi,dr,wx,wy,wz,sinphi1,cosphi1,sinphi2,cosphi2,cw,
     &  alpha,dalpha,x0,z0,sina,cosa,rwire

      integer k,icoil,iw,ir,ia,iphi,nr,nalpha,nphi,i,kolor

c The arc is created in the x-z-plane, than rotated and shifted

      iw=ncwires

      curr=carc(1,k)

      xc=carc(2,k)
      yc=carc(3,k)
      zc=carc(4,k)

      rc=carc(5,k)
      rwire=carc(6,k)
      if (rc.le.rwire) then
        write(lun6,*)"*** Error in undumag_circ_arc_to_fila: Radius of arc",k,
     &    " lower than radius of wire ***"
        stop
      endif

      phi=carc(7,k)*pi1/180.0d0

      nr=carc(8,k)
      nalpha=carc(9,k)
      nphi=carc(10,k)

      kolor=carc(11,k)

      rmat(1,1:3)=carc(12:14,k)
      rmat(2,1:3)=carc(15:17,k)
      rmat(3,1:3)=carc(18:20,k)

      dphi=phi/nphi
      dalpha=twopi1/nalpha
      dr=rwire/nr

      cw=curr/(nalpha*nr)

      phi0=-phi/2.0d0
      sinphi1=sin(phi0)
      cosphi1=cos(phi0)
      do iphi=1,nphi
        sinphi2=sin(phi0+dphi)
        cosphi2=cos(phi0+dphi)
        do ir=1,nr
          r=(ir-0.5d0)*dr
          do ia=1,nalpha
            alpha=(ia-1)*dalpha
            sina=sin(alpha)
            cosa=cos(alpha)
            iw=iw+1
            wire(1,iw)=5 !type circ-arc
            xx=rc+r*cosa
            yy=r*sina
            zz=0.0d0
            wire(2,iw)=cw
            wire(3,iw)=xx*cosphi1
            wire(4,iw)=yy
            wire(5,iw)=xx*sinphi1
            wire(6,iw)=xx*cosphi2
            wire(7,iw)=yy
            wire(8,iw)=xx*sinphi2
            wire(9,iw)=kolor
            wire(10,iw)=k ! arctrack number
            wire(11,iw)=icoil ! coil number
          enddo
        enddo
        phi0=phi0+dphi
        sinphi1=sinphi2
        cosphi1=cosphi2
      enddo

      !rotate and translate arc

      do i=ncwires+1,iw
        wx=wire(3,i)
        wy=wire(4,i)
        wz=wire(5,i)
        wire(3,i)=rmat(1,1)*wx+rmat(1,2)*wy+rmat(1,3)*wz+xc
        wire(4,i)=rmat(2,1)*wx+rmat(2,2)*wy+rmat(2,3)*wz+yc
        wire(5,i)=rmat(3,1)*wx+rmat(3,2)*wy+rmat(3,3)*wz+zc
        wx=wire(6,i)
        wy=wire(7,i)
        wz=wire(8,i)
        wire(6,i)=rmat(1,1)*wx+rmat(1,2)*wy+rmat(1,3)*wz+xc
        wire(7,i)=rmat(2,1)*wx+rmat(2,2)*wy+rmat(2,3)*wz+yc
        wire(8,i)=rmat(3,1)*wx+rmat(3,2)*wy+rmat(3,3)*wz+zc
      enddo

      ncwires=iw

      return
      end
