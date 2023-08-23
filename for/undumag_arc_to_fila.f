*CMZ :  2.02/02 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/01 10/11/2021  10.13.19  by  Michael Scheer
*CMZ :  2.00/03 23/04/2018  13.28.57  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  14.00.37  by  Michael Scheer
*CMZ :  1.25/01 20/03/2018  15.53.53  by  Michael Scheer
*CMZ :  1.25/00 15/03/2018  21.53.23  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_arc_to_fila(k,icoil)

      use undumagf90m

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

      double precision xx,yy,zz,curr,xc,yc,zc,rc,ri,ro,r,w,h,rmat(3,3),phi,
     &  phi0,dphi,dy,dr,wx,wy,wz,sinphi1,cosphi1,sinphi2,cosphi2,cw

      integer k,icoil,iw,ir,iy,iphi,nr,ny,nphi,i,kolor

c The arc is created in the x-z-plane, than rotated and shifted

      iw=ncwires

      curr=arc(1,k)

      xc=arc(2,k)
      yc=arc(3,k)
      zc=arc(4,k)

      rc=arc(5,k)

      w=arc(6,k)
      h=arc(7,k)
      phi=arc(8,k)*pi1/180.0d0

      nr=arc(9,k)
      ny=arc(11,k)
      nphi=arc(10,k)

      kolor=arc(12,k)

      rmat(1,1:3)=arc(13:15,k)
      rmat(2,1:3)=arc(16:18,k)
      rmat(3,1:3)=arc(19:21,k)

      dphi=phi/nphi
      dy=h/ny
      dr=w/nr

      ri=rc-w/2.0d0
      ro=ri+w

      cw=curr/(ny*nr)

      phi0=-phi/2.0d0
      sinphi1=sin(phi0)
      cosphi1=cos(phi0)
      do iphi=1,nphi
        sinphi2=sin(phi0+dphi)
        cosphi2=cos(phi0+dphi)
        do iy=1,ny
          yy=-h/2.0d0+(iy-0.5d0)*dy
          do ir=1,nr
            r=ri+(ir-0.5d0)*dr
            iw=iw+1
            wire(1,iw)=4 !type arc
            wire(2,iw)=cw
            xx=r*cosphi1
            zz=r*sinphi1
            wire(3,iw)=xx
            wire(4,iw)=yy
            wire(5,iw)=zz
            xx=r*cosphi2
            zz=r*sinphi2
            wire(6,iw)=xx
            wire(7,iw)=yy
            wire(8,iw)=zz
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
