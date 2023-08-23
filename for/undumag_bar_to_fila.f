*CMZ :  2.02/02 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/01 10/11/2021  10.13.19  by  Michael Scheer
*CMZ :  2.00/03 20/04/2018  12.36.16  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  14.02.48  by  Michael Scheer
*CMZ :  1.25/03 23/03/2018  16.36.44  by  Michael Scheer
*CMZ :  1.25/02 22/03/2018  14.48.44  by  Michael Scheer
*CMZ :  1.25/01 16/03/2018  16.51.35  by  Michael Scheer
*CMZ :  1.25/00 15/03/2018  21.53.23  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_bar_to_fila(k,icoil)

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

      double precision w,h,xc,yc,zc,xx,yy,zz,rmat(3,3),x0,y0,z0,dz,dy,dl,
     &  curr,cw

      integer k,icoil,iw,nz,ny,kolor,iz,iy

      iw=ncwires

      curr=rectbar(1,k)

      x0=rectbar(2,k)
      y0=rectbar(3,k)
      z0=rectbar(4,k)

      dl=rectbar(5,k)
      w=rectbar(6,k)
      h=rectbar(7,k)
      nz=rectbar(8,k)
      ny=rectbar(9,k)
      kolor=rectbar(10,k)

      rmat(1,1:3)=rectbar(11:13,k)
      rmat(2,1:3)=rectbar(14:16,k)
      rmat(3,1:3)=rectbar(17:19,k)

      dy=h/ny
      dz=w/nz

      cw=curr/(ny*nz)

      xx=dl/2.0d0
      do iy=1,ny
        yy=-h/2.0d0+(iy-0.5d0)*dy
        do iz=1,nz
          iw=iw+1
          zz=-w/2.0d0+(iz-0.5d0)*dz
          wire(1,iw)=6 ! type bar
          wire(2,iw)=cw
          wire(3,iw)=-rmat(1,1)*xx+rmat(1,2)*yy+rmat(1,3)*zz+x0
          wire(4,iw)=-rmat(2,1)*xx+rmat(2,2)*yy+rmat(2,3)*zz+y0
          wire(5,iw)=-rmat(3,1)*xx+rmat(3,2)*yy+rmat(3,3)*zz+z0
          wire(6,iw)= rmat(1,1)*xx+rmat(1,2)*yy+rmat(1,3)*zz+x0
          wire(7,iw)= rmat(2,1)*xx+rmat(2,2)*yy+rmat(2,3)*zz+y0
          wire(8,iw)= rmat(3,1)*xx+rmat(3,2)*yy+rmat(3,3)*zz+z0
          wire(9,iw)=kolor
          wire(10,iw)=k ! number
          wire(11,iw)=icoil ! coil number
        enddo
      enddo

      ncwires=iw

      return
      end
