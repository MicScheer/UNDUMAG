*CMZ :  2.04/20 20/09/2023  15.59.49  by  Michael Scheer
*CMZ :  2.04/16 11/09/2023  10.23.37  by  Michael Scheer
*CMZ :  2.04/08 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  17.21.35  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.33.07  by  Michael Scheer
*CMZ :  2.02/01 05/01/2022  11.09.38  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_cut_cyl(imag)

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      character(2048) cline

      double precision
     &  xp(8),yp(8),zp(8),phi,sp,cp,gcentot(3),
     &  z1,z2,z3,z4,y1,y2,x1,x2,x3,x4,x0,y0,z0,ro,ri,r,h,
     &  radin,radout,height,angle,xyz(3),dphi,dr,dh,gcen(3),
     &  xmint,xmaxt,ymint,ymaxt,zmint,zmaxt,
     &  xmn,xmx,ymn,ymx,zmn,zmx,
     &  xmin,xmax,ymin,ymax,zmin,zmax

      integer khull(8),kedge(4,2*8-2),kface(5*8),ir,iphi,ih,npoi,i,ivox,
     &  nhull,nedge,nface,kfacelast,ifailhull,imag,nr,nang,nh

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

      if (nmagcyl.le.0) return

      radin=t_magnets(imag)%size(1)
      radout=t_magnets(imag)%size(2)
      height=t_magnets(imag)%size(3)
      angle=t_magnets(imag)%cylphi

      t_magnets(imag)%yfracdiv=1.0d0
      t_magnets(imag)%zfracdiv=1.0d0

      gcentot=0.0d0

      if (coating.ne.0.0d0) then
        radin=radin+coating
        radout=radout-coating
        height=height-2.0d0*coating
        t_magnets(imag)%size(1)=radin
        t_magnets(imag)%size(2)=radout
        t_magnets(imag)%size(3)=height
      endif

      if (radin.lt.tiny) radin=tiny
      t_magnets(imag)%size(1)=radin

      xyz=t_magnets(imag)%xyz

      nr=t_magnets(imag)%nxdiv
      nang=t_magnets(imag)%nydiv
      nh=t_magnets(imag)%nzdiv

      allocate(t_magnets(imag)%t_voxels(nr*nang*nh))
      t_magnets(imag)%nvoxels=nr*nang*nh

      dphi=angle/nang*grarad1
      dr=(radout-radin)/nr
      dh=height/nh

      r=radin

      ivox=0
      r=radin+dr/2.0d0

      xmint=1.0e30
      xmaxt=-1.0e30
      ymint=1.0e30
      ymaxt=-1.0e30
      zmint=1.0e30
      zmaxt=-1.0e30

      do ir=1,nr
        h=-height/2.0d0+dh/2.0d0
        do ih=1,nh
          phi=-angle/2.0d0*grarad1+dphi/2.0d0
          do iphi=1,nang

            xmin=1.0e30
            xmax=-1.0e30
            ymin=1.0e30
            ymax=-1.0e30
            zmin=1.0e30
            zmax=-1.0e30

            ivox=ivox+1

            x0=r*sin(phi)
            y0=h
            z0=r*cos(phi)

            ri=r-dr/2.0d0
            ro=r+dr/2.0d0

            y1=y0-dh/2.0d0
            y2=y0+dh/2.0d0

            sp=sin(phi-dphi/2.0d0)
            cp=cos(phi-dphi/2.0d0)
            x1=ri*sp+coating*cp
            z1=ri*cp+coating*sp
            x2=ro*sp+coating*cp
            z2=ro*cp+coating*sp
            sp=sin(phi+dphi/2.0d0)
            cp=cos(phi+dphi/2.0d0)
            x3=ro*sp-coating*cp
            z3=ro*cp-coating*sp
            x4=ri*sp-coating*cp
            z4=ri*cp-coating*sp

            xp(1)=x1
            yp(1)=y1
            zp(1)=z1

            xp(2)=x2
            yp(2)=y1
            zp(2)=z2

            xp(3)=x3
            yp(3)=y1
            zp(3)=z3

            xp(4)=x4
            yp(4)=y1
            zp(4)=z4

            xp(5)=x1
            yp(5)=y2
            zp(5)=z1

            xp(6)=x2
            yp(6)=y2
            zp(6)=z2

            xp(7)=x3
            yp(7)=y2
            zp(7)=z3

            xp(8)=x4
            yp(8)=y2
            zp(8)=z4

            gcen=[sum(xp),sum(yp),sum(zp)]/8.0d0
            gcentot=gcentot+gcen

            xmn=minval(xp)-gcen(1)
            xmx=maxval(xp)-gcen(1)
            ymn=minval(yp)-gcen(2)
            ymx=maxval(yp)-gcen(2)
            zmn=minval(zp)-gcen(3)
            zmx=maxval(zp)-gcen(3)

            xmin=min(xmn,xmin)
            xmax=max(xmx,xmax)
            ymin=min(ymn,ymin)
            ymax=max(ymx,ymax)
            zmin=min(zmn,zmin)
            zmax=max(zmx,zmax)

            xmint=min(xmint,xmin)
            xmaxt=max(xmaxt,xmax)
            ymint=min(ymint,ymin)
            ymaxt=max(ymaxt,ymax)
            zmint=min(zmint,zmin)
            zmaxt=max(zmaxt,zmax)

            npoi=8

            call util_convex_hull_3d_overwrite(ivox,npoi,xp,yp,zp,
     &        khull,kedge,kface,
     &        nhull,nedge,nface,kfacelast,hulltiny,ifailhull)

            if (ifailhull.ne.0.or.nhull.lt.6) then
              write(lun6,*)"*** Error in clcmag_cut_cyl: Subroutine util_convex_hull_3d failed for ",
     &          trim(cline)
              stop
            endif

            allocate(t_magnets(imag)%t_voxels(ivox)%xhull(nhull))
            allocate(t_magnets(imag)%t_voxels(ivox)%yhull(nhull))
            allocate(t_magnets(imag)%t_voxels(ivox)%zhull(nhull))
            allocate(t_magnets(imag)%t_voxels(ivox)%khull(nhull))
            allocate(t_magnets(imag)%t_voxels(ivox)%lface(nface))
            allocate(t_magnets(imag)%t_voxels(ivox)%kface(5*4))
            allocate(t_magnets(imag)%t_voxels(ivox)%kedge(4,2*8-2))

            t_magnets(imag)%t_voxels(ivox)%nhull=nhull
            t_magnets(imag)%t_voxels(ivox)%khull=khull
            t_magnets(imag)%t_voxels(ivox)%nface=nface
            t_magnets(imag)%t_voxels(ivox)%kface=kface
            t_magnets(imag)%t_voxels(ivox)%kfacelast=kfacelast
            t_magnets(imag)%t_voxels(ivox)%nedge=nedge

            t_magnets(imag)%t_voxels(ivox)%IsBlock=t_magnets(imag)%IsBlock

            do i=1,nhull
              t_magnets(imag)%t_voxels(ivox)%xhull(i)=xp(i) - gcen(1) !+xyz(1)
              t_magnets(imag)%t_voxels(ivox)%yhull(i)=yp(i) - gcen(2) !+xyz(2)
              t_magnets(imag)%t_voxels(ivox)%zhull(i)=zp(i) - gcen(3) !+xyz(3)
            enddo

            phi=phi+dphi

            t_magnets(imag)%t_voxels(ivox)%ixdiv=ir
            t_magnets(imag)%t_voxels(ivox)%iydiv=iphi
            t_magnets(imag)%t_voxels(ivox)%izdiv=ih

            t_magnets(imag)%t_voxels(ivox)%gcen=gcen+xyz

            t_magnets(imag)%t_voxels(ivox)%xmin=xmin
            t_magnets(imag)%t_voxels(ivox)%xmax=xmax
            t_magnets(imag)%t_voxels(ivox)%ymin=ymin
            t_magnets(imag)%t_voxels(ivox)%ymax=ymax
            t_magnets(imag)%t_voxels(ivox)%zmin=zmin
            t_magnets(imag)%t_voxels(ivox)%zmax=zmax

            do i=1,nface
              t_magnets(imag)%t_voxels(ivox)%lface(i)=1+(i-1)*5
            enddo

          enddo !iphi

          h=h+dh

        enddo !ih

        r=r+dr

      enddo !ir

      gcentot=gcentot/dble(nr*nang*nh)
      t_magnets(imag)%gcen=gcentot+xyz

      !call util_break

      return
      end
