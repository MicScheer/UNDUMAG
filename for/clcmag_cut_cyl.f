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
     &  xp(8),yp(8),zp(8),xpc(8),ypc(8),zpc(8),phi,sp,cp,
     &  z1,z2,z3,z4,y1,y2,x1,x2,x3,x4,x0,y0,z0,ro,ri,r,h,
     &  radin,radout,height,angle,xyz(3),dphi,dr,dh

      integer khull(8),kedge(4,2*8-2),kface(5*8),ir,iphi,ih,npoi,i,k,ivox,
     &  nhull,nedge,nface,kfacelast,ifailhull,imag,nr,nang,nh

*KEEP,phyconparam.
      include 'phyconparam.cmn'
*KEND.

      if (nmagcyl.le.0) return

      radin=t_magnets(imag)%size(1)
      radout=t_magnets(imag)%size(2)
      height=t_magnets(imag)%size(3)
      angle=t_magnets(imag)%cylphi

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
      do ir=1,nr
        h=-height/2.0d0+dh/2.0d0
        do ih=1,nh
          phi=-angle/2.0d0*grarad1+dphi/2.0d0
          do iphi=1,nang

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

            npoi=8

            call util_convex_hull_3d_overwrite(npoi,xp,yp,zp,
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
            allocate(t_magnets(imag)%t_voxels(ivox)%kface(5*4))
            allocate(t_magnets(imag)%t_voxels(ivox)%kedge(4,2*8-2))

            t_magnets(imag)%t_voxels(ivox)%nhull=nhull
            t_magnets(imag)%t_voxels(ivox)%khull=khull
            t_magnets(imag)%t_voxels(ivox)%nface=nface
            t_magnets(imag)%t_voxels(ivox)%kface=kface
            t_magnets(imag)%t_voxels(ivox)%kfacelast=kfacelast
            t_magnets(imag)%t_voxels(ivox)%nedge=nedge

            do i=1,nhull
              t_magnets(imag)%t_voxels(ivox)%xhull(i)=xp(i)+xyz(1)
              t_magnets(imag)%t_voxels(ivox)%yhull(i)=yp(i)+xyz(2)
              t_magnets(imag)%t_voxels(ivox)%zhull(i)=zp(i)+xyz(3)
            enddo

          enddo !iphi

          h=h+dh

        enddo !ih

        r=r+dr

      enddo !ir

      return
      end
