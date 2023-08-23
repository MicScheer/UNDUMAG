*CMZ :  2.04/05 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  17.13.46  by  Michael Scheer
*CMZ :  2.04/01 22/01/2023  13.04.45  by  Michael Scheer
*CMZ :  2.04/00 17/01/2023  09.24.20  by  Michael Scheer
*CMZ :  2.03/00 22/08/2022  12.31.51  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  15.17.16  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_hull_to_mag(imag)

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

*KEEP,grarad,T=F77.
c-----------------------------------------------------------------------
c     grarad.cmn
c-----------------------------------------------------------------------
      double precision, parameter ::
     &  PI1=3.141592653589793D0,
     &  TWOPI1=2.0D0*PI1,HALFPI1=PI1/2.0D0,
     &  GRARAD1=PI1/180.0d0,RADGRA1=180.0d0/PI1
*KEND.

      double precision, dimension (:), allocatable :: xp,yp,zp,xpc,ypc,zpc
      double precision gcen(3)

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      integer i,npoi,nhull,nface,nedge,kfacelast,ifailhull,imag
      integer :: iallo=0

      save iallo

      ! To make indices of hull and faces consistent
      ! The hull must already be calculated!

      if (iallo.eq.0) then
        allocate(
     &    xp(ncornmax_t),yp(ncornmax_t),zp(ncornmax_t),
     &    xpc(ncornmax_t),ypc(ncornmax_t),zpc(ncornmax_t),
     &    kface((ncornmax_t+1)*ncornmax_t),kedge(4,2*ncornmax_t-2),khull(ncornmax_t))
        iallo=1
      endif

      do i=1,nhull
        xp(i)=t_magnets(imag)%xhull(i)
        yp(i)=t_magnets(imag)%yhull(i)
        zp(i)=t_magnets(imag)%zhull(i)
      enddo

      call util_convex_hull_3d_overwrite(npoi,xp,yp,zp,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,hulltiny,ifailhull)

      if (ifailhull.ne.0.or.nhull.lt.4) then
        write(lun6,*)"*** Error in clcmag_hull_to_mag: Subroutine util_convex_hull_3d failed for ",
     &    trim(t_magnets(imag)%cnam)
        stop
      endif

      t_magnets(imag)%nhull=nhull
      t_magnets(imag)%khull(1:npoi)=khull(1:npoi)

      gcen=0.0d0
      do i=1,npoi
        t_magnets(imag)%xhull(i)=xp(i)
        t_magnets(imag)%yhull(i)=yp(i)
        t_magnets(imag)%zhull(i)=zp(i)
        gcen=gcen+[xp(i),yp(i),zp(i)]
      enddo

      gcen=gcen/npoi
      t_magnets(imag)%gcen=gcen

      t_magnets(imag)%kedge(:,1:nedge)=kedge(:,1:nedge)
      t_magnets(imag)%nedge=nedge
      t_magnets(imag)%kedge(:,1:nedge)=kedge(:,1:nedge)
      t_magnets(imag)%nface=nface
      t_magnets(imag)%kfacelast=kfacelast
      t_magnets(imag)%kface(1:kfacelast)=kface(1:kfacelast)

      return
      end
