*CMZ :  2.04/16 11/09/2023  10.23.37  by  Michael Scheer
*CMZ :  2.04/05 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  17.13.46  by  Michael Scheer
*CMZ :  2.04/01 21/01/2023  11.47.09  by  Michael Scheer
*CMZ :  2.04/00 17/01/2023  09.24.20  by  Michael Scheer
*CMZ :  2.03/00 22/08/2022  12.31.51  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  15.17.16  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_update_magnet(tmag)

      ! To update magnet if points corners have changed

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

      Type(T_Magnet) tmag

      double precision, dimension (:), allocatable :: x,y,z,xc,yc,zc
      double precision gcen(3)

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      integer i,m,npoi,nface,nedge,kfacelast,nhull
      integer ifailhull

      npoi=tmag%nhull

      allocate(
     &  x(npoi),y(npoi),z(npoi),
     &  xc(npoi),yc(npoi),zc(npoi),
     &  kface((npoi+1)*npoi),kedge(4,2*npoi-2),khull(npoi))

      x=tmag%xhull
      y=tmag%yhull
      z=tmag%zhull

      call util_convex_hull_3d_overwrite(tmag%kmag,npoi,x,y,z,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,hulltiny,ifailhull)

      if (ifailhull.ne.0.or.nhull.lt.4) then
        write(lun6,*)"*** Error in clcmag_update_magnet: Subroutine util_convex_hull_3d failed for ",
     &    trim(tmag%cnam)
        stop
      endif

      do i=1,nhull
        m=khull(i)
        xc(i)=x(m)
        yc(i)=y(m)
        zc(i)=z(m)
      enddo

      call util_convex_hull_3d_overwrite(tmag%kmag,nhull,xc,yc,zc,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,hulltiny,ifailhull)

      deallocate(tmag%xhull0,tmag%yhull0,tmag%zhull0,
     &  tmag%xhull,tmag%yhull,tmag%zhull,
     &  tmag%kedge,tmag%kface,tmag%khull)

      allocate(
     &  tmag%xhull0(nhull),tmag%yhull0(nhull),tmag%zhull0(nhull),
     &  tmag%xhull(nhull),tmag%yhull(nhull),tmag%zhull(nhull),
     &  tmag%kface(kfacelast),tmag%kedge(4,nedge),tmag%khull(nhull))

      tmag%nhull=nhull

      gcen=0.0d0
      do i=1,npoi
        m=khull(i)
        tmag%xhull0(i)=xc(m)
        tmag%yhull0(i)=yc(m)
        tmag%zhull0(i)=zc(m)
        gcen=gcen+[xc(m),yc(m),zc(m)]
      enddo

      gcen=gcen/npoi
      tmag%gcen=gcen

      do i=1,npoi
        m=khull(i)
        tmag%xhull(i)=xc(m)-gcen(1)
        tmag%yhull(i)=yc(m)-gcen(2)
        tmag%zhull(i)=zc(m)-gcen(3)
      enddo

      tmag%khull(1:npoi)=khull(1:npoi)

      tmag%kedge(:,1:nedge)=kedge(:,1:nedge)
      tmag%nedge=nedge
      tmag%kedge(:,1:nedge)=kedge(:,1:nedge)
      tmag%nface=nface
      tmag%kfacelast=kfacelast
      tmag%kface(1:kfacelast)=kface(1:kfacelast)

      deallocate(x,y,z,xc,yc,zc,kface,kedge,khull)

      return
      end
