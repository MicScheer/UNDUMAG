*CMZ :  2.04/02 25/02/2023  17.13.46  by  Michael Scheer
*CMZ :  2.04/01 25/01/2023  06.16.23  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.19.49  by  Michael Scheer
*CMZ :  2.02/02 15/02/2022  16.00.05  by  Michael Scheer
*CMZ :  2.02/01 23/01/2022  14.52.10  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021

      subroutine clcmag_corn_to_vox(ncorn,corn,tvox,x0,y0,z0)

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision corn(3,2*ncornmax,2*nplanmax),x0,y0,z0
      double precision, dimension (:), allocatable :: xh,yh,zh,xhc,yhc,zhc

      double precision
     &  gcenv(3),xmin,xmax,ymin,ymax,zmin,zmax

      integer i,k,npoi,iplan,icorn,ncorn(nplanmax),
     &  ifailhull,nhull,nedge,nface,kfacelast

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      type(T_Voxel) :: tvox

      !call util_break

      npoi=0
      do iplan=1,nplanmax
        do icorn=1,ncorn(iplan)
          npoi=npoi+1
          xh(npoi)=corn(1,icorn,iplan)
          yh(npoi)=corn(2,icorn,iplan)
          zh(npoi)=corn(3,icorn,iplan)
        enddo
      enddo

      call util_convex_hull_3d_overwrite(npoi,
     &  xh,yh,zh,khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &  hulltiny,ifailhull)

      do i=1,nhull
        k=khull(i)
        xhc(i)=xh(k)
        yhc(i)=yh(k)
        zhc(i)=zh(k)
      enddo

      npoi=nhull
      call util_convex_hull_3d_overwrite(npoi,
     &  xhc,yhc,zhc,khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &  hulltiny,ifailhull)

      allocate(tvox%xhull(nhull))
      allocate(tvox%yhull(nhull))
      allocate(tvox%zhull(nhull))
      allocate(tvox%khull(nhull))
      allocate(tvox%kedge(4,nedge))
      allocate(tvox%kface(kfacelast))

      tvox%xyz=gcenv+[x0,y0,z0]
      tvox%gcen=gcenv+[x0,y0,z0]
      tvox%nhull=nhull
      tvox%khull=khull
      tvox%nface=nface
      tvox%kface=kface
      tvox%kfacelast=kfacelast
      tvox%nedge=nedge
      tvox%kedge=kedge

      xmin=1.0d30
      xmax=-1.0d30
      ymin=1.0d30
      ymax=-1.0d30
      zmin=1.0d30
      zmax=-1.0d30

      do i=1,nhull
        k=khull(i)
        tvox%xhull(i)=xhc(k)
        tvox%yhull(i)=yhc(k)
        tvox%zhull(i)=zhc(k)
        if (xhc(k).lt.xmin) xmin=xhc(k)
        if (xhc(k).gt.xmax) xmax=xhc(k)
        if (yhc(k).lt.ymin) ymin=yhc(k)
        if (yhc(k).gt.ymax) ymax=yhc(k)
        if (zhc(k).lt.zmin) zmin=zhc(k)
        if (zhc(k).gt.zmax) zmax=zhc(k)
      enddo

      tvox%size=[xmax-xmin,ymax-ymin,zmax-zmin]

      tvox%xmin=xmin
      tvox%xmax=xmax
      tvox%ymin=ymin
      tvox%ymax=ymax
      tvox%zmin=zmin
      tvox%zmax=zmax

      deallocate(xh,yh,zh,xhc,yhc,zhc,kedge,kface)

      !call util_break
      return
      end
