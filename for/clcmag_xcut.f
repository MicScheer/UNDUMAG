*CMZ :  2.04/22 25/09/2023  12.27.21  by  Michael Scheer
*CMZ :  2.04/16 11/09/2023  10.23.37  by  Michael Scheer
*CMZ :  2.04/14 05/09/2023  13.54.14  by  Michael Scheer
*CMZ :  2.04/03 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  12.14.18  by  Michael Scheer
*CMZ :  2.04/01 13/02/2023  13.19.45  by  Michael Scheer
*CMZ :  2.04/00 16/01/2023  15.35.00  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.19.49  by  Michael Scheer
*CMZ :  2.02/02 15/02/2022  15.33.18  by  Michael Scheer
*CMZ :  2.02/01 19/01/2022  12.18.14  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_xcut(imag)

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      Type(t_voxel) tvox

      character(128) ctype

      double precision, dimension (:,:,:), allocatable :: corn1,corn2
      double precision, dimension (:), allocatable :: xh,yh,zh,xhc,yhc,zhc

      double precision x,y,z,
     &  x02(2),y02(2),z02(2),xdivmin,xdivmax,ydivmin,ydivmax,zdivmin,zdivmax,
     &  xdiv,dxdiv,gcen(3),xyz(3),xmin,xmax,ymin,ymax,zmin,zmax,dydiv,ydiv,
     &  zdiv,dzdiv,vol,xvolmag

      integer, dimension (:), allocatable :: ncorn1,ncorn2

      integer i,j,k,l,n,imag,ip,npoi,iplan,icorn,nxdiv,nydiv,nzdiv,ix,kx

      integer :: idebug=0,
     &  ifailhull,ifail,nhull,nedge,nface,kfacelast

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      type(T_Magnet) :: tmag

      if (idebug.gt.0) call util_break


      xmin=1.0d30
      xmax=-1.0d30
      ymin=1.0d30
      ymax=-1.0d30
      zmin=1.0d30
      zmax=-1.0d30

      do k=1,t_magnets(imag)%nhull
        if (t_magnets(imag)%xhull(k).lt.xmin) xmin=t_magnets(imag)%xhull(k)
        if (t_magnets(imag)%xhull(k).gt.xmax) xmax=t_magnets(imag)%xhull(k)
        if (t_magnets(imag)%yhull(k).lt.ymin) ymin=t_magnets(imag)%yhull(k)
        if (t_magnets(imag)%yhull(k).gt.ymax) ymax=t_magnets(imag)%yhull(k)
        if (t_magnets(imag)%zhull(k).lt.zmin) zmin=t_magnets(imag)%zhull(k)
        if (t_magnets(imag)%zhull(k).gt.zmax) zmax=t_magnets(imag)%zhull(k)
      enddo

      t_magnets(imag)%xmin=xmin
      t_magnets(imag)%xmax=xmax
      t_magnets(imag)%ymin=ymin
      t_magnets(imag)%ymax=ymax
      t_magnets(imag)%zmin=zmin
      t_magnets(imag)%zmax=zmax

      t_magnets(imag)%size(1)=xmax-xmin
      t_magnets(imag)%size(2)=ymax-ymin
      t_magnets(imag)%size(3)=zmax-zmin

      allocate(corn1(3,2*ncornmax,2*nplanmax),corn2(3,2*ncornmax,2*nplanmax))
      allocate(ncorn1(2*nplanmax),ncorn2(2*nplanmax))

      allocate(
     &  xh(2*ncornmax*nplanmax),yh(2*ncornmax*nplanmax),zh(2*ncornmax*nplanmax),
     &  xhc(2*ncornmax*nplanmax),yhc(2*ncornmax*nplanmax),zhc(2*ncornmax*nplanmax))

      allocate(khull(2*ncornmax*nplanmax),kedge(4,2*ncornmax*nplanmax-2),
     &  kface(5*ncornmax*nplanmax))

      t_magnets(imag)%mxdiv=0

      if (t_magnets(imag)%nxdiv.gt.0) then
        t_magnets(imag)%dxdiv=(t_magnets(imag)%xmax-t_magnets(imag)%xmin)/
     &    t_magnets(imag)%nxdiv
c        t_magnets(imag)%dxdiv=t_magnets(imag)%size(1)/t_magnets(imag)%nxdiv
      endif

      tmag=t_magnets(imag)

      gcen=tmag%gcen

      !call util_break

      xyz=tmag%xyz

      nxdiv=tmag%nxdiv
      nydiv=tmag%nydiv
      nzdiv=tmag%nzdiv

      xdivmin=tmag%xmin
      xdivmax=tmag%xmax
      dxdiv=tmag%dxdiv
      xdiv=xdivmin

      ydivmin=tmag%ymin
      ydivmax=tmag%ymax
      dydiv=tmag%dydiv

      ydiv=ydivmin

      zdivmin=tmag%zmin
      zdivmax=tmag%zmax
      dzdiv=tmag%dzdiv

      zdiv=zdivmin

      ctype=tmag%ctype

      if (nxdiv.eq.1) then

        nhull=tmag%nhull
        khull=tmag%khull
        nedge=tmag%nedge
        nface=tmag%nface
        kedge=tmag%kedge
        kface=tmag%kface
        kfacelast=tmag%kfacelast

        allocate(t_magnets(imag)%t_xcuts(1)%xhull(nhull))
        allocate(t_magnets(imag)%t_xcuts(1)%yhull(nhull))
        allocate(t_magnets(imag)%t_xcuts(1)%zhull(nhull))
        allocate(t_magnets(imag)%t_xcuts(1)%khull(nhull))
        allocate(t_magnets(imag)%t_xcuts(1)%kedge(4,nedge))
        allocate(t_magnets(imag)%t_xcuts(1)%kface(kfacelast))

        t_magnets(imag)%t_xcuts(1)%nhull=nhull
        t_magnets(imag)%t_xcuts(1)%khull=khull
        t_magnets(imag)%t_xcuts(1)%nedge=nedge
        t_magnets(imag)%t_xcuts(1)%kedge=kedge
        t_magnets(imag)%t_xcuts(1)%nface=nface
        t_magnets(imag)%t_xcuts(1)%kface=kface
        t_magnets(imag)%t_xcuts(1)%kfacelast=kfacelast

        t_magnets(imag)%t_xcuts(1)%size=tmag%size
        t_magnets(imag)%t_xcuts(1)%xmin=tmag%xmin
        t_magnets(imag)%t_xcuts(1)%xmax=tmag%xmax
        t_magnets(imag)%t_xcuts(1)%ymin=tmag%ymin
        t_magnets(imag)%t_xcuts(1)%ymax=tmag%ymax
        t_magnets(imag)%t_xcuts(1)%zmin=tmag%zmin
        t_magnets(imag)%t_xcuts(1)%zmax=tmag%zmax

        t_magnets(imag)%t_xcuts(1)%xyz=gcen
        t_magnets(imag)%t_xcuts(1)%gcen=gcen
        t_magnets(imag)%t_xcuts(1)%ixdiv=1
        t_magnets(imag)%t_xcuts(1)%volume=t_magnets(imag)%volume

        do i=1,nhull
          t_magnets(imag)%t_xcuts(1)%xhull(i)=tmag%xhull(i)
          t_magnets(imag)%t_xcuts(1)%yhull(i)=tmag%yhull(i)
          t_magnets(imag)%t_xcuts(1)%zhull(i)=tmag%zhull(i)
        enddo

      else !nxdiv.eq.1

        l=0
        ncorn1=0
        ncorn2=0

        xmin=1.0d30
        xmax=-1.0d30

        do i=1,tmag%nface

          l=l+1

          n=tmag%kface(l)
          ncorn1(i)=n

          do j=1,n
            l=l+1
            ip=tmag%kface(l)
            x=tmag%xhull(ip)
            y=tmag%yhull(ip)
            z=tmag%zhull(ip)
            corn1(1,j,i)=x
            corn1(2,j,i)=y
            corn1(3,j,i)=z
            if (x.lt.xmin) xmin=x
            if (x.gt.xmax) xmax=x
          enddo !npoi

          ncorn1(i)=ncorn1(i)+1
          corn1(:,n+1,i)=corn1(:,1,i) ! for undumag_cut_magnet,
          ! the plane must be closed

        enddo !nface

        x02(2)=0.0d0
        y02(2)=0.0d0
        z02(2)=0.0d0

        do ix=1,nxdiv-1

          xdiv=xdiv+dxdiv

          x02(1)=x02(2)
          y02(1)=y02(2)
          z02(1)=z02(2)

          call undumag_cut_magnet(imag,x02,y02,z02,
     &      2*nplanmax,2*ncornmax,
     &      ncorn1,corn1,ncorn2,corn2
     &      ,1,xdiv,hulltiny,ifail)

          if (ifail.ne.0) then
            write(lun6,*)"*** Error in clcmag_xcut: Bad return from undumag_cut_magnet, magnet, ifail, mag, ixdiv, xdiv:",
     &        trim(tmag%cnam),ifail,imag,ix,xdiv
            stop
          endif

          if (ncorn2(1).eq.0) then
            write(lun6,*) '*** Error in clcmag_xcut: No x-Cut for magnet ',tmag%cnam
            stop
          else

            xmin=1.0d30
            xmax=-1.0d30

            npoi=0
            do iplan=1,nplanmax
              do icorn=1,ncorn1(iplan)
                npoi=npoi+1
                xh(npoi)=corn1(1,icorn,iplan)
                yh(npoi)=corn1(2,icorn,iplan)
                zh(npoi)=corn1(3,icorn,iplan)
                x=xh(npoi)
                if (x.lt.xmin) xmin=x
                if (x.gt.xmax) xmax=x
              enddo
            enddo

            !print*,"nachher,xmin,xmax:)",xmin+x02(1),xmax+x02(1)
            !print*,x02
            !stop

            call util_convex_hull_3d_overwrite(imag,npoi,
     &        xh,yh,zh,khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &        hulltiny,ifailhull)

            allocate(t_magnets(imag)%t_xcuts(ix)%xhull(nhull))
            allocate(t_magnets(imag)%t_xcuts(ix)%yhull(nhull))
            allocate(t_magnets(imag)%t_xcuts(ix)%zhull(nhull))

            t_magnets(imag)%mxdiv=t_magnets(imag)%mxdiv+1

            t_magnets(imag)%t_xcuts(ix)%xyz=gcen+[x02(1),y02(1),z02(1)]
            t_magnets(imag)%t_xcuts(ix)%gcen=gcen+[x02(1),y02(1),z02(1)]
            t_magnets(imag)%t_xcuts(ix)%nhull=nhull
            t_magnets(imag)%t_xcuts(ix)%khull=khull
            t_magnets(imag)%t_xcuts(ix)%nface=nface
            t_magnets(imag)%t_xcuts(ix)%kface=kface
            t_magnets(imag)%t_xcuts(ix)%kfacelast=kfacelast
            t_magnets(imag)%t_xcuts(ix)%nedge=nedge
            t_magnets(imag)%t_xcuts(ix)%kedge=kedge
            t_magnets(imag)%t_xcuts(ix)%ixdiv=ix

            xmin=1.0d30
            xmax=-1.0d30
            ymin=1.0d30
            ymax=-1.0d30
            zmin=1.0d30
            zmax=-1.0d30

            do i=1,nhull
              t_magnets(imag)%t_xcuts(ix)%xhull(i)=xh(i)
              t_magnets(imag)%t_xcuts(ix)%yhull(i)=yh(i)
              t_magnets(imag)%t_xcuts(ix)%zhull(i)=zh(i)
              if (xh(i).lt.xmin) xmin=xh(i)
              if (xh(i).gt.xmax) xmax=xh(i)
              if (yh(i).lt.ymin) ymin=yh(i)
              if (yh(i).gt.ymax) ymax=yh(i)
              if (zh(i).lt.zmin) zmin=zh(i)
              if (zh(i).gt.zmax) zmax=zh(i)
            enddo

            t_magnets(imag)%t_xcuts(ix)%xmin=xmin
            t_magnets(imag)%t_xcuts(ix)%xmax=xmax
            t_magnets(imag)%t_xcuts(ix)%ymin=ymin
            t_magnets(imag)%t_xcuts(ix)%ymax=ymax
            t_magnets(imag)%t_xcuts(ix)%zmin=zmin
            t_magnets(imag)%t_xcuts(ix)%zmax=zmax

            t_magnets(imag)%t_xcuts(ix)%size=[xmax-xmin,ymax-ymin,zmax-zmin]

            ncorn1=ncorn2
            corn1=corn2
            x02(1)=x02(2)
            y02(1)=y02(2)
            z02(1)=z02(2)

            if (ix.eq.nxdiv-1) then
              kx=ix+1
              npoi=0
              do iplan=1,nplanmax
                do icorn=1,ncorn1(iplan)
                  npoi=npoi+1
                  xh(npoi)=corn1(1,icorn,iplan)
                  yh(npoi)=corn1(2,icorn,iplan)
                  zh(npoi)=corn1(3,icorn,iplan)
                enddo
              enddo

              call util_convex_hull_3d_overwrite(imag,npoi,
     &          xh,yh,zh,khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &          hulltiny,ifailhull)

              allocate(t_magnets(imag)%t_xcuts(kx)%xhull(nhull))
              allocate(t_magnets(imag)%t_xcuts(kx)%yhull(nhull))
              allocate(t_magnets(imag)%t_xcuts(kx)%zhull(nhull))
              allocate(t_magnets(imag)%t_xcuts(kx)%khull(nhull))
              allocate(t_magnets(imag)%t_xcuts(kx)%kedge(4,nedge))
              allocate(t_magnets(imag)%t_xcuts(kx)%kface(kfacelast))

              t_magnets(imag)%t_xcuts(kx)%xyz=gcen+[x02(1),y02(1),z02(1)]
              t_magnets(imag)%t_xcuts(kx)%gcen=gcen+[x02(1),y02(1),z02(1)]
              t_magnets(imag)%t_xcuts(kx)%nhull=nhull
              t_magnets(imag)%t_xcuts(kx)%khull=khull
              t_magnets(imag)%t_xcuts(kx)%nface=nface
              t_magnets(imag)%t_xcuts(kx)%kface=kface
              t_magnets(imag)%t_xcuts(kx)%kfacelast=kfacelast
              t_magnets(imag)%t_xcuts(kx)%nedge=nedge
              t_magnets(imag)%t_xcuts(kx)%kedge=kedge
              t_magnets(imag)%t_xcuts(kx)%ixdiv=kx

              xmin=1.0d30
              xmax=-1.0d30
              ymin=1.0d30
              ymax=-1.0d30
              zmin=1.0d30
              zmax=-1.0d30

              do i=1,nhull
                t_magnets(imag)%t_xcuts(kx)%xhull(i)=xh(i)
                t_magnets(imag)%t_xcuts(kx)%yhull(i)=yh(i)
                t_magnets(imag)%t_xcuts(kx)%zhull(i)=zh(i)
                if (xh(i).lt.xmin) xmin=xh(i)
                if (xh(i).gt.xmax) xmax=xh(i)
                if (yh(i).lt.ymin) ymin=yh(i)
                if (yh(i).gt.ymax) ymax=yh(i)
                if (zh(i).lt.zmin) zmin=zh(i)
                if (zh(i).gt.zmax) zmax=zh(i)
              enddo

              t_magnets(imag)%t_xcuts(kx)%size=[xmax-xmin,ymax-ymin,zmax-zmin]

              t_magnets(imag)%t_xcuts(kx)%xmin=xmin
              t_magnets(imag)%t_xcuts(kx)%xmax=xmax
              t_magnets(imag)%t_xcuts(kx)%ymin=ymin
              t_magnets(imag)%t_xcuts(kx)%ymax=ymax
              t_magnets(imag)%t_xcuts(kx)%zmin=zmin
              t_magnets(imag)%t_xcuts(kx)%zmax=zmax
              t_magnets(imag)%mxdiv=t_magnets(imag)%mxdiv+1

            endif !last cut

          endif !ncorn2, cut

        enddo !nxdiv -1

      endif !nxdiv.dq.1

      deallocate(corn1,corn2)

      xvolmag=0.0d0
      t_magnets(imag)%xvolume=0.0d0
      do ix=1,nxdiv
        nhull=t_magnets(imag)%t_xcuts(ix)%nhull
        if (nhull.gt.0) then
          tvox=t_magnets(imag)%t_xcuts(ix)
          call util_volume(nhull,tvox%xhull,tvox%yhull,tvox%zhull,hulltiny,
     &      vol,ifail)
          if (ifail.ne.0) then
            write(lun6,*)"*** Error in clcmag_xcut: Bad return from util_volume, magnet, magnet number, ixdiv: ",
     &        trim(tmag%cnam),imag,ix
            stop
          endif
          t_magnets(imag)%t_xcuts(ix)%volume=vol
        else
          t_magnets(imag)%t_xcuts(ix)%volume=0.0d0
        endif
        xvolmag=xvolmag+vol
      enddo

      vol=(xvolmag-t_magnets(imag)%volume)/t_magnets(imag)%volume

      if (abs(vol).gt.1.0d-9) then
        write(lun6,*)"*** Warning in clcmag_xcut: Sum of x-cut volumes differs from magnet volume by (rel.):",vol
        write(lun6,*)"Magnet: ",t_magnets(imag)%cnam
        write(lun6,*)"Maybe you should try MODSIMPHULL=1 in undumag.nam"
      endif


      !call util_break

c      if (imag.eq.2) then
c        do ix=1,nxdiv
c          print*,t_magnets(imag)%t_xcuts(ix)%kface(1:t_magnets(imag)%t_xcuts(ix)%kfacelast)
c        enddo
c        stop
c      endif

      return
      end
