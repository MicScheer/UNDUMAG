*CMZ :  2.04/16 12/09/2023  13.51.44  by  Michael Scheer
*CMZ :  2.04/14 05/09/2023  14.01.56  by  Michael Scheer
*CMZ :  2.04/06 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/03 05/03/2023  16.30.33  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  17.27.31  by  Michael Scheer
*CMZ :  2.04/01 13/02/2023  13.19.45  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.19.49  by  Michael Scheer
*CMZ :  2.02/02 15/02/2022  16.00.05  by  Michael Scheer
*CMZ :  2.02/01 23/01/2022  14.52.10  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_zcut(imag)

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      character(128) ctype

      double precision, dimension (:,:,:), allocatable :: corn1,corn2
      double precision, dimension (:), allocatable :: xh,yh,zh,xhc,yhc,zhc

      double precision x,y,z,
     &  x02(2),y02(2),z02(2),zdivmin,zdivmax,
     &  gcen(3),gcenv(3),xmin,xmax,ymin,ymax,zmin,zmax,
     &  dzdiv,zdiv,fracsum,zfracdiv,zfacdiv,volmag,vol

      integer, dimension (:), allocatable :: ncorn1,ncorn2

      integer :: idebug=0,
     &  i,j,l,n,ix,iy,iz,kz,imag,ip,npoi,iplan,icorn,
     &  nxdiv,nydiv,nzdiv,kcut,izdiv,klast,nplan,ncorn

      integer ifailhull,ifail,nhull,nedge,nface,kfacelast,nvox

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      type(T_Magnet) :: tmag
      type(T_Voxel) :: tvox


      if (idebug.gt.0) call util_break

      tmag=t_magnets(imag)

      gcen=t_magnets(imag)%gcen

      nxdiv=t_magnets(imag)%nxdiv
      nydiv=t_magnets(imag)%nydiv
      nzdiv=t_magnets(imag)%nzdiv

      if (idebug.eq.10) then
        do ix=1,nxdiv
          do iy=1,nydiv
            print*,ix,iy,t_magnets(imag)%t_xycuts(ix,iy)%volume
          enddo
        enddo
        stop
      endif

      ctype=t_magnets(imag)%ctype

      if (idebug.lt.0) then
        print*,imag,tmag%cnam,tmag%cmoth,nxdiv,nydiv,nzdiv
      endif

      if (nzdiv.gt.1) then

        allocate(t_magnets(imag)%zdivs(nzdiv-1))

        fracsum=0.0d0
        ! zfracdiv**(nzdiv-1)=zfacdiv
        ! ln(zfracdiv)*(nzdiv-1)=ln(zfacdiv)
        zfacdiv=t_magnets(imag)%zfracdiv
        zdivmin=t_magnets(imag)%zmin
        zdivmax=t_magnets(imag)%zmax

        if (nzdiv.gt.1) then
          zfracdiv=exp(log(zfacdiv)/(nzdiv-1))
        else
          zfracdiv=1.0d0
        endif

        do izdiv=0,nzdiv-1
          fracsum=fracsum+zfracdiv**izdiv
        enddo

        dzdiv=(zdivmax-zdivmin)/fracsum*zfacdiv

        if (nzdiv.gt.1) t_magnets(imag)%zdivs(1)=zdivmin+dzdiv

        do iz=2,nzdiv-1
          dzdiv=dzdiv/zfracdiv
          t_magnets(imag)%zdivs(iz)=t_magnets(imag)%zdivs(iz-1)+dzdiv
        enddo

      endif

        allocate(corn1(3,2*ncornmax,2*nplanmax),corn2(3,2*ncornmax,2*nplanmax))
        allocate(ncorn1(2*nplanmax),ncorn2(2*nplanmax))
        allocate(
     &    xh(2*ncornmax*nplanmax),yh(2*ncornmax*nplanmax),zh(2*ncornmax*nplanmax),
     &    xhc(2*ncornmax*nplanmax),yhc(2*ncornmax*nplanmax),zhc(2*ncornmax*nplanmax))
        allocate(khull(2*ncornmax*nplanmax),kedge(4,2*ncornmax*nplanmax-2),
     &    kface(5*ncornmax*nplanmax))

      nvox=0

      do ix=1,nxdiv

        do iy=1,nydiv

          x02=0.0d0
          y02=0.0d0
          z02=0.0d0

          t_magnets(imag)%t_xycuts(ix,iy)%mzdiv=0

          tvox=t_magnets(imag)%t_xycuts(ix,iy)

          if (tvox%iydiv.eq.0) cycle

          if (idebug.lt.0) then
            print*,ix,iy,tvox%nface
          endif

          ncorn1=0
          ncorn2=0
          l=0

          if (nzdiv.gt.1) then

            do i=1,tvox%nface
              l=l+1
              n=tvox%kface(l)
              ncorn1(i)=n
              do j=1,n
                l=l+1
                ip=tvox%kface(l)
                x=tvox%xhull(ip)
                y=tvox%yhull(ip)
                z=tvox%zhull(ip)
                corn1(1,j,i)=x
                corn1(2,j,i)=y
                corn1(3,j,i)=z
              enddo
            enddo

          endif !iz

          gcenv=tvox%gcen

          kcut=0

          do iz=1,nzdiv-1

            zmin=1.0d30
            zmax=-1.0d30
            do iplan=1,nplanmax
              do icorn=1,ncorn1(iplan)
                z=corn1(3,icorn,iplan)+z02(1)
                if (z.lt.zmin) zmin=z
                if (z.gt.zmax) zmax=z
              enddo
            enddo

            zdiv=t_magnets(imag)%zdivs(iz)+gcen(3) !Labor
            zdiv=zdiv-gcenv(3) ! relative to gcenv, i.e. GCEN of voxel

            if (zdiv-zmin.le.cuttiny.or.zmax-zdiv.le.cuttiny) then
              if (idebug.gt.0) call util_break
              cycle
            endif

            kcut=1
            exit

          enddo !iz

          if (nzdiv.eq.1.or.kcut.eq.0) then

            nvox=nvox+1

            if (tvox%iydiv.eq.0) cycle

            nhull=tvox%nhull
            nedge=tvox%nedge
            nface=tvox%nface
            khull=tvox%khull
            kedge=tvox%kedge
            kface=tvox%kface
            kfacelast=tvox%kfacelast

            allocate(t_magnets(imag)%t_xyzcuts(ix,iy,1)%xhull(nhull))
            allocate(t_magnets(imag)%t_xyzcuts(ix,iy,1)%yhull(nhull))
            allocate(t_magnets(imag)%t_xyzcuts(ix,iy,1)%zhull(nhull))
            allocate(t_magnets(imag)%t_xyzcuts(ix,iy,1)%khull(nhull))
            allocate(t_magnets(imag)%t_xyzcuts(ix,iy,1)%kedge(4,nedge))
            allocate(t_magnets(imag)%t_xyzcuts(ix,iy,1)%kface(kfacelast))

            t_magnets(imag)%t_xyzcuts(ix,iy,1)%nhull=nhull
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%khull=khull
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%nedge=nedge
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%kedge=kedge
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%nface=nface
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%kface=kface
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%kfacelast=kfacelast

            t_magnets(imag)%t_xyzcuts(ix,iy,1)%size=tvox%size
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%volume=tvox%volume

            t_magnets(imag)%t_xyzcuts(ix,iy,1)%xmin=tvox%xmin
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%xmax=tvox%xmax
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%ymin=tvox%ymin
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%ymax=tvox%ymax
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%zmin=tvox%zmin
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%zmax=tvox%zmax

            t_magnets(imag)%t_xyzcuts(ix,iy,1)%xyz=tvox%gcen
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%gcen=tvox%gcen

            do i=1,nhull
              t_magnets(imag)%t_xyzcuts(ix,iy,1)%xhull(i)=tvox%xhull(i)
              t_magnets(imag)%t_xyzcuts(ix,iy,1)%yhull(i)=tvox%yhull(i)
              t_magnets(imag)%t_xyzcuts(ix,iy,1)%zhull(i)=tvox%zhull(i)
            enddo

            t_magnets(imag)%t_xyzcuts(ix,iy,1)%ixdiv=ix
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%iydiv=iy
            t_magnets(imag)%t_xyzcuts(ix,iy,1)%izdiv=1
            t_magnets(imag)%kvoxels(ix,iy,1)=nvox

          else  !nzdiv.eq.1

            l=0
            ncorn1=0
            ncorn2=0

            do i=1,tvox%nface

              l=l+1

              n=tvox%kface(l)
              ncorn1(i)=n

              do j=1,n
                l=l+1
                ip=tvox%kface(l)
                x=tvox%xhull(ip)
                y=tvox%yhull(ip)
                z=tvox%zhull(ip)
                corn1(1,j,i)=x
                corn1(2,j,i)=y
                corn1(3,j,i)=z
              enddo !npoi

              ncorn1(i)=ncorn1(i)+1
              corn1(:,n+1,i)=corn1(:,1,i) ! for undumag_cut_magnet,
              ! the plane must be closed

            enddo !nface

            x02(2)=0.0d0
            y02(2)=0.0d0
            z02(2)=0.0d0

            !if (nzdiv.gt.1) zdiv=t_magnets(imag)%zdivs(1)+gcen(3)-gcenv(3)
            do iz=1,nzdiv-1

              nvox=nvox+1

              !zdiv=t_magnets(imag)%zdivs(iz)+gcen(3)-gcenv(3)+z02(1)

              zmin=1.0d30
              zmax=-1.0d30
              do iplan=1,nplanmax
                do icorn=1,ncorn1(iplan)
                  z=corn1(3,icorn,iplan)+z02(1)
                  if (z.lt.zmin) zmin=z
                  if (z.gt.zmax) zmax=z
                enddo
              enddo

              !dzdiv=t_magnets(imag)%zdivs(iz)-t_magnets(imag)%zmin
              !zdiv=zmin+z02(1)+dzdiv
              !zdiv=t_magnets(imag)%zdivs(iz)
              zdiv=t_magnets(imag)%zdivs(iz)+gcen(3) !Labor
              zdiv=zdiv-gcenv(3) ! relative to gcenv, i.e. GCEN of voxel

              klast=0

              if (zdiv-zmin.le.cuttiny.or.zmax-zdiv.le.cuttiny) then

                if (idebug.gt.0) call util_break
                klast=iz-1
                !cycle
                ! Hier ggf. clcmag_corn_to_vox benutzen, und auch unten
                goto 123

              else !(zmin.ge.zdiv.or.zmax.le.zdiv) then

                x02(1)=x02(2) !Labor
                y02(1)=y02(2)
                z02(1)=z02(2)

                call undumag_cut_magnet(imag,x02,y02,z02,
     &            2*nplanmax,2*ncornmax,
     &            ncorn1,corn1,ncorn2,corn2
     &            ,3,zdiv,hulltiny,ifail)

                if (ifail.ne.0) then
                  write(lun6,*)"*** Error in clcmag_zcut: Bad return from undumag_cut_magnet, magnet, ifail, mag, ixdiv, iydiv, izdiv, zdiv:",
     &              trim(t_magnets(imag)%cnam),ifail,imag,ix,iy," 1 ",zdiv
                  stop
                endif

                if (ncorn2(1).eq.0) then
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)=
     &              t_magnets(imag)%t_xycuts(ix,iy)
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%ixdiv=ix
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%iydiv=iy
                   t_magnets(imag)%t_xyzcuts(ix,iy,iz)%izdiv=iz
                  t_magnets(imag)%t_xycuts(ix,iy)%mzdiv=
     &              t_magnets(imag)%t_xycuts(ix,iy)%mzdiv+1
                  cycle
c                write(lun6,*) '*** Error in clcmag_zcut: No z-Cut for magnet, ixdiv, iydiv, izdiv ',
c     &            t_magnets(imag)%cnam,ixdiv,iydiv," 1"

                else !if (ncorn2(1).eq.0) then

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
     &              xh,yh,zh,khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &              hulltiny,ifailhull)

                  allocate(t_magnets(imag)%t_xyzcuts(ix,iy,iz)%xhull(nhull))
                  allocate(t_magnets(imag)%t_xyzcuts(ix,iy,iz)%yhull(nhull))
                  allocate(t_magnets(imag)%t_xyzcuts(ix,iy,iz)%zhull(nhull))
                  allocate(t_magnets(imag)%t_xyzcuts(ix,iy,iz)%khull(nhull))

                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%xyz=gcenv+[x02(1),y02(1),z02(1)]
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%gcen=gcenv+[x02(1),y02(1),z02(1)]
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%nhull=nhull
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%khull=khull
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%nface=nface
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%kface=kface
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%kfacelast=kfacelast
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%nedge=nedge
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%kedge=kedge

                  xmin=1.0d30
                  xmax=-1.0d30
                  ymin=1.0d30
                  ymax=-1.0d30
                  zmin=1.0d30
                  zmax=-1.0d30

                  do i=1,nhull
                    t_magnets(imag)%t_xyzcuts(ix,iy,iz)%xhull(i)=xh(i)
                    t_magnets(imag)%t_xyzcuts(ix,iy,iz)%yhull(i)=yh(i)
                    t_magnets(imag)%t_xyzcuts(ix,iy,iz)%zhull(i)=zh(i)
                    if (xh(i).lt.xmin) xmin=xh(i)
                    if (xh(i).gt.xmax) xmax=xh(i)
                    if (yh(i).lt.ymin) ymin=yh(i)
                    if (yh(i).gt.ymax) ymax=yh(i)
                    if (zh(i).lt.zmin) zmin=zh(i)
                    if (zh(i).gt.zmax) zmax=zh(i)
                  enddo

                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%xmin=xmin
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%xmax=xmax
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%ymin=ymin
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%ymax=ymax
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%zmin=zmin
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%zmax=zmax

                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%size=
     &              [xmax-xmin,ymax-ymin,zmax-zmin]
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%ixdiv=ix
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%iydiv=iy
                  t_magnets(imag)%t_xyzcuts(ix,iy,iz)%izdiv=iz
                  t_magnets(imag)%t_xycuts(ix,iy)%mzdiv=
     &              t_magnets(imag)%t_xycuts(ix,iy)%mzdiv+1
                  t_magnets(imag)%kvoxels(ix,iy,iz)=nvox

                  ncorn1=ncorn2
                  corn1=corn2
                  x02(1)=x02(2)
                  y02(1)=y02(2)
                  z02(1)=z02(2)

                endif !(ncorn2(1).eq.0)

              endif !(zmin.ge.zdiv.or.zmax.le.zdiv) then

123           if (iz.eq.nzdiv-1.or.klast.ne.0) then

                nvox=nvox+1

                kz=iz+1
                if (klast.ne.0) then
                  kz=klast+1
                endif
                npoi=0
                do iplan=1,nplanmax
                  do icorn=1,ncorn1(iplan)
                    npoi=npoi+1
                    xh(npoi)=corn1(1,icorn,iplan)
                    yh(npoi)=corn1(2,icorn,iplan)
                    zh(npoi)=corn1(3,icorn,iplan)
                  enddo
                enddo

                call util_convex_hull_3d_overwrite(nvox,npoi,
     &            xh,yh,zh,khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &            hulltiny,ifailhull)

                allocate(t_magnets(imag)%t_xyzcuts(ix,iy,kz)%xhull(nhull))
                allocate(t_magnets(imag)%t_xyzcuts(ix,iy,kz)%yhull(nhull))
                allocate(t_magnets(imag)%t_xyzcuts(ix,iy,kz)%zhull(nhull))
                allocate(t_magnets(imag)%t_xyzcuts(ix,iy,kz)%khull(nhull))
                allocate(t_magnets(imag)%t_xyzcuts(ix,iy,kz)%kedge(4,nedge))
                allocate(t_magnets(imag)%t_xyzcuts(ix,iy,kz)%kface(kfacelast))

                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%xyz=gcenv+[x02(1),y02(1),z02(1)]
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%gcen=gcenv+[x02(1),y02(1),z02(1)]
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%nhull=nhull
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%khull=khull
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%nface=nface
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%kface=kface
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%kfacelast=kfacelast
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%nedge=nedge
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%kedge=kedge

                xmin=1.0d30
                xmax=-1.0d30
                ymin=1.0d30
                ymax=-1.0d30
                zmin=1.0d30
                zmax=-1.0d30

                do i=1,nhull
                  t_magnets(imag)%t_xyzcuts(ix,iy,kz)%xhull(i)=xh(i)
                  t_magnets(imag)%t_xyzcuts(ix,iy,kz)%yhull(i)=yh(i)
                  t_magnets(imag)%t_xyzcuts(ix,iy,kz)%zhull(i)=zh(i)
                  if (xh(i).lt.xmin) xmin=xh(i)
                  if (xh(i).gt.xmax) xmax=xh(i)
                  if (yh(i).lt.ymin) ymin=yh(i)
                  if (yh(i).gt.ymax) ymax=yh(i)
                  if (zh(i).lt.zmin) zmin=zh(i)
                  if (zh(i).gt.zmax) zmax=zh(i)
                enddo

                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%size=[xmax-xmin,ymax-ymin,zmax-zmin]

                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%xmin=xmin
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%xmax=xmax
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%ymin=ymin
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%ymax=ymax
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%zmin=zmin
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%zmax=zmax

                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%ixdiv=ix
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%iydiv=iy
                t_magnets(imag)%t_xyzcuts(ix,iy,kz)%izdiv=kz
                t_magnets(imag)%t_xycuts(ix,iy)%mzdiv=
     &            t_magnets(imag)%t_xycuts(ix,iy)%mzdiv+1
                t_magnets(imag)%kvoxels(ix,iy,kz)=nvox

                exit

              endif ! klast

            enddo !nzdiv -1

          volmag=0.0d0

          do iz=1,nzdiv

            nhull=t_magnets(imag)%t_xyzcuts(ix,iy,iz)%nhull

            if (nhull.gt.0) then
              tvox=t_magnets(imag)%t_xyzcuts(ix,iy,iz)
              call util_volume(nhull,tvox%xhull,tvox%yhull,tvox%zhull,hulltiny,
     &          vol,ifail)
              if (ifail.ne.0) then
                write(lun6,*)"*** Error in clcmag_ycut: Bad return from util_volume, magnet, magnet number, ix,iy:",
     &            trim(tmag%cnam),imag,ix,iy
                stop
              endif
              t_magnets(imag)%t_xyzcuts(ix,iy,iz)%volume=vol
              volmag=volmag+vol
            else
              t_magnets(imag)%t_xyzcuts(ix,iy,iz)%volume=0.0d0
            endif

          enddo !iz

          vol=(volmag-t_magnets(imag)%t_xycuts(ix,iy)%volume)/
     &      t_magnets(imag)%t_xycuts(ix,iy)%volume

          if (abs(vol).gt.1.0d-9) then
            write(lun6,*)"*** Warning in clcmag_zcut: Sum of xyz-cut volumes differs from xy-cuts by (rel.):",vol
            write(lun6,*)"*** magnet, ixdiv, iydiv: ",trim(tmag%cnam),ix,iy
          endif

        endif !nzdiv.eq.1

      enddo !iy=1,nydiv

      enddo !nxdiv

!      if (nzdiv.gt.1) then
      deallocate(ncorn1,ncorn2,corn1,corn2,xh,yh,zh,xhc,yhc,zhc,kedge,kface)
!      endif

      t_magnets(imag)%nvoxels=nvox
      nvox_t=nvox_t+nvox

      if (idebug.gt.0) call util_break


      volmag=0.0d0
      do ix=1,nxdiv
        do iy=1,nydiv
          do iz=1,nzdiv
            volmag=volmag+t_magnets(imag)%t_xyzcuts(ix,iy,iz)%volume
            !print*,"++++++++++++++++++++++++++++++++++++++++++++++++"
            !print*,ix,iy,iz
            !print*,"++++++++++++++++++++++++++++++++++++++++++++++++"
            l=1
            nplan=0
            do i=1,t_magnets(imag)%t_xyzcuts(ix,iy,iz)%kfacelast
              nplan=nplan+1
              ncorn=t_magnets(imag)%t_xyzcuts(ix,iy,iz)%kface(l)
              !print*,nplan,ncorn
              if (nplan.gt.nplanmax) then
                nplanmax=nplan
                call clcbuff_reallocate
              endif
              if (ncorn.gt.ncornmax) ncornmax=ncorn
              l=l+n+1
              if (nplan.eq.t_magnets(imag)%t_xyzcuts(ix,iy,iz)%nface) exit
            enddo
          enddo
        enddo
      enddo

      vol=(volmag-t_magnets(imag)%volume)/t_magnets(imag)%volume

      if (abs(vol).gt.1.0d-9) then
        write(lun6,*)"*** Warning in clcmag_zcut: Sum of xyz-cut volumes differs from magnet volume by (rel.): ",vol
        write(lun6,*)"*** magnet :",trim(tmag%cnam),imag
      endif

      return
      end
