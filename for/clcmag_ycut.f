*CMZ :          17/12/2024  12.24.21  by  Michael Scheer
*CMZ :  2.05/05 27/02/2024  21.58.56  by  Michael Scheer
*CMZ :  2.05/02 24/10/2023  14.46.42  by  Michael Scheer
*CMZ :  2.04/24 27/09/2023  16.32.07  by  Michael Scheer
*CMZ :  2.04/22 25/09/2023  12.27.21  by  Michael Scheer
*CMZ :  2.04/16 11/09/2023  10.23.37  by  Michael Scheer
*CMZ :  2.04/14 05/09/2023  13.59.00  by  Michael Scheer
*CMZ :  2.04/06 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/03 05/03/2023  16.22.55  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  18.03.22  by  Michael Scheer
*CMZ :  2.04/01 13/02/2023  13.49.55  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.19.49  by  Michael Scheer
*CMZ :  2.02/02 15/02/2022  15.59.51  by  Michael Scheer
*CMZ :  2.02/01 23/01/2022  14.50.54  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_ycut(imag)

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision, dimension (:,:,:), allocatable :: corn1,corn2
      double precision, dimension (:), allocatable :: xh,yh,zh,xhc,yhc,zhc

      double precision x,y,z,
     &  x02(2),y02(2),z02(2),ydivmin,ydivmax,
     &  gcen(3),gcenv(3),xmin,xmax,ymin,ymax,zmin,zmax,dydiv,ydiv,
     &  yfracdiv,fracsum,yfacdiv,vol,yvolmag

      integer, dimension (:), allocatable :: ncorn1,ncorn2

      integer :: idebug=0,
     &  i,j,l,n,ix,iy,ky,imag,ip,npoi,iplan,icorn,
     &  nxdiv,nydiv,nzdiv,kcut,klast

      integer :: ifailhull,ifail,nhull,nedge,nface,kfacelast,iydiv,ical=0

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface
*KEEP,hulldim.
      integer lenhull,lenedge,lenface,nverhullmax
      common/uhullc/lenhull,lenedge,lenface,nverhullmax
*KEND.
      character(128) ctype

      type(T_Magnet) :: tmag
      type(T_Voxel) :: tvox


      if (idebug.ne.0) then
        call util_break
      endif

      ical=ical+1

      tmag=t_magnets(imag)
      gcen=t_magnets(imag)%gcen

      nxdiv=t_magnets(imag)%nxdiv
      nydiv=t_magnets(imag)%nydiv
      nzdiv=t_magnets(imag)%nzdiv

      ctype=t_magnets(imag)%ctype

      if (nydiv.gt.1) then

        allocate(t_magnets(imag)%ydivs(nydiv-1))

        fracsum=0.0d0
        ! yfracdiv**(nydiv-1)=yfacdiv
        ! ln(yfracdiv)*(nydiv-1)=ln(yfacdiv)
        yfacdiv=t_magnets(imag)%yfracdiv
        ydivmin=t_magnets(imag)%ymin
        ydivmax=t_magnets(imag)%ymax
        yfacdiv=t_magnets(imag)%yfracdiv

        if (nydiv.gt.1) then
          yfracdiv=exp(log(yfacdiv)/(nydiv-1))
        else
          yfracdiv=1.0d0
        endif
        do iydiv=0,nydiv-1
          fracsum=fracsum+yfracdiv**iydiv
        enddo

        dydiv=(ydivmax-ydivmin)/fracsum*yfacdiv
        t_magnets(imag)%ydivs(1)=ydivmin+dydiv

        if (idebug.ne.0) then
          call util_break
        endif

        do iy=2,nydiv-1
          dydiv=dydiv/yfracdiv
          t_magnets(imag)%ydivs(iy)=t_magnets(imag)%ydivs(iy-1)+dydiv !relative to gcen
        enddo

        allocate(corn1(3,2*ncornmax,2*nplanmax),corn2(3,2*ncornmax,2*nplanmax))
        allocate(ncorn1(2*nplanmax),ncorn2(2*nplanmax))
        allocate(
     &    xh(2*ncornmax*nplanmax),yh(2*ncornmax*nplanmax),zh(2*ncornmax*nplanmax),
     &    xhc(2*ncornmax*nplanmax),yhc(2*ncornmax*nplanmax),zhc(2*ncornmax*nplanmax))
        allocate(khull(lenhull),kedge(4,lenedge),kface(lenface))
      endif

      do ix=1,nxdiv

        tvox = t_magnets(imag)%t_xcuts(ix)
        t_magnets(imag)%t_xcuts(ix)%mydiv=0
        gcenv=tvox%gcen

        kcut=0

        !if (yfracdiv.gt.1) ydiv=t_magnets(imag)%ydivs(1)+gcen(2)-gcenv(2)

        do iy=1,nydiv-1
          ydiv=t_magnets(imag)%ydivs(iy)+gcen(2) !Labor
          ydiv=ydiv-gcenv(2) ! relative to gcenv
          if (ydiv.le.tvox%ymin.or.ydiv-tvox%ymin.le.cuttiny.or.tvox%ymax-ydiv.le.cuttiny) cycle
          kcut=1
          exit
        enddo

        if (nydiv.eq.1.or.kcut.eq.0) then

          nhull=tvox%nhull
          nedge=tvox%nedge
          nface=tvox%nface
          khull=tvox%khull
          kedge=tvox%kedge
          kface=tvox%kface
          kfacelast=tvox%kfacelast

          allocate(t_magnets(imag)%t_xycuts(ix,1)%khull(nhull))
          allocate(t_magnets(imag)%t_xycuts(ix,1)%xhull(nhull))
          allocate(t_magnets(imag)%t_xycuts(ix,1)%yhull(nhull))
          allocate(t_magnets(imag)%t_xycuts(ix,1)%zhull(nhull))
          allocate(t_magnets(imag)%t_xycuts(ix,1)%kedge(4,nedge))
          allocate(t_magnets(imag)%t_xycuts(ix,1)%kface(kfacelast))

          t_magnets(imag)%t_xycuts(ix,1)%nhull=nhull
          t_magnets(imag)%t_xycuts(ix,1)%khull=khull
          t_magnets(imag)%t_xycuts(ix,1)%nedge=nedge
          t_magnets(imag)%t_xycuts(ix,1)%kedge=kedge
          t_magnets(imag)%t_xycuts(ix,1)%nface=nface
          t_magnets(imag)%t_xycuts(ix,1)%kface=kface
          t_magnets(imag)%t_xycuts(ix,1)%kfacelast=kfacelast

          t_magnets(imag)%t_xycuts(ix,1)%size=tvox%size

          t_magnets(imag)%t_xycuts(ix,1)%xmin=tvox%xmin
          t_magnets(imag)%t_xycuts(ix,1)%xmax=tvox%xmax
          t_magnets(imag)%t_xycuts(ix,1)%ymin=tvox%ymin
          t_magnets(imag)%t_xycuts(ix,1)%ymax=tvox%ymax
          t_magnets(imag)%t_xycuts(ix,1)%zmin=tvox%zmin
          t_magnets(imag)%t_xycuts(ix,1)%zmax=tvox%zmax

          t_magnets(imag)%t_xycuts(ix,1)%xyz=tvox%gcen
          t_magnets(imag)%t_xycuts(ix,1)%gcen=tvox%gcen
          t_magnets(imag)%t_xycuts(ix,1)%volume=tvox%volume

          do i=1,nhull
            t_magnets(imag)%t_xycuts(ix,1)%xhull(i)=tvox%xhull(i)
            t_magnets(imag)%t_xycuts(ix,1)%yhull(i)=tvox%yhull(i)
            t_magnets(imag)%t_xycuts(ix,1)%zhull(i)=tvox%zhull(i)
          enddo

          t_magnets(imag)%t_xycuts(ix,1)%ixdiv=ix
          t_magnets(imag)%t_xycuts(ix,1)%iydiv=1
          t_magnets(imag)%t_xcuts(ix)%mydiv=1

        else  !nydiv.eq.1

          l=0
          ncorn1=0
          ncorn2=0

          do i=1,tvox%nface

            l=l+1

            n=tvox%kface(l)
            ncorn1(i)=n
            !corn1 is rel. to tvox%gcen
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

          x02=0.0d0
          y02=0.0d0
          z02=0.0d0

          !if (nydiv.gt.1) ydiv=t_magnets(imag)%ydivs(1)+gcen(2)-gcenv(2)

          do iy=1,nydiv-1

            ydiv=t_magnets(imag)%ydivs(iy)+gcen(2) !Labor
            ydiv=ydiv-gcenv(2) ! relative to gcenv
            ymin=1.0d30
            ymax=-1.0d30
            do iplan=1,nplanmax
              do icorn=1,ncorn1(iplan)
                npoi=npoi+1
                y=corn1(2,icorn,iplan)+y02(1)
                if (y.lt.ymin) ymin=y
                if (y.gt.ymax) ymax=y
              enddo
            enddo

            klast=0

            if (ydiv-ymin.le.cuttiny.or.ymax-ydiv.le.cuttiny) then

              klast=iy-1
              goto 123

            else !if (ymin.ge.ydiv.or.ymax.le.ydiv)

              x02(1)=x02(2) ! relative to tvox%gcen
              y02(1)=y02(2)
              z02(1)=z02(2)

              call undumag_cut_magnet(imag,x02,y02,z02,
     &          2*nplanmax,2*ncornmax,
     &          ncorn1,corn1,ncorn2,corn2
     &          ,2,ydiv,hulltiny,ifail)

              if (idebug.ne.0) then
                call util_break
              endif

              if (ifail.ne.0) then
                write(lun6,*)"*** Error in clcmag_ycuts: Bad return from undumag_cut_magnet, magnet, ifail, mag, ixdiv, iydiv, ydiv:",
     &            trim(t_magnets(imag)%cnam),ifail,imag,ix,iy,ydiv
                stop
              endif

            endif !(ymin.ge.ydiv.or.ymax.le.ydiv)

            if (ncorn2(1).eq.0) then

              t_magnets(imag)%t_xycuts(ix,iy)=
     &          t_magnets(imag)%t_xcuts(ix)
              t_magnets(imag)%t_xycuts(ix,iy)%ixdiv=ix
              t_magnets(imag)%t_xycuts(ix,iy)%iydiv=iy
              cycle
c              write(lun6,*) '*** Error in clcmag_ycut: No y-Cut for magnet, ixdiv, iydiv: ',
c     &          t_magnets(imag)%cnam,ix,iy
c              stop

            else !if (ncorn2(1).eq.0) then

              ! Lower piece

              t_magnets(imag)%t_xcuts(ix)%mydiv=
     &          t_magnets(imag)%t_xcuts(ix)%mydiv+1

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

              allocate(t_magnets(imag)%t_xycuts(ix,iy)%xhull(nhull))
              allocate(t_magnets(imag)%t_xycuts(ix,iy)%yhull(nhull))
              allocate(t_magnets(imag)%t_xycuts(ix,iy)%zhull(nhull))

              t_magnets(imag)%t_xycuts(ix,iy)%xyz=gcenv+[x02(1),y02(1),z02(1)]
              ! t_magnets(imag)%t_xycuts(ix,iy)%gcen is in cutting plane, here
              t_magnets(imag)%t_xycuts(ix,iy)%gcen=gcenv+[x02(1),y02(1),z02(1)]
              t_magnets(imag)%t_xycuts(ix,iy)%nhull=nhull
              t_magnets(imag)%t_xycuts(ix,iy)%khull=khull
              t_magnets(imag)%t_xycuts(ix,iy)%nface=nface
              t_magnets(imag)%t_xycuts(ix,iy)%kface=kface
              t_magnets(imag)%t_xycuts(ix,iy)%kfacelast=kfacelast
              t_magnets(imag)%t_xycuts(ix,iy)%nedge=nedge
              t_magnets(imag)%t_xycuts(ix,iy)%kedge=kedge

              xmin=1.0d30
              xmax=-1.0d30
              ymin=1.0d30
              ymax=-1.0d30
              zmin=1.0d30
              zmax=-1.0d30

              do i=1,nhull
                t_magnets(imag)%t_xycuts(ix,iy)%xhull(i)=xh(i)
                t_magnets(imag)%t_xycuts(ix,iy)%yhull(i)=yh(i)
                t_magnets(imag)%t_xycuts(ix,iy)%zhull(i)=zh(i)
                if (xh(i).lt.xmin) xmin=xh(i)
                if (xh(i).gt.xmax) xmax=xh(i)
                if (yh(i).lt.ymin) ymin=yh(i)
                if (yh(i).gt.ymax) ymax=yh(i)
                if (zh(i).lt.zmin) zmin=zh(i)
                if (zh(i).gt.zmax) zmax=zh(i)
              enddo

              t_magnets(imag)%t_xycuts(ix,iy)%xmin=xmin
              t_magnets(imag)%t_xycuts(ix,iy)%xmax=xmax
              t_magnets(imag)%t_xycuts(ix,iy)%ymin=ymin
              t_magnets(imag)%t_xycuts(ix,iy)%ymax=ymax
              t_magnets(imag)%t_xycuts(ix,iy)%zmin=zmin
              t_magnets(imag)%t_xycuts(ix,iy)%zmax=zmax

              t_magnets(imag)%t_xycuts(ix,iy)%size=[xmax-xmin,ymax-ymin,zmax-zmin]
              t_magnets(imag)%t_xycuts(ix,iy)%ixdiv=ix
              t_magnets(imag)%t_xycuts(ix,iy)%iydiv=iy

              !corn2 is upper piece

              ncorn1=ncorn2
              corn1=corn2
              x02(1)=x02(2)
              y02(1)=y02(2)
              z02(1)=z02(2)

            endif !(ncorn2(1).eq.0)

123         continue

            if (iy.eq.nydiv-1.or.klast.ne.0) then

              ky=iy+1
              if (klast.ne.0) then
                ky=klast+1
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

              call util_convex_hull_3d_overwrite(imag,npoi,
     &          xh,yh,zh,khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &          hulltiny,ifailhull)

              allocate(t_magnets(imag)%t_xycuts(ix,ky)%khull(nhull))
              allocate(t_magnets(imag)%t_xycuts(ix,ky)%xhull(nhull))
              allocate(t_magnets(imag)%t_xycuts(ix,ky)%yhull(nhull))
              allocate(t_magnets(imag)%t_xycuts(ix,ky)%zhull(nhull))
              allocate(t_magnets(imag)%t_xycuts(ix,ky)%kedge(4,nedge))
              allocate(t_magnets(imag)%t_xycuts(ix,ky)%kface(kfacelast))

              t_magnets(imag)%t_xycuts(ix,ky)%xyz=gcenv+[x02(1),y02(1),z02(1)]
              t_magnets(imag)%t_xycuts(ix,ky)%gcen=gcenv+[x02(1),y02(1),z02(1)]
              t_magnets(imag)%t_xycuts(ix,ky)%nhull=nhull
              t_magnets(imag)%t_xycuts(ix,ky)%khull=khull
              t_magnets(imag)%t_xycuts(ix,ky)%nface=nface
              t_magnets(imag)%t_xycuts(ix,ky)%kface=kface
              t_magnets(imag)%t_xycuts(ix,ky)%kfacelast=kfacelast
              t_magnets(imag)%t_xycuts(ix,ky)%nedge=nedge
              t_magnets(imag)%t_xycuts(ix,ky)%kedge=kedge

              xmin=1.0d30
              xmax=-1.0d30
              ymin=1.0d30
              ymax=-1.0d30
              zmin=1.0d30
              zmax=-1.0d30

              do i=1,nhull
                t_magnets(imag)%t_xycuts(ix,ky)%xhull(i)=xh(i)
                t_magnets(imag)%t_xycuts(ix,ky)%yhull(i)=yh(i)
                t_magnets(imag)%t_xycuts(ix,ky)%zhull(i)=zh(i)
                if (xh(i).lt.xmin) xmin=xh(i)
                if (xh(i).gt.xmax) xmax=xh(i)
                if (yh(i).lt.ymin) ymin=yh(i)
                if (yh(i).gt.ymax) ymax=yh(i)
                if (zh(i).lt.zmin) zmin=zh(i)
                if (zh(i).gt.zmax) zmax=zh(i)
              enddo

              t_magnets(imag)%t_xycuts(ix,ky)%xmin=xmin
              t_magnets(imag)%t_xycuts(ix,ky)%xmax=xmax
              t_magnets(imag)%t_xycuts(ix,ky)%ymin=ymin
              t_magnets(imag)%t_xycuts(ix,ky)%ymax=ymax
              t_magnets(imag)%t_xycuts(ix,ky)%zmin=zmin
              t_magnets(imag)%t_xycuts(ix,ky)%zmax=zmax

              t_magnets(imag)%t_xycuts(ix,ky)%size=[xmax-xmin,ymax-ymin,zmax-zmin]
              t_magnets(imag)%t_xycuts(ix,ky)%ixdiv=ix
              t_magnets(imag)%t_xycuts(ix,ky)%iydiv=ky

              t_magnets(imag)%t_xcuts(ix)%mydiv=
     &          t_magnets(imag)%t_xcuts(ix)%mydiv+1

              exit

            endif !(iy.eq.nydiv-1.or.klast.ne.0)

          enddo !iy=1,nydiv-1

        endif !nydiv.eq.1

        yvolmag=0.0d0

        do iy=1,nydiv

          if (kcut.eq.0.and.iy.gt.1) exit

          nhull=t_magnets(imag)%t_xycuts(ix,iy)%nhull

          if (nhull.gt.0) then
            tvox=t_magnets(imag)%t_xycuts(ix,iy)
            call util_volume(nhull,tvox%xhull,tvox%yhull,tvox%zhull,hulltiny,
     &        vol,ifail)
            if (ifail.ne.0) then
              write(lun6,*)"*** Error in clcmag_ycut: Bad return from util_volume, magnet, magnet number, ix,iy:",
     &          trim(tmag%cnam),imag,ix,iy
              stop
            endif
            t_magnets(imag)%t_xycuts(ix,iy)%volume=vol
            yvolmag=yvolmag+vol
          else
            t_magnets(imag)%t_xycuts(ix,iy)%volume=0.0d0
          endif

        enddo

        vol=(yvolmag-t_magnets(imag)%t_xcuts(ix)%volume)/
     &    t_magnets(imag)%t_xcuts(ix)%volume

        if (abs(vol).gt.1.0d-9) then
          write(lun6,*)"*** Warning in clcmag_ycut: Sum of xy-cut volumes differs from magnet volume by (rel.):",vol
          write(lun6,*)"*** magnet, ixdiv:",trim(tmag%cnam),ix
          write(lun6,*)"Maybe you should try MODSIMPHULL=1 in undumag.nam"
        endif

      enddo !nxdiv

      if (nydiv.gt.1) then
        deallocate(corn1,corn2,xh,yh,zh,xhc,yhc,zhc,kedge,kface)
      endif


      !all util_break

      yvolmag=0.0d0
      do ix=1,nxdiv
        do iy=1,nydiv
          yvolmag=yvolmag+t_magnets(imag)%t_xycuts(ix,iy)%volume
        enddo
      enddo
      vol=(yvolmag-t_magnets(imag)%volume)/t_magnets(imag)%volume

      if (abs(vol).gt.1.0d-9) then
        write(lun6,*)"*** Warning in clcmag_ycut: Sum of xy-cut volumes differs from magnet volume by (rel.): ",vol
        write(lun6,*)"*** magnet :",trim(tmag%cnam),imag
        write(lun6,*)"Maybe you should try MODSIMPHULL=1 in undumag.nam"
      endif


      return
      end
