*CMZ :  2.04/24 27/09/2023  15.47.49  by  Michael Scheer
*CMZ :  2.04/22 25/09/2023  12.27.21  by  Michael Scheer
*CMZ :  2.04/03 03/03/2023  14.42.15  by  Michael Scheer
*CMZ :  2.04/01 22/01/2023  18.48.18  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.19.49  by  Michael Scheer
*CMZ :  2.02/01 28/09/2021  13.02.36  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.01/03 02/05/2019  16.44.45  by  Michael Scheer
*CMZ :  1.22/02 31/07/2017  12.04.40  by  Michael Scheer
*CMZ :  1.15/07 05/04/2017  15.49.03  by  Michael Scheer
*CMZ :  1.15/00 28/03/2017  11.38.20  by  Michael Scheer
*CMZ :  1.13/03 16/03/2017  12.49.34  by  Michael Scheer
*CMZ :  1.11/05 22/02/2017  09.55.22  by  Michael Scheer
*CMZ :  1.11/03 16/01/2017  12.22.22  by  Michael Scheer
*CMZ :  1.10/00 09/11/2016  12.50.19  by  Michael Scheer
*CMZ :  0.00/13 12/08/2016  15.41.28  by  Michael Scheer
*CMZ :  0.00/09 04/07/2016  17.51.47  by  Michael Scheer
*CMZ :  0.00/00 19/04/2016  16.56.39  by  Michael Scheer
*CMZ :  1.17/15 19/04/2016  15.50.32  by  Michael Scheer
*CMZ :  1.17/14 12/04/2016  17.30.57  by  Michael Scheer
*CMZ :          11/04/2016  16.18.24  by  Michael Scheer
      subroutine undumag_cut_magnet(kmag,
     &  x0,y0,z0,nplanmax,ncornmax,
     &  ncorn1,corn1,ncorn2,corn2,modediv,cut,tol,ifail)

      use commandlinef90m

      implicit none

      double precision, dimension (:), allocatable :: xh,yh,zh,
     &  xh1,yh1,zh1,xh2,yh2,zh2

      double precision corn1(3,ncornmax,nplanmax),corn2(3,ncornmax,nplanmax),
     &  x0(2),y0(2),z0(2),cut,x,y,z,x1,y1,z1,x2,y2,z2

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      double precision :: tol,zmin,zmax,ymin,ymax,xmin,xmax

      integer kmag,modediv,iplan,icorn,ifail,
     &  nplanmax,ncornmax,i,nh,nh1,nh2,ncorn,
     &  ncorn1(nplanmax),ncorn2(nplanmax),ipoi,npoi
     &  ,nedge,nface,kfacelast,n1,n2,iedge,k,k1,k2,lunf

      integer :: ical=0

*KEEP,hulldim.
      include 'hulldim.cmn'
*KEND.

      ical=ical+1
      ifail=0

c      nh=ncornmax*nplanmax

      allocate(
     &  xh(lenhull),yh(lenhull),zh(lenhull),
     &  xh1(lenhull),yh1(lenhull),zh1(lenhull),
     &  xh2(lenhull),yh2(lenhull),zh2(lenhull),
     &  khull(lenhull),kedge(4,lenedge),kface(lenface))

      xmin=1.0d30
      xmax=-1.0d30
      ymin=1.0d30
      ymax=-1.0d30
      zmin=1.0d30
      zmax=-1.0d30

      npoi=0
      do iplan=1,nplanmax-1
        ncorn=ncorn1(iplan)-1
        if(ncorn.eq.0) exit
        do icorn=1,ncorn
          npoi=npoi+1
          x=corn1(1,icorn,iplan)+x0(1) !Labor
          y=corn1(2,icorn,iplan)+y0(1)
          z=corn1(3,icorn,iplan)+z0(1)
          xh(npoi)=x
          yh(npoi)=y
          zh(npoi)=z
          if (x.lt.xmin) xmin=x
          if (x.gt.xmax) xmax=x
          if (y.lt.ymin) ymin=y
          if (y.gt.ymax) ymax=y
          if (z.lt.zmin) zmin=z
          if (z.gt.zmax) zmax=z
        enddo
      enddo


      if (
     &    modediv.eq.1.and.(abs(xmax-cut).lt.tol.or.abs(xmin-cut).lt.tol).or.
     &    modediv.eq.2.and.(abs(ymax-cut).lt.tol.or.abs(ymin-cut).lt.tol).or.
     &    modediv.eq.3.and.(abs(zmax-cut).lt.tol.or.abs(zmin-cut).lt.tol)
     &    ) then
c        ifail=-1234
        ncorn2=0
        goto 9999
      endif

      call util_convex_hull_3d_overwrite(kmag,npoi,xh,yh,zh,khull,kedge,kface,
     &  nh,nedge,nface,kfacelast,tol,ifail)

      if (ifail.ne.0) then
        write(lun6,*)"Error in undumag_cut_magnet: Bad return from util_convex_hull_3d_overwrite ***"
        write(lun6,*)"ifail, ical: ",ifail, ical
        write(lun6,*)"*** Date written to util_convex_hull_3d.dat ***"
        open(newunit=lunf,file='util_convex_hull_3d.dat')
        do i=1,npoi
          write(lunf,*)xh(i),yh(i),zh(i),i
        enddo
        flush(lunf)
        close(lunf)
        goto 9999
      endif

      n1=0
      n2=0
      npoi=nh

      do ipoi=1,npoi

        k=khull(ipoi)

        x=xh(k)
        y=yh(k)
        z=zh(k)

        if (
     &      modediv.eq.1.and.x.le.cut.or.
     &      modediv.eq.2.and.y.le.cut.or.
     &      modediv.eq.3.and.z.le.cut
     &      ) then
          n1=n1+1
          xh1(n1)=x
          yh1(n1)=y
          zh1(n1)=z
        endif

        if (
     &      modediv.eq.1.and.x.ge.cut.or.
     &      modediv.eq.2.and.y.ge.cut.or.
     &      modediv.eq.3.and.z.ge.cut
     &      ) then
          n2=n2+1
          xh2(n2)=x
          yh2(n2)=y
          zh2(n2)=z
        endif

      enddo


      do iedge=1,nedge
        k1=kedge(1,iedge)
        k2=kedge(2,iedge)
        x1=xh(k1)
        x2=xh(k2)
        y1=yh(k1)
        y2=yh(k2)
        z1=zh(k1)
        z2=zh(k2)
        if (modediv.eq.1.and.(
     &      x1.lt.cut.and.x2.gt.cut.or.
     &      x1.gt.cut.and.x2.lt.cut)) then
          n1=n1+1
          xh1(n1)=cut
          yh1(n1)=y1+(y2-y1)/(x2-x1)*(cut-x1)
          zh1(n1)=z1+(z2-z1)/(x2-x1)*(cut-x1)
          n2=n2+1
          xh2(n2)=xh1(n1)
          yh2(n2)=yh1(n1)
          zh2(n2)=zh1(n1)
        else if (modediv.eq.2.and.(
     &      y1.lt.cut.and.y2.gt.cut.or.
     &      y1.gt.cut.and.y2.lt.cut)) then
          n1=n1+1
          yh1(n1)=cut
          xh1(n1)=x1+(x2-x1)/(y2-y1)*(cut-y1)
          zh1(n1)=z1+(z2-z1)/(y2-y1)*(cut-y1)
          n2=n2+1
          xh2(n2)=xh1(n1)
          yh2(n2)=yh1(n1)
          zh2(n2)=zh1(n1)
        else if (modediv.eq.3.and.(
     &      z1.lt.cut.and.z2.gt.cut.or.
     &      z1.gt.cut.and.z2.lt.cut)) then
          n1=n1+1
          zh1(n1)=cut
          xh1(n1)=x1+(x2-x1)/(z2-z1)*(cut-z1)
          yh1(n1)=y1+(y2-y1)/(z2-z1)*(cut-z1)
          n2=n2+1
          xh2(n2)=xh1(n1)
          yh2(n2)=yh1(n1)
          zh2(n2)=zh1(n1)
        endif
      enddo

      if (n1.gt.3) then


        call util_convex_hull_3d_overwrite(kmag,n1,xh1,yh1,zh1,khull,kedge,kface,
     &    nh1,nedge,nface,kfacelast,tol,ifail)
      endif

      if (ifail.ne.0) then
        write(lun6,*)"Error in undumag_cut_magnet: Bad return from util_convex_hull_3d_overwrite ***"
        write(lun6,*)"ifail, ical: ",ifail, ical
        open(newunit=lunf,file='util_convex_hull_3d.dat')
        do i=1,n1
          write(lunf,*)xh1(i),yh1(i),zh1(i),i
        enddo
        flush(lunf)
        close(lunf)
        goto 9999
      endif


      if (nh1.gt.3) then
        x0(1)=0.0d0
        y0(1)=0.0d0
        z0(1)=0.0d0
        do icorn=1,nh1
          k=khull(icorn)
c          x0(1)=x0(1)+xh1(khull(k))
c          y0(1)=y0(1)+yh1(khull(k))
c          z0(1)=z0(1)+zh1(khull(k))
          x0(1)=x0(1)+xh1(k)
          y0(1)=y0(1)+yh1(k)
          z0(1)=z0(1)+zh1(k)
        enddo
        x0(1)=x0(1)/nh1
        y0(1)=y0(1)/nh1
        z0(1)=z0(1)/nh1
        k=1
        ncorn1=0
        do iplan=1,nface
          ncorn=kface(k)
          ncorn1(iplan)=ncorn
          do icorn=1,ncorn
            k=k+1
            corn1(1,icorn,iplan)=xh1(kface(k))-x0(1) !rel. coord.
            corn1(2,icorn,iplan)=yh1(kface(k))-y0(1)
            corn1(3,icorn,iplan)=zh1(kface(k))-z0(1)
          enddo
          k=k+1
        enddo
      else
        ncorn1=0
      endif


      nh2=n2

      if (nh2.gt.3) then

        call util_convex_hull_3d_overwrite(kmag,n2,xh2,yh2,zh2,khull,kedge,kface,
     &    nh2,nedge,nface,kfacelast,tol,ifail)

        if (ifail.ne.0) then
          write(lun6,*)"Error in undumag_cut_magnet: Bad return from util_convex_hull_3d_overwrite ***"
          write(lun6,*)"ifail, ical: ",ifail, ical
          open(newunit=lunf,file='util_convex_hull_3d.dat')
          do i=1,n2
            write(lunf,*)xh2(i),yh2(i),zh2(i),i
          enddo
          flush(lunf)
          close(lunf)
          goto 9999
        endif


        x0(2)=0.0d0
        y0(2)=0.0d0
        z0(2)=0.0d0
        do icorn=1,nh2
          k=khull(icorn)
c          x0(2)=x0(2)+xh2(khull(k))
c          y0(2)=y0(2)+yh2(khull(k))
c          z0(2)=z0(2)+zh2(khull(k))
          x0(2)=x0(2)+xh2(k)
          y0(2)=y0(2)+yh2(k)
          z0(2)=z0(2)+zh2(k)
        enddo
        x0(2)=x0(2)/nh2
        y0(2)=y0(2)/nh2
        z0(2)=z0(2)/nh2

        k=1
        iplan=0
        ncorn2=0
        do iplan=1,nface
          ncorn=kface(k)
          ncorn2(iplan)=ncorn
          do icorn=1,ncorn
            k=k+1
            corn2(1,icorn,iplan)=xh2(kface(k))-x0(2) ! rel. coord.
            corn2(2,icorn,iplan)=yh2(kface(k))-y0(2)
            corn2(3,icorn,iplan)=zh2(kface(k))-z0(2)
          enddo
          k=k+1
        enddo
      else
        ncorn2=0
      endif


      if (ncorn1(1).eq.0) then
        ncorn1=ncorn2
        corn1=corn2
        ncorn2=0
      endif

9999  deallocate(
     &  xh,yh,zh,
     &  xh1,yh1,zh1,
     &  xh2,yh2,zh2,
     &  khull,kedge,kface)

c      stop "Ende in cut"
      return
      end
