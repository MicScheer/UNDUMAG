*CMZ :  2.02/01 10/11/2021  10.13.19  by  Michael Scheer
*CMZ :  2.02/00 29/03/2021  08.13.23  by  Michael Scheer
*CMZ :  1.25/00 29/01/2018  11.05.20  by  Michael Scheer
*CMZ :  1.24/01 16/10/2017  19.14.21  by  Michael Scheer
*CMZ :  1.20/01 22/06/2017  12.00.01  by  Michael Scheer
*CMZ :  1.18/02 13/06/2017  14.33.56  by  Michael Scheer
*CMZ :  1.11/03 16/01/2017  12.22.22  by  Michael Scheer
*CMZ :  1.11/02 10/01/2017  10.19.31  by  Michael Scheer
*CMZ :  1.11/01 06/01/2017  10.21.57  by  Michael Scheer
*CMZ :  1.11/00 04/01/2017  15.29.49  by  Michael Scheer
*CMZ :  1.04/00 13/09/2016  17.22.01  by  Michael Scheer
*-- Author :    Michael Scheer   13/08/2004
      subroutine undumag_force

*KEEP,bpolyederf90u.
      include 'bpolyederf90u.cmn'
*KEND.

      use commandlinef90m

      implicit none

*KEEP,phyconparam.
      include 'phyconparam.cmn'
*KEND.

      integer ical
      data ical/0/

      integer npoix,npoiy,npoiz,iy,iz,ix,i,ifail,lunfor,lunbox,
     &  idiv

      double precision bfint(3),xx,yy,zz,xbase,ybase,zbase
     &  ,dx,dy,dz,bx,by,bz,bnx,bny,bnz
     &  ,f(3),r(3),t(3),result,
     &  rx,ry,rz,torqtotx,torqtoty,torqtotz,torqrotx,torqroty,torqrotz,
     &  bflenydiv,bftop

      double precision, dimension(:),allocatable ::
     &  xb,yb,ws1,ws2,ws3,ws4,coef,zb

      double precision, dimension(:,:),allocatable :: bb,bbi,bt,bti,fortorq

      integer nbfdimp,kstop
      double precision
     &  bforcx(7),bforcy(7),bforcz(7),
     &  torqx(7),torqy(7),torqz(7),ftsum(6)

      save

      if (nbforcx.lt.0) then
        kstop=1
        nbforcx=-nbforcx
      else
        kstop=0
      endif

      call util_zeit_kommentar(lun6,"Starting force calculations")

      nbfdimp=max(nbforcx,nbforcy,nbforcz)+1

      allocate(fortorq(7,ndivfby))
      fortorq=0.0d0

      allocate(xb(nbfdimp))
      allocate(yb(nbfdimp))
      allocate(zb(nbfdimp))
      allocate(ws1(nbfdimp))
      allocate(ws2(nbfdimp))
      allocate(ws3(nbfdimp))
      allocate(ws4(nbfdimp))
      allocate(coef(nbfdimp))

      allocate(bb(nbfdimp,3))
      allocate(bbi(nbfdimp,3))
      allocate(bt(nbfdimp,3))
      allocate(bti(nbfdimp,3))

      if (ical.eq.0) then

        if (jplforce.ne.0) then
          open(newunit=lunbox,file='undumag_force.fbx')
          write(lunbox,*)'* Run',kundurun
          write(lunbox,*)'* x y z Bx By Bz nx ny nz ifail'
        endif

        npoix=nbforcx+1
        npoiy=mod(nbforcy,ndivfby)
        if (npoiy.ne.0) then
          nbforcy=nbforcy-npoiy+ndivfby
        endif
        npoiy=nbforcy/ndivfby+1
        npoiz=nbforcz+1

        if (nbforcx.ne.0) then
          dx=bflenx/nbforcx
        else
          dx=0.0
        endif

        if (nbforcy.ne.0) then
          dy=bfleny/nbforcy
        else
          dy=0.0
        endif

        if (nbforcz.ne.0) then
          dz=bflenz/nbforcz
        else
          dz=0.0
        endif

        xbase=bfcenx-bflenx/2.d0
        ybase=bfceny-bfleny/2.d0
        zbase=bfcenz-bflenz/2.d0

        bflenydiv=bfleny/ndivfby
        bftop=ybase+bfleny

        ical=1

      endif !ical

      ftsum=0.0d0

      do idiv=1,ndivfby

        bforcx=0.d0
        bforcy=0.d0
        bforcz=0.d0
        torqx=0.d0
        torqy=0.d0
        torqz=0.d0

        ybase=bftop-dble(idiv)*bflenydiv
        fortorq(7,idiv)=ybase+bflenydiv/2.0d0

c first yz-plane, normal vector is (-1,0,0){

      bnx=-1.d0
      bny=0.d0
      bnz=0.d0

      xx=xbase
      yy=ybase-dy

      do iy=1,npoiy

        yy=yy+dy
        yb(iy)=yy
        zz=zbase-dz

        do iz=1,npoiz

          zz=zz+dz
          call undumag_fbfield(1,xx,yy,zz,bx,by,bz,ifail)

          if (jplforce.ne.0) then
            write(lunbox,'(6e15.5e3,4i3)')xx*1000.,yy*1000.,zz*1000.,
     &        bx,by,bz,nint(bnx),nint(bny),nint(bnz),ifail
          endif

          zb(iz)=zz
          bb(iz,1)=bx*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnx)
          bb(iz,2)=by*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bny)
          bb(iz,3)=bz*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnz)

          f(1)=bb(iz,1)
          f(2)=bb(iz,2)
          f(3)=bb(iz,3)

          r(1)=xx-bfcenx
          r(2)=yy-bfceny
          r(3)=zz-bfcenz

          call util_vcross(r,f,t)

          bt(iz,1)=t(1)
          bt(iz,2)=t(2)
          bt(iz,3)=t(3)

        enddo !iz

        call util_spline_or_simpson_integral(zb,bb(1,1),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(iy,1)=result
        call util_spline_or_simpson_integral(zb,bb(1,2),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(iy,2)=result
        call util_spline_or_simpson_integral(zb,bb(1,3),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(iy,3)=result

        call util_spline_or_simpson_integral(zb,bt(1,1),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(iy,1)=result
        call util_spline_or_simpson_integral(zb,bt(1,2),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(iy,2)=result
        call util_spline_or_simpson_integral(zb,bt(1,3),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(iy,3)=result

      enddo !iy

      call util_spline_or_simpson_integral(yb,bbi(1,1),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(yb,bbi(1,2),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(yb,bbi(1,3),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      bforcx(1)=bfint(1)/rmu01
      bforcy(1)=bfint(2)/rmu01
      bforcz(1)=bfint(3)/rmu01

      call util_spline_or_simpson_integral(yb,bti(1,1),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(yb,bti(1,2),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(yb,bti(1,3),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      torqx(1)=bfint(1)/rmu01
      torqy(1)=bfint(2)/rmu01
      torqz(1)=bfint(3)/rmu01

c }first yz-plane, normal vector is (-1,0,0)

c second yz-plane, normal vector is (1,0,0){

      bnx=1.d0
      bny=0.d0
      bnz=0.d0

      xx=xbase+bflenx
      yy=ybase-dy

      do iy=1,npoiy

        yy=yy+dy
        yb(iy)=yy
        zz=zbase-dz

        do iz=1,npoiz

          zz=zz+dz
          call undumag_fbfield(3,xx,yy,zz,bx,by,bz,ifail)

          if (jplforce.ne.0) then
            write(lunbox,'(6e15.5e3,4i3)')xx*1000.,yy*1000.,zz*1000.,
     &        bx,by,bz,nint(bnx),nint(bny),nint(bnz),ifail
          endif

          zb(iz)=zz
          bb(iz,1)=bx*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnx)
          bb(iz,2)=by*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bny)
          bb(iz,3)=bz*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnz)

          f(1)=bb(iz,1)
          f(2)=bb(iz,2)
          f(3)=bb(iz,3)

          r(1)=xx-bfcenx
          r(2)=yy-bfceny
          r(3)=zz-bfcenz

          call util_vcross(r,f,t)

          bt(iz,1)=t(1)
          bt(iz,2)=t(2)
          bt(iz,3)=t(3)

        enddo !iz

        call util_spline_or_simpson_integral(zb,bb(1,1),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(iy,1)=result
        call util_spline_or_simpson_integral(zb,bb(1,2),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(iy,2)=result
        call util_spline_or_simpson_integral(zb,bb(1,3),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(iy,3)=result

        call util_spline_or_simpson_integral(zb,bt(1,1),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(iy,1)=result
        call util_spline_or_simpson_integral(zb,bt(1,2),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(iy,2)=result
        call util_spline_or_simpson_integral(zb,bt(1,3),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(iy,3)=result

      enddo !iy

      call util_spline_or_simpson_integral(yb,bbi(1,1),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(yb,bbi(1,2),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(yb,bbi(1,3),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      bforcx(3)=bfint(1)/rmu01
      bforcy(3)=bfint(2)/rmu01
      bforcz(3)=bfint(3)/rmu01

      call util_spline_or_simpson_integral(yb,bti(1,1),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(yb,bti(1,2),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(yb,bti(1,3),npoiy,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      torqx(3)=bfint(1)/rmu01
      torqy(3)=bfint(2)/rmu01
      torqz(3)=bfint(3)/rmu01

c } second yz-plane, normal vector is (1,0,0)

c first xz-plane, normal vector is (0,-1,0){

      bnx=0.d0
      bny=-1.d0
      bnz=0.d0

      xx=xbase-dx
      yy=ybase

      do ix=1,npoix

        xx=xx+dx
        xb(ix)=xx
        zz=zbase-dz

        do iz=1,npoiz

          zz=zz+dz
          call undumag_fbfield(2,xx,yy,zz,bx,by,bz,ifail)

          if (jplforce.ne.0) then
            write(lunbox,'(6e15.5e3,4i3)')xx*1000.,yy*1000.,zz*1000.,
     &        bx,by,bz,nint(bnx),nint(bny),nint(bnz),ifail
          endif

          zb(iz)=zz
          bb(iz,1)=bx*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnx)
          bb(iz,2)=by*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bny)
          bb(iz,3)=bz*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnz)

          f(1)=bb(iz,1)
          f(2)=bb(iz,2)
          f(3)=bb(iz,3)

          r(1)=xx-bfcenx
          r(2)=yy-bfceny
          r(3)=zz-bfcenz

          call util_vcross(r,f,t)

          bt(iz,1)=t(1)
          bt(iz,2)=t(2)
          bt(iz,3)=t(3)

        enddo !iz

        call util_spline_or_simpson_integral(zb,bb(1,1),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,1)=result
        call util_spline_or_simpson_integral(zb,bb(1,2),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,2)=result
        call util_spline_or_simpson_integral(zb,bb(1,3),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,3)=result

        call util_spline_or_simpson_integral(zb,bt(1,1),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,1)=result
        call util_spline_or_simpson_integral(zb,bt(1,2),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,2)=result
        call util_spline_or_simpson_integral(zb,bt(1,3),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,3)=result

      enddo !ix

      call util_spline_or_simpson_integral(xb,bbi(1,1),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(xb,bbi(1,2),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(xb,bbi(1,3),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      bforcx(2)=bfint(1)/rmu01
      bforcy(2)=bfint(2)/rmu01
      bforcz(2)=bfint(3)/rmu01

      call util_spline_or_simpson_integral(xb,bti(1,1),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(xb,bti(1,2),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(xb,bti(1,3),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      torqx(2)=bfint(1)/rmu01
      torqy(2)=bfint(2)/rmu01
      torqz(2)=bfint(3)/rmu01

c }first xz-plane, normal vector is (0,-1,0)

c second xz-plane, normal vector is (0,+1,0){

      bnx=0.d0
      bny=1.d0
      bnz=0.d0

      xx=xbase-dx
      yy=ybase+bflenydiv

      do ix=1,npoix

        xx=xx+dx
        xb(ix)=xx
        zz=zbase-dz

        do iz=1,npoiz

          zz=zz+dz
          call undumag_fbfield(4,xx,yy,zz,bx,by,bz,ifail)

          if (jplforce.ne.0) then
            write(lunbox,'(6e15.5e3,4i3)')xx*1000.,yy*1000.,zz*1000.,
     &        bx,by,bz,nint(bnx),nint(bny),nint(bnz),ifail
          endif

          zb(iz)=zz
          bb(iz,1)=bx*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnx)
          bb(iz,2)=by*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bny)
          bb(iz,3)=bz*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnz)

          f(1)=bb(iz,1)
          f(2)=bb(iz,2)
          f(3)=bb(iz,3)

          r(1)=xx-bfcenx
          r(2)=yy-bfceny
          r(3)=zz-bfcenz

          call util_vcross(r,f,t)

          bt(iz,1)=t(1)
          bt(iz,2)=t(2)
          bt(iz,3)=t(3)

        enddo !iz

        call util_spline_or_simpson_integral(zb,bb(1,1),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,1)=result
        call util_spline_or_simpson_integral(zb,bb(1,2),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,2)=result
        call util_spline_or_simpson_integral(zb,bb(1,3),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,3)=result

        call util_spline_or_simpson_integral(zb,bt(1,1),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,1)=result
        call util_spline_or_simpson_integral(zb,bt(1,2),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,2)=result
        call util_spline_or_simpson_integral(zb,bt(1,3),npoiz,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,3)=result

      enddo !ix

      call util_spline_or_simpson_integral(xb,bbi(1,1),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(xb,bbi(1,2),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(xb,bbi(1,3),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      bforcx(4)=bfint(1)/rmu01
      bforcy(4)=bfint(2)/rmu01
      bforcz(4)=bfint(3)/rmu01

      call util_spline_or_simpson_integral(xb,bti(1,1),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(xb,bti(1,2),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(xb,bti(1,3),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      torqx(4)=bfint(1)/rmu01
      torqy(4)=bfint(2)/rmu01
      torqz(4)=bfint(3)/rmu01

c }second xz-plane, normal vector is (0,+1,0)

c first xy-plane, normal vector is (0,0,-1){

      bnx=0.d0
      bny=0.d0
      bnz=-1.d0

      xx=xbase-dx
      zz=zbase

      do ix=1,npoix

        xx=xx+dx
        xb(ix)=xx
        yy=ybase-dy

        do iy=1,npoiy

          yy=yy+dy
          call undumag_fbfield(5,xx,yy,zz,bx,by,bz,ifail)

          if (jplforce.ne.0) then
            write(lunbox,'(6e15.5e3,4i3)')xx*1000.,yy*1000.,zz*1000.,
     &        bx,by,bz,nint(bnx),nint(bny),nint(bnz),ifail
          endif

          yb(iy)=yy
          bb(iy,1)=bx*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnx)
          bb(iy,2)=by*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bny)
          bb(iy,3)=bz*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnz)

          f(1)=bb(iy,1)
          f(2)=bb(iy,2)
          f(3)=bb(iy,3)

          r(1)=xx-bfcenx
          r(2)=yy-bfceny
          r(3)=zz-bfcenz

          call util_vcross(r,f,t)

          bt(iy,1)=t(1)
          bt(iy,2)=t(2)
          bt(iy,3)=t(3)

        enddo !iy

        call util_spline_or_simpson_integral(yb,bb(1,1),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,1)=result
        call util_spline_or_simpson_integral(yb,bb(1,2),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,2)=result
        call util_spline_or_simpson_integral(yb,bb(1,3),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,3)=result

        call util_spline_or_simpson_integral(yb,bt(1,1),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,1)=result
        call util_spline_or_simpson_integral(yb,bt(1,2),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,2)=result
        call util_spline_or_simpson_integral(yb,bt(1,3),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,3)=result

      enddo !ix

      call util_spline_or_simpson_integral(xb,bbi(1,1),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(xb,bbi(1,2),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(xb,bbi(1,3),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      bforcx(5)=bfint(1)/rmu01
      bforcy(5)=bfint(2)/rmu01
      bforcz(5)=bfint(3)/rmu01

      call util_spline_or_simpson_integral(xb,bti(1,1),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(xb,bti(1,2),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(xb,bti(1,3),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      torqx(5)=bfint(1)/rmu01
      torqy(5)=bfint(2)/rmu01
      torqz(5)=bfint(3)/rmu01

c }first xy-plane, normal vector is (0,0,-1)

c second xy-plane, normal vector is (0,0,+1){

      bnx=0.d0
      bny=0.d0
      bnz=1.d0

      xx=xbase-dx
      zz=zbase+bflenz

      do ix=1,npoix

        xx=xx+dx
        xb(ix)=xx
        yy=ybase-dy

        do iy=1,npoiy

          yy=yy+dy
          call undumag_fbfield(6,xx,yy,zz,bx,by,bz,ifail)

          if (jplforce.ne.0) then
            write(lunbox,'(6e15.5e3,4i3)')xx*1000.,yy*1000.,zz*1000.,
     &        bx,by,bz,nint(bnx),nint(bny),nint(bnz),ifail
          endif

          yb(iy)=yy
          bb(iy,1)=bx*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnx)
          bb(iy,2)=by*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bny)
          bb(iy,3)=bz*(bx*bnx+by*bny+bz*bnz)
     &      -0.5d0*((bx*bx+by*by+bz*bz)*bnz)

          f(1)=bb(iy,1)
          f(2)=bb(iy,2)
          f(3)=bb(iy,3)

          r(1)=xx-bfcenx
          r(2)=yy-bfceny
          r(3)=zz-bfcenz

          call util_vcross(r,f,t)

          bt(iy,1)=t(1)
          bt(iy,2)=t(2)
          bt(iy,3)=t(3)

        enddo !iy

        call util_spline_or_simpson_integral(yb,bb(1,1),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,1)=result
        call util_spline_or_simpson_integral(yb,bb(1,2),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,2)=result
        call util_spline_or_simpson_integral(yb,bb(1,3),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bbi(ix,3)=result

        call util_spline_or_simpson_integral(yb,bt(1,1),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,1)=result
        call util_spline_or_simpson_integral(yb,bt(1,2),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,2)=result
        call util_spline_or_simpson_integral(yb,bt(1,3),npoiy,result
     &    ,coef,ws1,ws2,ws3,ws4)
        bti(ix,3)=result

      enddo !ix

      call util_spline_or_simpson_integral(xb,bbi(1,1),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(xb,bbi(1,2),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(xb,bbi(1,3),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      bforcx(6)=bfint(1)/rmu01
      bforcy(6)=bfint(2)/rmu01
      bforcz(6)=bfint(3)/rmu01

      call util_spline_or_simpson_integral(xb,bti(1,1),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(1)=result
      call util_spline_or_simpson_integral(xb,bti(1,2),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(2)=result
      call util_spline_or_simpson_integral(xb,bti(1,3),npoix,result
     &  ,coef,ws1,ws2,ws3,ws4)
      bfint(3)=result

      torqx(6)=bfint(1)/rmu01
      torqy(6)=bfint(2)/rmu01
      torqz(6)=bfint(3)/rmu01

c }second xy-plane, normal vector is (0,0,+1)

      do i=1,6
        bforcx(7)=bforcx(7)+bforcx(i)
        bforcy(7)=bforcy(7)+bforcy(i)
        bforcz(7)=bforcz(7)+bforcz(i)
        torqx(7)=torqx(7)+torqx(i)
        torqy(7)=torqy(7)+torqy(i)
        torqz(7)=torqz(7)+torqz(i)
c        write(lun6,*)idiv,i,bforcx(i),bforcy(i)
      enddo

      fortorq(1,idiv)=bforcx(7)
      fortorq(2,idiv)=bforcy(7)
      fortorq(3,idiv)=bforcz(7)
      fortorq(4,idiv)=torqx(7)
      fortorq(5,idiv)=torqy(7)
      fortorq(6,idiv)=torqz(7)

      ftsum(1:3)=ftsum(1:3)+fortorq(1:3,idiv)
      ftsum(4:6)=ftsum(4:6)+fortorq(4:6,idiv)

      enddo !idiv=1,ndivfby

      bforcx(7)=ftsum(1)
      bforcy(7)=ftsum(2)
      bforcz(7)=ftsum(3)

      torqx(7)=ftsum(4)
      torqy(7)=ftsum(5)
      torqz(7)=ftsum(6)

      rx=bfcenx-torqcenx
      ry=bfceny-torqceny
      rz=bfcenz-torqcenz

      torqrotx=ry*bforcz(7)-rz*bforcy(7)
      torqroty=rz*bforcx(7)-rx*bforcz(7)
      torqrotz=rx*bforcy(7)-ry*bforcx(7)

      torqtotx=torqrotx+torqx(7)
      torqtoty=torqroty+torqy(7)
      torqtotz=torqrotz+torqz(7)

      write(lun6,*)
      write(lun6,*)'     undumag_force:'
      write(lun6,*)

      write(lun6,*)'     box parameters:'
      write(lun6,*)'     ubfcenx, ubfceny, ubfcenz:'
     &  ,sngl(bfcenx*1000.0d0),sngl(bfceny*1000.0d0),sngl(bfcenz*1000.0d0)
      write(lun6,*)'     ubflenx, ubfleny, ubflenz:'
     &  ,sngl(bflenx*1000.0d0),sngl(bfleny*1000.0d0),sngl(bflenz*1000.0d0)
      write(lun6,*)'     reference point for total torque:'
      write(lun6,*)'     '
     &  ,sngl(torqcenx*1000.0d0),sngl(torqceny*1000.0d0),sngl(torqcenz*1000.0d0)
      write(lun6,*)'     mbforcx, mbforcy, mbforcz:'
     &  ,nbforcx,nbforcy,nbforcz
      write(lun6,*)
      write(lun6,*)'                     force [N]       torque [Nmm]'
      write(lun6,*)
      write(lun6,'(a,3f10.3)')
     &  '         x:         ',bforcx(7),torqx(7)*1000.0d0
      write(lun6,'(a,3f10.3)')
     &  '         y:         ',bforcy(7),torqy(7)*1000.0d0
      write(lun6,'(a,3f10.3)')
     &  '         z:         ',bforcz(7),torqz(7)*1000.0d0
      write(lun6,*)
      write(lun6,*)

      write(lun6,*)
      write(lun6,*)
     &  '              torque due to r x f:         '
      write(lun6,'(a,3f10.3)')
     &  '                        ',torqrotx*1000.0d0,
     &  torqroty*1000.0d0,torqrotz*1000.0d0
      write(lun6,*)
     &  '              total torque:         '
      write(lun6,'(a,3f10.3)')
     &  '                        ',torqtotx*1000.0d0,
     &  torqtoty*1000.0d0,torqtotz*1000.0d0
      write(lun6,*)
      write(lun6,*)

      open(newunit=lunfor,file='undumag.frc')

      write(lunfor,*)'* Run'
      write(lunfor,*)kundurun
      write(lunfor,*)"* ubfcenx, ubfceny, ubfcenz [mm]"
      write(lunfor,'(3f10.5)')bfcenx*1000.0d0,bfceny*1000.0d0,bfcenz*1000.0d0
      write(lunfor,*)"* ubflenx, ubfleny, ubflenz [mm]"
      write(lunfor,'(3f10.5)')bflenx*1000.0d0,bfleny*1000.0d0,bflenz*1000.0d0
      write(lunfor,*)"* utorqcenx, utorqceny, utorqcenz [Nmm]"
      write(lunfor,'(3f10.5)')torqcenx*1000.0d0, torqceny*1000.0d0, torqcenz*1000.0d0
      write(lunfor,*)'* mbforcx, mbforcy, mbforcz'
      write(lunfor,*)nbforcx,nbforcy,nbforcz
      write(lunfor,*)'* Fx, Fy, Fz [N]'
      write(lunfor,'(3f10.3)')bforcx(7),bforcy(7),bforcz(7)
      write(lunfor,*)'* Tx, Ty, Tz [Nmm]'
      write(lunfor,'(3f10.3)')torqx(7)*1000.0d0,torqy(7)*1000.0d0,torqz(7)*1000.0d0
      write(lunfor,*)'* r x F: Tx, Ty, Tz [Nmm]'
      write(lunfor,'(3f10.3)')torqrotx*1000.0d0,torqroty*1000.0d0,torqrotz
      write(lunfor,*)'* TxTot, TyTot, TzTot [Nmm]'
      write(lunfor,'(3f10.3)')torqtotx*1000.0d0,torqtoty*1000.0d0,
     &  torqtotz*1000.0d0

      write(lunfor,*)' '
      write(lunfor,*)'* Vertikal division of box'
      write(lunfor,*)'* Idiv y Fx Fy Fz Tx Ty Tz [mm and N]'
      write(lunfor,*)' '
      do idiv=1,ndivfby
        write(lunfor,'(i5,7f12.5)')idiv,
     &    fortorq(7,idiv)*1000.0d0,
     &    fortorq(1:3,idiv),
     &    fortorq(4:6,idiv)*1000.0d0
      enddo
c      write(lunfor,*)' '
c      write(lunfor,'(a,6f10.5)')"sum:",ftsum

      close(lunfor)

      deallocate(xb)
      deallocate(yb)
      deallocate(zb)
      deallocate(ws1)
      deallocate(ws2)
      deallocate(ws3)
      deallocate(ws4)
      deallocate(coef)

      deallocate(bb)
      deallocate(bbi)
      deallocate(bt)
      deallocate(bti)

      if (kstop.ne.0) then
        write(lun6,*)'--- UNDUMAG stopped due to negative value of nbforcx ---'
        stop
      endif

      close(lunbox)
      call util_zeit_kommentar(lun6,"Force calculations finished")

      deallocate(fortorq)

      return
      end
