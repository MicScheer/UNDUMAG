*CMZ :  2.04/22 26/09/2023  21.22.26  by  Michael Scheer
*CMZ :  2.04/17 11/09/2023  20.47.28  by  Michael Scheer
*CMZ :  2.04/16 11/09/2023  10.30.11  by  Michael Scheer
*CMZ :  2.04/09 15/08/2023  12.14.13  by  Michael Scheer
*CMZ :  2.04/04 06/03/2023  09.47.47  by  Michael Scheer
*CMZ :  2.04/03 02/03/2023  07.55.42  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  16.23.46  by  Michael Scheer
*-- Author :    Michael Scheer   25/02/2023
      subroutine util_convex_hull_3d_overwrite(imag,
     &  nin,xin,yin,zin,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,tinyin,
     &  kfail)

      ! like  util_convex_hull_3d, but overwrites xin,yin,zin

      use utilmod

      implicit none

      double precision xin(*),yin(*),zin(*),tinyin,tiny

      double precision, dimension (:), allocatable ::  xh,yh,zh
      double precision, dimension (:,:), allocatable ::  vn

      integer, dimension (:), allocatable ::  ksimp,lface

      integer khull(*),kedge(4,*),kface(*)
      integer :: nin,n,k,kfail,i,nhull,nedge,nface,kfacelast,lunbad,
     &  nsimp,n3,ipoi,j,i1,i2,npoi,ical=0,iface,imag,icycle,idebug=0,ifail

c+seq,debugutil.
      save ical

      ical=ical+1
      npoi=nin

      tiny=tinyin
      if (tiny.le.0.0d0) tiny=1.0d-12

      allocate(xh(npoi),yh(npoi),zh(npoi))

      xh(1:npoi)=xin(1:npoi)
      yh(1:npoi)=yin(1:npoi)
      zh(1:npoi)=zin(1:npoi)

      do icycle=1,2

        if (modsimp.eq.0) then

          call util_convex_hull_3d(
     &      npoi,xh,yh,zh,khull,kedge,kface,
     &      nhull,nedge,nface,kfacelast,tiny,
     &      kfail)

          if (kfail.ne.0) then

            print*,""
            print*,"*** Warning in util_convex_hull_3d_overwrite:"
            print*,"*** Bad return from util_convex_hull_3d.f"
            print*,"*** Dumping points to hull.bad"
            print*,""
            open(newunit=lunbad,file='hull.bad')
            do i=1,npoi
              write(lunbad,*) xin(i),yin(i),zin(i),i
            enddo
            flush(lunbad)
            close(lunbad)
            print*,"*** Trying util_convex_hull_3d_simp.f"
            print*,""

            modsimp=2

          endif

        endif

        if (modsimp.ne.0) then

          n=npoi
          n3=n*(n+1)**2

          allocate(ksimp(4*n3),lface(n3),vn(3,4*n3))

          call util_weed_points(n,xh,yh,zh,tiny)

          call util_convex_hull_3d_simp(n,xh,yh,zh,tiny,nhull,khull,
     &      nsimp,ksimp,nface,kface,lface,kfacelast,nedge,kedge,vn,kfail)

          if (kfail.ne.0) then
            print*,""
            print*,"*** Error in util_convex_hull_3d_overwrite:"
            print*,"*** Bad return from util_convex_hull_3d_simp.f"
            return
          else if (modsimp.eq.2) then
            print*,"--> Success"
          endif

          deallocate(ksimp,lface,vn)

        endif !modsimp

        do i=1,nhull
          xin(i)=xh(khull(i))
          yin(i)=yh(khull(i))
          zin(i)=zh(khull(i))
        enddo

        npoi=nhull
        xh(1:npoi)=xin(1:npoi)
        yh(1:npoi)=yin(1:npoi)
        zh(1:npoi)=zin(1:npoi)

        if (idebug.ne.0) then

          call util_check_hull_3d(
     &      npoi,xin,yin,zin,khull,kedge,kface,
     &      nhull,nedge,nface,kfacelast,tinyin,
     &      ifail)

          if (ifail.ne.0) then
            if (icycle.eq.1) then
              do i=1,nin
                write(45,*)xin(i),yin(i),zin(i),i,ical
              enddo
              do i=1,nhull
                write(46,*)xh(khull(i)),yh(khull(i)),zh(khull(i)),i,ical
              enddo
            else
              do i=1,nin
                write(45,*)xin(i),yin(i),zin(i),i,-ical
              enddo
              do i=1,nhull
                write(46,*)xh(khull(i)),yh(khull(i)),zh(khull(i)),i,-ical
              enddo
            endif
          endif
        endif

      enddo

      deallocate(xh,yh,zh)

c      call util_break

c      i_debug=imag

      if (idebug.ne.0) then
        call util_check_hull_3d(
     &    npoi,xin,yin,zin,khull,kedge,kface,
     &    nhull,nedge,nface,kfacelast,tinyin,
     &    ifail)
      endif

      if (modsimp.eq.2) modsimp=0

      return
      end
