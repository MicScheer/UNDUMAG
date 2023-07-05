*CMZ :  2.04/04 06/03/2023  09.47.47  by  Michael Scheer
*CMZ :  2.04/03 02/03/2023  07.55.42  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  16.23.46  by  Michael Scheer
*-- Author :    Michael Scheer   25/02/2023
      subroutine util_convex_hull_3d_overwrite(
     &  nin,xin,yin,zin,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,tinyin,
     &  kfail)

      ! like  util_convex_hull_3d, but overwrites xin,yin,zin

      implicit none

      double precision xin(*),yin(*),zin(*),tinyin,tiny

      double precision, dimension (:), allocatable ::  xh,yh,zh

      integer khull(*),kedge(4,*),kface(*)
      integer :: nin,n,k,kfail,i,nhull,nedge,nface,kfacelast,itry,ntry=5,lunbad

      tiny=tinyin
      if (tiny.le.0.0d0) tiny=1.0d-12

      do itry=1,ntry
        call util_convex_hull_3d(
     &    nin,xin,yin,zin,khull,kedge,kface,
     &    nhull,nedge,nface,kfacelast,tiny,
     &    kfail)
        if (kfail.ne.0) then
          print*,""
          print*,"*** Warning in util_convex_hull_3d_overwrite:"
          print*,"*** Bad return from util_convex_hull_3d"
          print*,"*** Increasing tiny from ",tiny," to tiny * 2: ",tiny*2.0d0
          print*,""
          tiny=tiny*2.0d0
          cycle
        else
          exit
        endif
      enddo

      if (kfail.ne.0) then
        print*,""
        print*,"*** Error in util_convex_hull_3d_overwrite:"
        print*,"*** Bad return from util_convex_hull_3d"
        print*,"*** Giving up"
        print*,""
        return
      endif

      allocate(xh(nhull),yh(nhull),zh(nhull))

      n=nhull
      do i=1,n
        k=khull(i)
        xh(i)=xin(k)
        yh(i)=yin(k)
        zh(i)=zin(k)
      enddo

      do itry=1,ntry

        call util_convex_hull_3d(
     &    n,xh,yh,zh,khull,kedge,kface,
     &    nhull,nedge,nface,kfacelast,tiny,
     &    kfail)

         if (kfail.ne.0) then
          print*,""
          print*,"*** Warning in util_convex_hull_3d_overwrite:"
          print*,"*** Bad return from util_convex_hull_3d"
          print*,"*** Increasing tiny from ",tiny," to tiny * 2: ",tiny*2.0d0
          print*,""
          tiny=tiny*2.0d0
          cycle
        else
          exit
        endif
      enddo

      if (kfail.ne.0) then
        print*,""
        print*,"*** Error in util_convex_hull_3d_overwrite:"
        print*,"*** Bad return from util_convex_hull_3d"
        print*,"*** Giving up"
        print*,""
        print*,"*** Dumping data to hull.bad"
        print*,""
        open(newunit=lunbad,file='hull.bad')
        n=nhull
        do i=1,n
          write(lunbad,*) xh(i),yh(i),zh(i),i
        enddo
        flush(lunbad)
        close(lunbad)
        return
      endif

      n=nhull
      do i=1,n
        xin(i)=xh(i)
        yin(i)=yh(i)
        zin(i)=zh(i)
      enddo

      deallocate(xh,yh,zh)

      return
      end
