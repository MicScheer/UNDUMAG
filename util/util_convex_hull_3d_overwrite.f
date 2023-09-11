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

      implicit none

      double precision xin(*),yin(*),zin(*),tinyin,tiny

      double precision, dimension (:), allocatable ::  xh,yh,zh
      double precision, dimension (:,:), allocatable ::  vn

      integer, dimension (:), allocatable ::  ksimp,lface,mface,mhull,mbuff
      integer, dimension (:,:), allocatable ::  medge

      integer khull(*),kedge(4,*),kface(*)
      integer :: nin,n,k,kfail,i,nhull,nedge,nface,kfacelast,lunbad,
     &  nsimp,n3,modsimp=0,ipoi,j,i1,i2,npoi,ical=0,iface,imag

c+seq,debugutil.
      save ical

      ical=ical+1

      modsimp=0

      tiny=tinyin
      if (tiny.le.0.0d0) tiny=1.0d-12

      allocate(xh(nin),yh(nin),zh(nin),
     &  mbuff(nin),mhull(nin),mface((nin+1)*nin),medge(4,2*nin))

      xh(1:nin)=xin(1:nin)
      yh(1:nin)=yin(1:nin)
      zh(1:nin)=zin(1:nin)

      if (modsimp.eq.0) then

        call util_convex_hull_3d(
     &    nin,xh,yh,zh,khull,kedge,kface,
     &    nhull,nedge,nface,kfacelast,tiny,
     &    kfail)

        if (kfail.ne.0) then

          print*,""
          print*,"*** Warning in util_convex_hull_3d_overwrite:"
          print*,"*** Bad return from util_convex_hull_3d.f"
          print*,"*** Dumping points to hull.bad"
          print*,""
          open(newunit=lunbad,file='hull.bad')
          do i=1,nin
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

        n=nin
        n3=n*(n+1)**2

        allocate(ksimp(4*n3),lface(n3),vn(3,4*n3))

        call util_weed_points(n,xh,yh,zh,tiny)

        call util_convex_hull_3d_simp(n,xh,yh,zh,tiny,nhull,khull,
     &    nsimp,ksimp,nface,kface,lface,kfacelast,nedge,kedge,vn,kfail)

        if (kfail.ne.0) then
          print*,""
          print*,"*** Error in util_convex_hull_3d_overwrite:"
          print*,"*** Bad return from util_convex_hull_3d_simp.f"
          return
        endif

        deallocate(ksimp,lface,vn)

      endif !modsimp

      mhull(1:nhull)=khull(1:nhull)
      mface(1:kfacelast)=kface(1:kfacelast)
      medge(1:4,1:nedge)=kedge(1:4,1:nedge)

      n=nhull
      do i=1,n
        khull(i)=i
        mbuff(mhull(i))=i
        xin(i)=xh(mhull(i))
        yin(i)=yh(mhull(i))
        zin(i)=zh(mhull(i))
      enddo

      k=0
      iface=0
      do while (k.lt.kfacelast)
        k=k+1
        iface=iface+1
        npoi=mface(k)
        kface(k)=npoi
        do ipoi=1,npoi
          k=k+1
c          print*,ical,iface,k,mface(i)
          kface(k)=mbuff(mface(k))
        enddo
C        if (k.ge.kfacelast) exit
      enddo

      do i=1,nedge
        kedge(1:2,i)=medge(1,mbuff(medge(1:2,i)))
      enddo

      deallocate(xh,yh,zh,mbuff,mhull,mface,medge)

c      call util_break

c      i_debug=imag

      return
      end
