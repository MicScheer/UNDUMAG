*CMZ :  2.04/23 27/09/2023  09.36.23  by  Michael Scheer
*CMZ :  2.04/22 26/09/2023  16.33.57  by  Michael Scheer
*CMZ :  2.04/17 11/09/2023  20.47.28  by  Michael Scheer
*CMZ :  2.04/16 11/09/2023  10.30.11  by  Michael Scheer
*CMZ :  2.04/09 15/08/2023  12.14.13  by  Michael Scheer
*CMZ :  2.04/04 06/03/2023  09.47.47  by  Michael Scheer
*CMZ :  2.04/03 02/03/2023  07.55.42  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  16.23.46  by  Michael Scheer
*-- Author :    Michael Scheer   25/02/2023
      subroutine util_check_hull_3d(
     &  nin,xin,yin,zin,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,tinyin,
     &  kfail)

      implicit none

      double precision xin(*),yin(*),zin(*),tinyin,tiny,gcen(3),
     &  p1(3),p2(3),p3(3),vnorm(3),pcen(3)

      integer khull(*),kedge(4,*),kface(*)
      integer :: i,nin,iface,ipoi,nhull,nedge,nface,kfacelast,npoi,kfail,k,kpoi,
     &  ical=0,idebug=0

c+seq,debugutil.
      save ical

      ical=ical+1
c      if (ical.eq.10) idebug=1

      if (idebug.ne.0) then
        print*
        print*,"Check:",ical
        print*
c      return
      endif

      kfail=0

      tiny=tinyin
      if (tiny.le.0.0d0) tiny=1.0d-12

      gcen=0.0d0
      do i=1,nhull
        k=khull(i)
        gcen=gcen+[xin(k),yin(k),zin(k)]
        if (idebug.ne.0) then
          write(77,*)xin(k),yin(k),zin(k),i,'0',ical
        endif
      enddo

      if (idebug.ne.0) then
        flush(77)
      endif

      gcen=gcen/dble(nhull)

      k=0
      iface=0
      do i=1,kfacelast
        k=k+1
        if (k.gt.kfacelast) exit
        iface=iface+1
        npoi=kface(k)
        do ipoi=1,npoi
          k=k+1
          kpoi=kface(k)
          if(ipoi.eq.1)then
            p1=[xin(kpoi),yin(kpoi),zin(kpoi)]
          else if(ipoi.eq.2)then
            p2=[xin(kpoi),yin(kpoi),zin(kpoi)]
          else if(ipoi.eq.3)then
            p3=[xin(kpoi),yin(kpoi),zin(kpoi)]
          else
            p1=p2
            p2=p3
            p3=[xin(kpoi),yin(kpoi),zin(kpoi)]
          endif

          if (ipoi.lt.3) cycle

          pcen=(p1+p2+p3)/dble(3)
          call util_vcross(p2-p1,p3-p2,vnorm)
          vnorm=vnorm/norm2(vnorm)

          if(dot_product((gcen-pcen),vnorm).gt.tiny) then
            print*,"*** Error in util_check_hull_3d, ical,iface:",ical,iface
            if (idebug.ne.0) then
              write(77,*)p1,ipoi-2,iface,-ical
              write(77,*)p2,ipoi-1,iface,-ical
              write(77,*)p3,ipoi,iface,-ical
              print*,iface
              print*,p2-p1
              print*,p3-p2
              print*,vnorm
              print*,dot_product((gcen-pcen),vnorm)
            endif
            kfail=1
          endif
        enddo
      enddo

      if (idebug.ne.0) flush(77)

      return
      end
