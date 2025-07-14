*CMZ :          09/07/2025  14.37.52  by  Michael Scheer
*CMZ :  2.06/00 08/07/2025  12.29.27  by  Michael Scheer
*CMZ : 00.00/21 13/03/2017  11.18.51  by  Michael Scheer
*CMZ : 00.00/20 20/02/2017  22.03.44  by  Michael Scheer
*CMZ : 00.00/19 12/04/2016  09.18.40  by  Michael Scheer
*-- Author : Michael Scheer
      subroutine util_convex_hull_3d_python(nin,x,y,z,khull,kedge,kface,nhull,nedge,nface,
     &  kfacelast,ifail)

      implicit none

      integer lun

      integer :: nin

      double precision xp(nin),yp(nin),zp(nin)
      double precision x(nin),y(nin),z(nin),xx,yy,zz,
     &  xmin,xmax,ymin,ymax,zmin,zmax,gcen(3),p21(3),p32(3),vnor(3)

c      integer n,k,l,ifail,khull(nin),kedge(4,nin*nin-2),kface(2*(nin+1)*nin),
      integer n,k,l,ifail,khull(*),kedge(4,*),kface(*),
     &  nhull,nedge,nface,lface,kfacelast,i1,i2,
     &  ifoundedge,ke,i,ip(nin)

      open(newunit=lun,file='hull3d.in')
      do k=1,nin
        write(lun,*)x(k),y(k),z(k)
      enddo
      close(lun)

      call execute_command_line('python3 ../python/hull3d_main.py',.true.,ifail)

      if (ifail.ne.0) then
        return
      endif

      open(newunit=lun,file='hull3d.out')

      read(lun,*) xmin,xmax
      read(lun,*) ymin,ymax
      read(lun,*) zmin,zmax

      read(lun,*) nhull
      read(lun,*) nface

      gcen=0.0d0
      do k=1,nhull
        read(lun,*) xx,yy,zz
        gcen=gcen+[xx,yy,zz]
      enddo
      gcen=gcen/dble(nhull)

      do k=1,nhull
        read(lun,*) khull(k)
        khull(k)=khull(k)+1
      enddo

c      print*,'--------------------------------'
      kfacelast=1
      do k=1,nface
        read(lun,*) kface(kfacelast)
        i=0
c      print*,'--------------------------------'
c        print*,k,kfacelast
        do l=1,kface(kfacelast)
          read(lun,*) i1
          i1=i1+1
          i=i+1
          xp(i)=x(i1)
          yp(i)=y(i1)
          zp(i)=z(i1)
          ip(i)=i1
c          print*,i1

          kface(kfacelast+l)=i1
        enddo

        kfacelast=kfacelast+l
      enddo

      close(lun)

      lface=1
      !Euler
      nedge=nhull+nface-2
      kfacelast=1
      kedge(1:4,1:nedge)=0
      nedge=0

      do k=1,nface
        n=kface(kfacelast)
        do l=1,n
          if (l.lt.n) then
            i1=kface(kfacelast+l)
            i2=kface(kfacelast+l+1)
          else
            i1=kface(kfacelast+l)
            i2=kface(kfacelast+1)
          endif
          ifoundedge=0
          do ke=1,nedge
            if (kedge(1,ke).eq.i1.and.kedge(2,ke).eq.i2
     &        .or.
     &        kedge(1,ke).eq.i2.and.kedge(2,ke).eq.i1)
     &       then
              ifoundedge=ke
            endif
          enddo
          if (ifoundedge.eq.0) then
            nedge=nedge+1
            if (i1.lt.i2) then
              kedge(1,nedge)=i1
              kedge(2,nedge)=i2
            else
              kedge(1,nedge)=i2
              kedge(2,nedge)=i1
            endif
            kedge(3,nedge)=nface
          else
            kedge(4,ifoundedge)=nface
          endif
        enddo
        kfacelast=kfacelast+l
      enddo

      kfacelast=kfacelast-1

      if (nhull+nface-nedge.ne.2) then
        ifail=-9
      endif

      return
      end
