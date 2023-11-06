*CMZ :  2.05/02 30/10/2023  09.33.51  by  Michael Scheer
*CMZ :  2.04/04 06/03/2023  09.44.39  by  Michael Scheer
*CMZ :  2.03/00 26/08/2022  13.47.55  by  Michael Scheer
*CMZ :  2.02/02 05/07/2022  10.41.27  by  Michael Scheer
*CMZ :  1.25/01 20/03/2018  12.00.00  by  Michael Scheer
*CMZ :  1.23/03 22/09/2017  16.56.04  by  Michael Scheer
*CMZ :  1.22/02 31/07/2017  14.29.57  by  Michael Scheer
*CMZ : 00.00/21 12/03/2017  15.58.39  by  Michael Scheer
*CMZ : 00.00/20 03/03/2017  14.39.52  by  Michael Scheer
*CMZ :  1.11/05 22/02/2017  14.51.09  by  Michael Scheer
*CMZ :  0.00/13 12/08/2016  14.07.32  by  Michael Scheer
*CMZ :  0.00/09 03/07/2016  18.52.20  by  Michael Scheer
*CMZ :  1.17/14 12/04/2016  13.09.48  by  Michael Scheer
*-- Author :    Michael Scheer   12/04/2016
      subroutine util_convex_hull_2d(nin,x,y,nh,ihull,tinyin,ifail)

! Returns convex hull index-array, last point [x(ihull(nh)),y(ihull(nh))]
! is first point [x(ihull(1)),y(ihull(1))]

      implicit none

      double precision, dimension (:), allocatable :: xb,yb
      double precision x(*),y(*),xmin,ymin,xsc,ysc,
     &  cosmax,cosang,vx1,vy1,vx2,vy2,v(3),v0(3),tinyin,
     &  tiny,gcenx,gceny,xmax,ymax,dx,dy,dist2,dist2max,xymin

      integer, dimension (:), allocatable :: istore,ihullb
      integer ifail,n,nh,i,ihull(*),next,i1,i2,idup,kll,nb,k,
     &  ixmin,ixmax,iymin,iymax,iflat,nin,ifailin
c+self,if=debug2d.
      integer :: ical=0
c+self.

      ifailin=ifail
c+self,if=debug2d.
      ical=ical+1
c+self.
      ifail=0

      tiny=tinyin
      if (tiny.le.0.0d0) tiny=1.0d-12

      n=nin

      if (n.eq.1) then
        ihull(1)=1
        ihull(2)=1
        nh=2
        return
      else if (n.eq.2) then
        ihull(1)=1
        ihull(2)=2
        ihull(3)=1
        nh=3
        return
      endif

      ymin=1.0d30
      ymax=-1.0d30
      kll=0
      xmin=1.0d30
      xmax=-1.0d30


      do i=1,n
        if (x(i).lt.xmin) then
          xmin=x(i)
          ixmin=i
        endif
        if (x(i).gt.xmax) then
          xmax=x(i)
          ixmax=i
        endif
        if (y(i).lt.ymin) then
          ymin=y(i)
          iymin=i
        endif
        if (y(i).gt.ymax) then
          ymax=y(i)
          iymax=i
        endif
      enddo

      if (xmax.eq.xmin) then
        ihull(1)=iymin
        ihull(2)=iymax
        ihull(3)=iymin
        nh=3
        return
      endif

      if (ymax.eq.ymin) then
        ihull(1)=ixmin
        ihull(2)=ixmax
        ihull(3)=ixmin
        nh=3
        return
      endif

      do i=2,n
        if (y(i).ne.y(1).or.x(i).ne.x(1)) then
          v0(1)=x(i)-x(1)
          v0(2)=y(i)-y(1)
          kll=i
          exit
        endif
      enddo

      iflat=1
      do i=2,n
        if (i.eq.kll) cycle
        v(1)=x(i)-x(1)
        v(2)=y(i)-y(1)
        if (
     &      (v0(2)*v(3)-v0(3)*v(2))**2+
     &      (v0(3)*v(1)-v0(1)*v(3))**2+
     &      (v0(1)*v(2)-v0(2)*v(1))**2
     &      .gt. tiny) then
          iflat=0
          exit
        endif
      enddo

      if (iflat.eq.1) then
        ihull(1)=ixmin
        ihull(2)=ixmax
        ihull(3)=ixmin
        nh=3
        return
      endif

      allocate(xb(n),yb(n),istore(n),ihullb(n+1))

      xb(1:n)=x(1:n)
      yb(1:n)=y(1:n)
      nb=n

      call util_weed_points_2d(n,x,y,tiny)

      istore=0
      do k=1,n
        do i=1,nb
          if (
     &        xb(i).eq.x(k).and.yb(i).eq.y(k)) then
            istore(k)=i
            exit
          endif
        enddo
      enddo

      xsc=1.0d0/(xmax-xmin)
      ysc=1.0d0/(ymax-ymin)

      gcenx=0.0d0
      gceny=0.0d0

      do i=1,n
        x(i)=(x(i)-xmin)*xsc
        y(i)=(y(i)-ymin)*ysc
        gcenx=gcenx+x(i)
        gceny=gceny+y(i)
      enddo

      gcenx=gcenx/n
      gceny=gceny/n

      xmax=(xmax-xmin)*xsc
      xmin=0.0d0
      ymax=(ymax-ymin)*ysc
      ymin=0.0d0

      dx=xmax-xmin
      dy=ymax-ymin

      if (dx.eq.0.0d0.or.dy.eq.0.0d0) then
        ifail=2
        goto 9999
      endif

      if (n.eq.3) then
        ihull(1)=1
        ihull(2)=2
        ihull(3)=3
        ihull(4)=1
        nh=4
        goto 9999
      endif

      xymin=1.0d30
      do i=1,n
        if (y(i).lt.ymin+tiny) then
          if (x(i).lt.xymin) then
            xymin=x(i)
            kll=i
          endif
        endif
      enddo

      i1=0

      cosmax=-10.0d0
      dist2max=-10.0d0
      do i=1,n

        if (i.eq.kll) cycle

        dx=x(i)-x(kll)
        dy=y(i)-y(kll)
        cosang=dx/sqrt(dx**2+dy**2)

        if (cosang.gt.cosmax) then
          cosmax=cosang
          dist2max=-1.0d30
        endif

        if (abs(cosang-cosmax).lt.tiny) then
          dist2=dx**2+dy**2
          if (dist2.gt.dist2max) dist2max=dist2
          if (dist2.eq.dist2max) then
            i1=i
          endif
        endif
      enddo

      if (i1.eq.0) then
        ifail=3
        goto 9999
      endif

      xmin=1.0d30
      xmax=-1.0d30

      nh=1
      ihull(nh)=kll

      next=i1

      vx1=1.0d0
      vy1=0.0d0

      do while (abs(x(next)-x(kll)).gt.tiny
     &    .or.abs(y(next)-y(kll)).gt.tiny)

        cosmax=-1.0d30

        do i=1,n

          i1=ihull(nh)
          i2=i1+i
          if (i2.gt.n) i2=i2-n

          vx2=x(i2)-x(i1)
          vy2=y(i2)-y(i1)

          dist2=vx2**2+vy2**2
          if (dist2.gt.tiny) then
            cosang=(vx1*vx2+vy1*vy2)/sqrt((vx2**2+vy2**2)*(vx1**2+vy1**2))
            if (cosang.gt.cosmax) then
              cosmax=cosang
            endif
          endif

        enddo

        dist2max=-1.0d30
        do i=1,n

          i1=ihull(nh)
          i2=i1+i
          if (i2.gt.n) i2=i2-n

          vx2=x(i2)-x(i1)
          vy2=y(i2)-y(i1)

          dist2=vx2**2+vy2**2
          if (dist2.gt.tiny) then
            cosang=(vx1*vx2+vy1*vy2)/sqrt((vx2**2+vy2**2)*(vx1**2+vy1**2))
            if (abs(cosang-cosmax).lt.tiny) then
              if (dist2.gt.dist2max) then
                dist2max=dist2
                idup=i2
              endif
              next=idup
            endif
          endif

        enddo

        nh=nh+1
        ihull(nh)=next

        vx1=x(ihull(nh))-x(ihull(nh-1))
        vy1=y(ihull(nh))-y(ihull(nh-1))

      enddo

9999  continue

      n=nb
      x(1:n)=xb(1:n)
      y(1:n)=yb(1:n)

      if (nh.gt.2.and.ifail.eq.0) then
        ihullb(1:nh)=ihull(1:nh)
        ihull(1:nh)=istore(ihullb(1:nh))
      endif

      deallocate(xb,yb,istore,ihullb)

c+self,if=debug2d.
      if (ifailin.lt.0) then
        Print*,"------------------------"
        do i=1,nh
          write(772,*)ical," 2 ",i,x(ihull(i)),y(ihull(i)),ifail
        enddo
        Print*,"------------------------"
        flush(772)
        close(772)
      endif
c+self.,if=debug2d.

      return
      end
