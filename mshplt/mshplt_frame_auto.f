*CMZ :  1.04/00 11/02/2025  17.16.24  by  Michael Scheer
*CMZ :  1.03/03 31/10/2018  12.03.15  by  Michael Scheer
*-- Author :    Michael Scheer   31/07/2018
      subroutine mshplt_frame_auto(n,x,y,xtit,ytit,chopt)

      implicit none

c call mshplt_frame after auto-scaling

      character(*) xtit,ytit,chopt

      real x(n),y(n)
      real :: xmin, xmax, ymin, ymax, dx, dy

      integer n

      xmin=minval(x)
      xmax=maxval(x)
      ymin=minval(y)
      ymax=maxval(y)

      dx=(xmax-xmin)*0.05
      dy=(ymax-ymin)*0.05

      if (dx.eq.0.0) then
        dx=1.
      else if (dx/(abs(xmax)+abs(xmin)).lt.0.001) then
        dx=(abs(xmax)+abs(xmin))*0.0001
      endif

      if (dy.eq.0.0) then
        dy=1.
      else if (dy/(abs(ymax)+abs(ymin)).lt.0.001) then
        dy=(abs(ymax)+abs(ymin))*0.0001
      endif

      call mshplt_frame(xmin-dx,xmax+dx,ymin-dy,ymax+dy,xtit,ytit,chopt)

      return
      end
