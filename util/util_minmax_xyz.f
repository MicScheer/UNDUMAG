*CMZ :  2.04/04 06/03/2023  16.01.21  by  Michael Scheer
*-- Author :    Michael Scheer   22/11/2018
      subroutine util_minmax_xzy(n,x,y,z,xmin,xmax,ymin,ymax,zmin,zmax)

      implicit none

      real*8 x(n),y(n),z(n),xmin,xmax,ymin,ymax,zmin,zmax
      integer i,n

      xmin=1.0e30
      xmax=-1.0e30
      ymin=1.0e30
      ymax=-1.0e30
      zmin=1.0e30
      zmax=-1.0e30

      do i=1,n
        if (x(i).lt.xmin) then
          xmin=x(i)
        endif
        if (x(i).gt.xmax) then
          xmax=x(i)
        endif
      enddo

      do i=1,n
        if (y(i).lt.ymin) then
          ymin=y(i)
        endif
        if (y(i).gt.ymax) then
          ymax=y(i)
        endif
      enddo

      do i=1,n
        if (z(i).lt.zmin) then
          zmin=z(i)
        endif
        if (z(i).gt.zmax) then
          zmax=z(i)
        endif
      enddo

      return
      end
