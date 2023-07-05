*CMZ :  2.04/04 22/11/2018  13.21.09  by  Michael Scheer
*-- Author :    Michael Scheer   22/11/2018
      subroutine util_minmax_array(n,x,xmin,xmax,kmin,kmax)

      implicit none

      real*8 x(n),xmin,xmax
      integer i,n,kmin,kmax

      xmin=1.0e30
      xmax=-1.0e30

      do i=1,n
        if (x(i).lt.xmin) then
          xmin=x(i)
          kmin=i
        endif
        if (x(i).gt.xmax) then
          xmax=x(i)
          kmax=i
        endif
      enddo

      return
      end
