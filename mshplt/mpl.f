*CMZ :  0.00/06 20/08/2014  09.34.38  by  Michael Scheer
*CMZ :  1.17/00 01/05/2014  09.49.35  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  14.57.09  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mpl(n,x,y)

      implicit none

      integer n
      real x(n),y(n)


      call mshplt_pline(n,x,y)
      return
      end
