*CMZ :  0.00/06 20/08/2014  09.17.11  by  Michael Scheer
*CMZ :  1.17/00 26/05/2014  09.48.06  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.31.14  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mpm(n,x,y)

      implicit none

      integer n
      real x(*),y(*)


      call mshplt_marker(n,x,y)

      return
      end
