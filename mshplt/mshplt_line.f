*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  13.25.01  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_line(x1,y1,x2,y2)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(2),y(2),x1,y1,x2,y2

      x(1)=x1
      y(1)=y1
      x(2)=x2
      y(2)=y2

      call mshplt_pline(2,x,y)

      return
      end
