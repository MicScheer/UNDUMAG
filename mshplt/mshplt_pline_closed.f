*CMZ :  0.01/03 22/09/2014  20.54.58  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.18.49  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_pline_closed(n,x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer n
      real x(*),y(*)

      if (n.le.1) return

      call mshplt_pline(n,x,y)
      call mshplt_line_raw(x(n),y(n),x(1),y(1))

      return
      end
