*CMZ :  0.01/03 22/09/2014  20.54.58  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.19.17  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_box(x1,y1,x2,y2)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x1,y1,x2,y2

      call mshplt_line_raw(x1,y1,x2,y1)
      call mshplt_line_raw(x2,y1,x2,y2)
      call mshplt_line_raw(x2,y2,x1,y2)
      call mshplt_line_raw(x1,y2,x1,y1)

      return
      end
