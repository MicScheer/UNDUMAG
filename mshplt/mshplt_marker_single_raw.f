*CMZ :  1.02/00 02/10/2014  13.30.19  by  Michael Scheer
*CMZ :  1.00/01 23/09/2014  17.16.19  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_marker_single_raw(x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(1),y(1)

      call mshplt_marker_raw(1,x(1),y(1))

      return
      end
