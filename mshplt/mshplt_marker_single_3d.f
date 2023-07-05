*CMZ :  1.01/01 25/09/2014  09.02.51  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  15.22.29  by  Michael Scheer
*CMZ :  1.00/01 23/09/2014  17.15.48  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_marker_single_3d(x,y,z)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(1),y(1),z(1)

      call mshplt_marker_3d(1,x,y,z)

      return
      end
