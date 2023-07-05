*CMZ :  1.02/00 03/10/2014  10.00.50  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 08/08/2014  15.00.08  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  15.21.58  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_marker_size(siz)

      implicit none

      real siz

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      rmsiz_ps=siz

      return
      end
