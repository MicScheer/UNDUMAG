*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  20.51.17  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  15.21.58  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_get_marker_size(siz)

      implicit none

      real siz

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      siz=rmsiz_ps

      return
      end
