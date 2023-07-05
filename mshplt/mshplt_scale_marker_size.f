*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  18.38.59  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  15.21.58  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_scale_marker_size(fac)

      implicit none

      real fac

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      rmsiz_ps=rmsiz_ps*fac

      return
      end
