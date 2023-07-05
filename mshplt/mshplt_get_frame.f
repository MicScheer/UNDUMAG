*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  16.06.35  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_get_frame(xmin,xmax,ymin,ymax)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xmin,xmax,ymin,ymax

      xmin=wxmin_ps
      xmax=wxmax_ps
      ymin=wymin_ps
      ymax=wymax_ps

      return
      end
