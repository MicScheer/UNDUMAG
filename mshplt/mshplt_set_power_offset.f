*CMZ :  1.02/00 30/09/2014  12.39.33  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 21/08/2014  16.58.38  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.18.06  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_power_offset(x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x,y

      xoffexp_ps=x
      yoffexp_ps=y

      return
      end
