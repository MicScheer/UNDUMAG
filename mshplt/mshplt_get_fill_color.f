*CMZ :  1.02/00 29/09/2014  10.09.09  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.01.54  by  Michael Scheer
*CMZ :  0.00/04 08/08/2014  16.34.41  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_get_fill_color(icolor,ired,igreen,iblue)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer icolor,ired,igreen,iblue

      icolor=kFillColor_ps
      ired=kFillRed_ps
      igreen=kFillGreen_ps
      iblue=kFillBlue_ps

      return
      end
