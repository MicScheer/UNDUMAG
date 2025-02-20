*CMZ :  1.03/03 06/02/2025  10.29.52  by  Michael Scheer
*-- Author :    Michael Scheer   04/02/2025
      subroutine mshplt_get_colorbar(x,ymin,ymax,chvar,offax,offtit,offlab)

      use cmapmod

      implicit none

      real x,ymin,ymax,offtit,offlab,offax
      character(*) chvar

      x=xcolorbar
      ymin=ymincolorbar
      ymax=ymaxcolorbar
      chvar=chmapvar

      offax=offaxiscolorbar
      offtit=offtitcolorbar
      offlab=offlabcolorbar

      end
