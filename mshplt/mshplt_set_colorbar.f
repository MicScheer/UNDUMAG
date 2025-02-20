*CMZ :  1.03/03 06/02/2025  10.20.45  by  Michael Scheer
*-- Author :    Michael Scheer   04/02/2025
      subroutine mshplt_set_colorbar(x,ymin,ymax,chvar,offax,offtit,offlab)

      use cmapmod

      implicit none

      real x,ymin,ymax,offtit,offlab,offax
      character(*) chvar

      xcolorbar=x
      ymincolorbar=ymin
      ymaxcolorbar=ymax
      chmapvar=trim(chvar)

      offaxiscolorbar=offax
      offtitcolorbar=offtit
      offlabcolorbar=offlab

      end
