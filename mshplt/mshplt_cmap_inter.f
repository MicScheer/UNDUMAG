*CMZ :  1.04/00 12/02/2025  11.49.50  by  Michael Scheer
*-- Author :    Michael Scheer   09/02/2025
      subroutine mshplt_cmap_inter(z,icolor,red,green,blue)

      use cmapmod
      implicit none

      real :: z,red,green,blue,dz=1.0/255,rgb1(3),rgb2(3),zi,drgb(3),dzi
      integer icolor,icol

      if (z.lt.0.0.or.z.gt.1.0) then
        print*,"*** Error in mshplt_cmap_inter: Argument must be in [0.,1.] ***"
        return
      endif

      zi=z/dz
      icol=min(255,int(zi)+1)

      rgb1(1:3)=cmap(1:3,icol)
      rgb2(1:3)=cmap(1:3,icol+1)

      drgb=(rgb2-rgb1)/dz
      dzi=z-icol*dz

      red=rgb1(1)+drgb(1)*dzi
      green=rgb1(2)+drgb(2)*dzi
      blue=rgb1(3)+drgb(3)*dzi

      icolor=min(256,icol)

      return
      end
