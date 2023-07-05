*CMZ :  1.02/00 02/10/2014  13.33.43  by  Michael Scheer
*CMZ :  1.01/02 25/09/2014  09.19.29  by  Michael Scheer
*CMZ :  1.01/01 25/09/2014  09.15.18  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  14.46.38  by  Michael Scheer
*CMZ :  1.00/01 23/09/2014  14.36.32  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  11.12.41  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  16.03.13  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_text_3d(x,y,z,text)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x,y,z,xn(1),yn(1),zn(1),xx(1),yy(1)
      character(*) text

      xn=-0.5+(x-xmin3d_ps)/(xmax3d_ps-xmin3d_ps)
      yn=-0.5+(y-ymin3d_ps)/(ymax3d_ps-ymin3d_ps)

      if (log10z_ps.eq.0) then
        zn=-0.5+(z-zmin3d_ps)/(zmax3d_ps-zmin3d_ps)
      else
        zn=-0.5+(alog10(z)-zmin3d_ps)/(zmax3d_ps-zmin3d_ps)
      endif

      call mshplt_3dto2d(1,xn,yn,zn,xx,yy)
      call mshplt_text_raw(xx(1),yy(1),text)

      return
      end
