*CMZ :  1.04/00 12/02/2025  14.57.24  by  Michael Scheer
*CMZ :  1.02/00 30/09/2014  13.52.50  by  Michael Scheer
*CMZ :  1.01/01 25/09/2014  09.16.46  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  14.46.38  by  Michael Scheer
*CMZ :  1.00/01 23/09/2014  14.36.32  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  11.12.41  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  16.03.13  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_marker_3d(n,x,y,z)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(n),y(n),z(n),xn(n),yn(n),zn(n),xx(n),yy(n)
      integer n,i,ic,ir,ig,ib

      do i=1,n
        if (log10x_ps.eq.0) then
          xn(i)=xcornmin_ps+(x(i)-xmin3d_ps)/(xmax3d_ps-xmin3d_ps)*dxcorn_ps
        else
          xn(i)=xcornmin_ps+(alog10(x(i))-xmin3d_ps)/(xmax3d_ps-xmin3d_ps)*dxcorn_ps
        endif
        if (log10y_ps.eq.0) then
          yn(i)=ycornmin_ps+(y(i)-ymin3d_ps)/(ymax3d_ps-ymin3d_ps)*dycorn_ps
        else
          yn(i)=ycornmin_ps+(alog10(y(i))-ymin3d_ps)/(ymax3d_ps-ymin3d_ps)*dycorn_ps
        endif
        if (log10z_ps.eq.0) then
          zn(i)=zcornmin_ps+(z(i)-zmin3d_ps)/(zmax3d_ps-zmin3d_ps)*dzcorn_ps
        else
          zn(i)=zcornmin_ps+(alog10(z(i))-zmin3d_ps)/(zmax3d_ps-zmin3d_ps)*dzcorn_ps
        endif
      enddo

      call mshplt_3dto2d(n,xn,yn,zn,xx,yy)

      call mshplt_get_marker_color(ic,ir,ig,ib)
      call mshplt_set_marker_color(ic,ir,ig,ib)

      call mshplt_marker_raw(n,xx,yy)

      return
      end
