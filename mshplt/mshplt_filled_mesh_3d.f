*CMZ :  1.04/00 11/02/2025  16.31.05  by  Michael Scheer
*CMZ :  1.02/00 29/09/2014  11.01.39  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_filled_mesh_3d(n,x,y,z,ioutlined)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(*),y(*),z(*),xp(4),yp(4),xn(4),yn(4),zn(4)
      integer ioutlined,n,i

      if (n.gt.4.or.n.lt.3) return

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

      call mshplt_3dto2d(n,xn,yn,zn,xp,yp)
      call mshplt_filled_area(n,xp,yp,ioutlined)

      return
      end
