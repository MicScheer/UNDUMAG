*CMZ :  1.04/00 11/02/2025  16.13.19  by  Michael Scheer
*CMZ :  1.01/02 26/09/2014  14.00.09  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  14.46.38  by  Michael Scheer
*CMZ :  1.00/01 23/09/2014  14.36.32  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  11.12.41  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  16.03.13  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_pline_3d(n,x,y,z)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(n),y(n),z(n),xn(n),yn(n),zn(n),xx(n),yy(n)

      integer n,i
      integer kc,kr,kg,kb,ic,ir,ig,ib

      call mshplt_get_line_color(ic,ir,ig,ib)
      call mshplt_get_color(kc,kr,kg,kb)
      if (ic.ne.kc.or.ir.ne.kr.or.kg.ne.ig.or.ib.ne.kb) then
        call mshplt_set_line_color(ic,ir,ig,ib)
      endif

      mode3d_ps=1

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
      call mshplt_pline(n,xx,yy)

      mode3d_ps=0

      return
      end
