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
        if (log10z_ps.eq.0) then
          zn(i)=-0.5+(z(i)-zmin3d_ps)/(zmax3d_ps-zmin3d_ps)
        else
          zn(i)=-0.5+(alog10(z(i))-zmin3d_ps)/(zmax3d_ps-zmin3d_ps)
        endif
        if (log10x_ps.eq.0) then
          xn(i)=-0.5+(x(i)-xmin3d_ps)/(xmax3d_ps-xmin3d_ps)
        else
          xn(i)=-0.5+(alog10(x(i))-xmin3d_ps)/(xmax3d_ps-xmin3d_ps)
        endif
        if (log10y_ps.eq.0) then
          yn(i)=-0.5+(y(i)-ymin3d_ps)/(ymax3d_ps-ymin3d_ps)
        else
          yn(i)=-0.5+(alog10(y(i))-ymin3d_ps)/(ymax3d_ps-ymin3d_ps)
        endif
      enddo

      call mshplt_3dto2d(n,xn,yn,zn,xp,yp)
      call mshplt_filled_area(n,xp,yp,ioutlined)

      return
      end
