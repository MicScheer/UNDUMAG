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

      real x(*),y(*),z(*)
      real, dimension(:), allocatable :: xn,yn,zn,xx,yy
      integer n,i

      allocate(xn(n))
      allocate(yn(n))
      allocate(zn(n))
      allocate(xx(n))
      allocate(yy(n))

      do i=1,n
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
        if (log10z_ps.eq.0) then
          zn(i)=-0.5+(z(i)-zmin3d_ps)/(zmax3d_ps-zmin3d_ps)
        else
          zn(i)=-0.5+(alog10(z(i))-zmin3d_ps)/(zmax3d_ps-zmin3d_ps)
        endif
      enddo

      call mshplt_3dto2d(n,xn,yn,zn,xx,yy)
      call mshplt_marker_raw(n,xx,yy)

      deallocate(xn)
      deallocate(yn)
      deallocate(zn)
      deallocate(xx)
      deallocate(yy)

      return
      end
