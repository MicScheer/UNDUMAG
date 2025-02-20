*CMZ :          14/02/2025  17.10.06  by  Michael Scheer
*CMZ :  1.04/00 09/02/2025  13.11.42  by  Michael Scheer
*-- Author :    Michael Scheer   18/03/2024
      subroutine util_inter_spline_2d(nx,ny,xa,ya,fa,x,y,f,modus,istatus)

      implicit none

      integer nx,ny,modus,istatus
      real*8 :: xa(nx),ya(ny),fa(nx,ny),x,y,f,a(4,4),dx,dy,xx,yy

      real*8, dimension(:,:,:,:), allocatable :: coef
      integer :: nxo=0,nyo=0,ix,iy

      save coef,nxo,nyo

      if (ny.lt.3.or.nx.lt.3) then
        istatus=1
        return
      endif

      if (modus.lt.0) then
        if (nx.gt.nxo.or.ny.gt.nyo) then
          deallocate(coef,stat=istatus)
          allocate(coef(4,4,nx,ny))
        endif
        call util_coef_spline_2d(nx,ny,fa,coef,istatus)
        if(istatus.ne.0) return
      endif

      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)

      if (x.lt.xa(1)) then
        istatus=-1
        return
      else if(x.gt.xa(nx)+1.0d-9*dx) then
        istatus=-2
        return
      endif

      if (y.lt.ya(1)) then
        istatus=-1
        return
      else if(y.gt.ya(ny)+1.0d-9*dy) then
        istatus=-2
        return
      endif

      if (x.gt.xa(nx)) x=xa(nx)-1.0d-9*dx
      if (y.gt.ya(ny)) y=ya(ny)-1.0d-9*dy

      ix=int((x-xa(1))/dx)+1
      xx=(x-xa(ix))/dx

      iy=int((y-ya(1))/dy)+1
      yy=(y-ya(iy))/dy

      a(1:4,1:4)=coef(1:4,1:4,ix,iy)

      f=
     &  a(1,1)+a(1,2)*yy+a(1,3)*yy**2+a(1,4)*yy**3+
     &  a(2,1)*xx+a(2,2)*xx**1*yy+a(2,3)*xx**1*yy**2+a(2,4)*xx**1*yy**3+
     &  a(3,1)*xx**2+a(3,2)*xx**2*yy+a(3,3)*xx**2*yy**2+a(3,4)*xx**2*yy**3+
     &  a(4,1)*xx**3+a(4,2)*xx**3*yy+a(4,3)*xx**3*yy**2+a(4,4)*xx**3*yy**3

      nxo=nx
      nyo=ny

      return
      end
