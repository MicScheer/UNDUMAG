*CMZ :          14/02/2025  17.09.28  by  Michael Scheer
*CMZ :  1.04/00 09/02/2025  13.44.25  by  Michael Scheer
*CMZ :  1.03/03 04/02/2025  10.25.27  by  Michael Scheer
*CMZ :  1.03/00 07/10/2014  10.55.15  by  Michael Scheer
*CMZ :  1.02/01 05/10/2014  14.25.34  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  12.39.05  by  Michael Scheer
*-- Author :    Michael Scheer   30/09/2014
      subroutine mshplt_z_spline(nx,xmin,xmax,ny,ymin,ymax,z,nxs,nys,zs)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xmin,xmax,ymin,ymax,z(nx*ny),zs(nxs*nys)
      real*8 xmind,xmaxd,ymind,ymaxd,xd(nx),yd(ny),zxy(nx,ny),zd,dx,dy,x,y

      integer nx,ny,nxs,nys,ix,iy,iz,istatus,modus

      xmind=xmin
      ymind=ymin
      xmaxd=xmax
      ymaxd=ymax

      iz=0
      do iy=1,ny
        do ix=1,nx
          iz=iz+1
          zxy(ix,iy)=z(iz)
        enddo
      enddo

      dx=(xmaxd-xmind)/(nx-1)
      xd(1)=xmind
      do ix=2,nx
        xd(ix)=xd(ix-1)+dx
      enddo

      dy=(ymaxd-ymind)/(ny-1)
      yd(1)=ymind
      do iy=2,ny
        yd(iy)=yd(iy-1)+dy
      enddo

      dx=(xmaxd-xmind)/(nxs-1)
      dy=(ymaxd-ymind)/(nys-1)

      y=ymind
      iz=0
      do iy=1,nys
        x=xmind
        do ix=1,nxs
          modus=1
          if (ix+iy.eq.2) modus=-1
          call util_inter_spline_2d(nx,ny,xd,yd,zxy,x,y,zd,modus,istatus)
          iz=iz+1
          zs(iz)=sngl(zd)
          x=x+dx
        enddo
        y=y+dy
      enddo

      end
