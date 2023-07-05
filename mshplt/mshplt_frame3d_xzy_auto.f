*CMZ :          06/08/2018  15.37.07  by  Michael Scheer
*-- Author :    Michael Scheer   01/08/2018
      subroutine mshplt_frame3d_xzy_auto(n,x,z,y,
     &  xtit,ztit,ytit,chopt,istatus)

      implicit none

      integer istatus,n,i
      real x(n),y(n),z(n),xmin,ymin,zmin,xmax,ymax,zmax,dx,dy,dz
      character(*) xtit,ztit,ytit,chopt

      xmin=1.0e30
      xmax=-1.0e30
      ymin=1.0e30
      ymax=-1.0e30

      do i=1,n
        if (x(i).lt.xmin) xmin=x(i)
        if (x(i).gt.xmax) xmax=x(i)
        if (y(i).lt.ymin) ymin=y(i)
        if (y(i).gt.ymax) ymax=y(i)
        if (z(i).lt.zmin) zmin=z(i)
        if (z(i).gt.zmax) zmax=z(i)
      enddo

      dx=(xmax-xmin)*0.05
      dy=(ymax-ymin)*0.05
      dz=(zmax-zmin)*0.05

      if(dx.eq.0.0) dx=1.
      if(dy.eq.0.0) dy=1.
      if(dz.eq.0.0) dz=1.

      call mshplt_frame3d_xzy(xmin,xmax,zmin,zmax,ymin,ymax,
     &  xtit,ztit,ytit,chopt)

      istatus=0

      end
