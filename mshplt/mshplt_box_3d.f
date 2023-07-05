*CMZ :  1.03/00 06/10/2014  18.03.21  by  Michael Scheer
*CMZ :  1.02/01 05/10/2014  15.49.28  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_box_3d(xc,yc,zc,xl,yl,zl,
     &  kcol,kred,kgreen,kblue,ioutlined)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(8),y(8),z(8),xp(4),yp(4),zp(4),
     &  xmin,ymin,zmin,xmax,ymax,zmax,
     &  xc,yc,zc,xl,yl,zl

      integer kcol(3),kred(3),kgreen(3),kblue(3),ioutlined,kc,kr,kg,kb

      ymin=1.0e30
      ymax=-1.0e30

      xmin=xc-xl/2.
      xmax=xc+xl/2.
      ymin=yc-yl/2.
      ymax=yc+yl/2.
      zmin=zc-zl/2.
      zmax=zc+zl/2.

      x(1)=xmin
      x(2)=xmax
      x(3)=xmax
      x(4)=xmin
      x(5:8)=x(1:4)

      y(1)=ymin
      y(2)=ymin
      y(3)=ymax
      y(4)=ymax
      y(5:8)=y(1:4)

      z(1:4)=zmin
      z(5:8)=zmax

      call mshplt_get_fill_color(kc,kr,kg,kb)

      if(theta_ps.le.90.0)then
        call mshplt_set_fill_color(kcol(1),kred(1),kgreen(1),kblue(1))
        call mshplt_filled_mesh_3d(4,x(5),y(5),z(5),ioutlined)
        if (phi_ps.le.90.) then
          xp(1)=x(8)
          xp(2)=x(4)
          xp(3)=x(1)
          xp(4)=x(5)
          yp(1)=y(8)
          yp(2)=y(4)
          yp(3)=y(1)
          yp(4)=y(5)
          zp(1)=z(8)
          zp(2)=z(4)
          zp(3)=z(1)
          zp(4)=z(5)
          call mshplt_set_fill_color(kcol(2),kred(2),kgreen(2),kblue(2))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
          xp(1)=x(1)
          xp(2)=x(2)
          xp(3)=x(6)
          xp(4)=x(5)
          yp(1)=y(1)
          yp(2)=y(2)
          yp(3)=y(6)
          yp(4)=y(5)
          zp(1)=z(1)
          zp(2)=z(2)
          zp(3)=z(6)
          zp(4)=z(5)
          call mshplt_set_fill_color(kcol(3),kred(3),kgreen(3),kblue(3))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
        else if (phi_ps.le.180.) then
          xp(1)=x(8)
          xp(2)=x(4)
          xp(3)=x(1)
          xp(4)=x(5)
          yp(1)=y(8)
          yp(2)=y(4)
          yp(3)=y(1)
          yp(4)=y(5)
          zp(1)=z(8)
          zp(2)=z(4)
          zp(3)=z(1)
          zp(4)=z(5)
          call mshplt_set_fill_color(kcol(2),kred(2),kgreen(2),kblue(2))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
          xp(1)=x(7)
          xp(2)=x(3)
          xp(3)=x(4)
          xp(4)=x(8)
          yp(1)=y(7)
          yp(2)=y(3)
          yp(3)=y(4)
          yp(4)=y(8)
          zp(1)=z(7)
          zp(2)=z(3)
          zp(3)=z(4)
          zp(4)=z(8)
          call mshplt_set_fill_color(kcol(3),kred(3),kgreen(3),kblue(3))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
        else if (phi_ps.le.270.) then
          xp(1)=x(6)
          xp(2)=x(2)
          xp(3)=x(3)
          xp(4)=x(7)
          yp(1)=y(6)
          yp(2)=y(2)
          yp(3)=y(3)
          yp(4)=y(7)
          zp(1)=z(6)
          zp(2)=z(2)
          zp(3)=z(3)
          zp(4)=z(7)
          call mshplt_set_fill_color(kcol(2),kred(2),kgreen(2),kblue(2))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
          xp(1)=x(7)
          xp(2)=x(3)
          xp(3)=x(4)
          xp(4)=x(8)
          yp(1)=y(7)
          yp(2)=y(3)
          yp(3)=y(4)
          yp(4)=y(8)
          zp(1)=z(7)
          zp(2)=z(3)
          zp(3)=z(4)
          zp(4)=z(8)
          call mshplt_set_fill_color(kcol(3),kred(3),kgreen(3),kblue(3))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
        else if (phi_ps.le.360.) then
          xp(1)=x(6)
          xp(2)=x(2)
          xp(3)=x(3)
          xp(4)=x(7)
          yp(1)=y(6)
          yp(2)=y(2)
          yp(3)=y(3)
          yp(4)=y(7)
          zp(1)=z(6)
          zp(2)=z(2)
          zp(3)=z(3)
          zp(4)=z(7)
          call mshplt_set_fill_color(kcol(2),kred(2),kgreen(2),kblue(2))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
          xp(1)=x(5)
          xp(2)=x(1)
          xp(3)=x(2)
          xp(4)=x(6)
          yp(1)=y(5)
          yp(2)=y(1)
          yp(3)=y(2)
          yp(4)=y(6)
          zp(1)=z(5)
          zp(2)=z(1)
          zp(3)=z(2)
          zp(4)=z(6)
          call mshplt_set_fill_color(kcol(3),kred(3),kgreen(3),kblue(3))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
        endif
      else !theta
        call mshplt_set_fill_color(kcol(1),kred(1),kgreen(1),kblue(1))
        call mshplt_filled_mesh_3d(4,x,y,z,ioutlined)
        if (phi_ps.le.90.) then
          xp(1)=x(7)
          xp(2)=x(3)
          xp(3)=x(4)
          xp(4)=x(8)
          yp(1)=y(7)
          yp(2)=y(3)
          yp(3)=y(4)
          yp(4)=y(8)
          zp(1)=z(7)
          zp(2)=z(3)
          zp(3)=z(4)
          zp(4)=z(8)
          call mshplt_set_fill_color(kcol(2),kred(2),kgreen(2),kblue(2))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
          xp(1)=x(6)
          xp(2)=x(2)
          xp(3)=x(3)
          xp(4)=x(7)
          yp(1)=y(6)
          yp(2)=y(2)
          yp(3)=y(3)
          yp(4)=y(7)
          zp(1)=z(6)
          zp(2)=z(2)
          zp(3)=z(3)
          zp(4)=z(7)
          call mshplt_set_fill_color(kcol(3),kred(3),kgreen(3),kblue(3))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
        else if (phi_ps.le.180.) then
          xp(1)=x(1)
          xp(2)=x(5)
          xp(3)=x(6)
          xp(4)=x(2)
          yp(1)=y(1)
          yp(2)=y(5)
          yp(3)=y(6)
          yp(4)=y(2)
          zp(1)=z(1)
          zp(2)=z(5)
          zp(3)=z(6)
          zp(4)=z(2)
          call mshplt_set_fill_color(kcol(2),kred(2),kgreen(2),kblue(2))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
          xp(1)=x(6)
          xp(2)=x(2)
          xp(3)=x(3)
          xp(4)=x(7)
          yp(1)=y(6)
          yp(2)=y(2)
          yp(3)=y(3)
          yp(4)=y(7)
          zp(1)=z(6)
          zp(2)=z(2)
          zp(3)=z(3)
          zp(4)=z(7)
          call mshplt_set_fill_color(kcol(3),kred(3),kgreen(3),kblue(3))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
        else if (phi_ps.le.270.) then
          xp(1)=x(1)
          xp(2)=x(4)
          xp(3)=x(8)
          xp(4)=x(5)
          yp(1)=y(1)
          yp(2)=y(4)
          yp(3)=y(8)
          yp(4)=y(5)
          zp(1)=z(1)
          zp(2)=z(4)
          zp(3)=z(8)
          zp(4)=z(5)
          call mshplt_set_fill_color(kcol(2),kred(2),kgreen(2),kblue(2))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
          xp(1)=x(1)
          xp(2)=x(2)
          xp(3)=x(6)
          xp(4)=x(5)
          yp(1)=y(1)
          yp(2)=y(2)
          yp(3)=y(6)
          yp(4)=y(5)
          zp(1)=z(1)
          zp(2)=z(2)
          zp(3)=z(6)
          zp(4)=z(5)
          call mshplt_set_fill_color(kcol(3),kred(3),kgreen(3),kblue(3))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
        else if (phi_ps.le.360.) then
          xp(1)=x(1)
          xp(2)=x(5)
          xp(3)=x(8)
          xp(4)=x(4)
          yp(1)=y(1)
          yp(2)=y(5)
          yp(3)=y(8)
          yp(4)=y(4)
          zp(1)=z(1)
          zp(2)=z(5)
          zp(3)=z(8)
          zp(4)=z(4)
          call mshplt_set_fill_color(kcol(2),kred(2),kgreen(2),kblue(2))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
          xp(1)=x(4)
          xp(2)=x(8)
          xp(3)=x(7)
          xp(4)=x(3)
          yp(1)=y(4)
          yp(2)=y(8)
          yp(3)=y(7)
          yp(4)=y(3)
          zp(1)=z(4)
          zp(2)=z(8)
          zp(3)=z(7)
          zp(4)=z(3)
          call mshplt_set_fill_color(kcol(3),kred(3),kgreen(3),kblue(3))
          call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)
        endif !phi
      endif !theta

      call mshplt_set_fill_color(kc,kr,kg,kb)

      return
      end
