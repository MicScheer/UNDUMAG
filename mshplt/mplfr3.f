*CMZ :  1.02/00 02/10/2014  15.04.37  by  Michael Scheer
*CMZ :  0.00/06 20/08/2014  11.28.27  by  Michael Scheer
*CMZ :  1.17/00 06/05/2014  09.27.26  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.27.58  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mplfr3(xmin,xmax,ymin,ymax,zmin,zmax,theta,phi,chopt)

      implicit none

      real xmin,xmax,ymin,ymax,zmin,zmax,theta,phi
      character*(*) chopt
      character(8) copt
      integer ko



      copt=chopt(1:len_trim(chopt))
      call mshplt_set_theta_phi(theta,phi)
      call mshplt_get_box(ko)
      call mshplt_set_box(0)
      call mshplt_frame3d(xmin,xmax,ymin,ymax,zmin,zmax,
     &  '','','',copt)
      call mshplt_set_box(ko)

      return
      end
