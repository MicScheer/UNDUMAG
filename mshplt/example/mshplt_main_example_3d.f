*CMZ :          12/02/2025  13.37.31  by  Michael Scheer
*CMZ :  1.03/03 06/02/2025  14.56.40  by  Michael Scheer
*-- Author :    Michael Scheer   31/01/2025
      program mshplt_main_example_3d

      implicit none

      integer, parameter :: nxp=11,nyp=11

      real :: x(nxp),y(nyp),z(nxp*nyp),xmin=-2.0,xmax=2.0,ymin=-1.,ymax=1.,dx,dy,
     &  zmin,zmax,zmean,chhe

      integer ix,iy,i

      include 'mshplt.cmn'

      if (nxp.eq.1) then
        x(1)=(xmax+xmin)/2.
        dx=xmax-xmin
      else
        dx=(xmax-xmin)/(nxp-1)
        x(1)=xmin
        do ix=2,nxp
          x(ix)=x(ix-1)+dx
        enddo
      endif

      if (nyp.eq.1) then
        y(1)=(ymax+ymin)/2.
        dy=ymax-ymin
      else
        dy=(ymax-ymin)/(nyp-1)
        y(1)=ymin
        do iy=2,nyp
          y(iy)=y(iy-1)+dy
        enddo
      endif

      i=0
      do ix=1,nxp
        do iy=1,nyp
          i=nxp*(iy-1)+ix
          z(i)=5.0-(x(ix)**2+y(iy)**2)
        enddo
      enddo

      zmin=minval(z)
      zmax=maxval(z)

      if (zmin.eq.zmax) then
        zmean=(zmin+zmax)/2.
        zmin=zmean-0.5
        zmax=zmean+0.5
      endif

c      call mshplt_init(0,-20.,-20.,0,0,800,800,'mshplt.eps',' ',' ',0.0)
      call mplint(0)
      call mshplt_get_character_height(chhe)
      call mshplt_set_character_height(chhe*1.5)

      call mshplt_set_theta_phi(30.,30.)

      call mshplt_zone(3,2,1,'')

      call mshplt_frame3d(xmin-dx/2.,xmax+dx/2.,ymin-dy/2,ymax+dy/2,zmin,zmax,'x','y','z','')
      call mshplt_lego(nxp,xmin,xmax,nyp,ymin,ymax,z,10)
      call mshplt_text_ndc(0.1,1.05,'mshplt_lego (10 levels)')

      call mshplt_zone(3,2,2,'S')

      call mshplt_frame3d(xmin,xmax,ymin,ymax,zmin,zmax,'x','y','z','')
c      call mshplt_frame3d(xmin-dx/2.,xmax+dx/2.,ymin-dy/2,ymax+dy/2,zmin,zmax,'x','y','z','')
      call mshplt_surf(nxp,xmin,xmax,nyp,ymin,ymax,z,'',0)
      call mshplt_text_ndc(0.1,1.05,'mshplt_surf (surf)')

      call mshplt_zone(3,2,4,'S')

c      call mshplt_frame3d(xmin-dx/2.,xmax+dx/2.,ymin-dy/2.,ymax+dy/2.,zmin,zmax,'x','y','z','')
      call mshplt_frame3d(xmin,xmax,ymin,ymax,zmin,zmax,'x','y','z','')
      call mshplt_surf(nxp,xmin,xmax,nyp,ymin,ymax,z,'tile',0)
      call mshplt_text_ndc(0.1,1.05,'mshplt_surf (tile)')
      call mshplt_top(nxp,xmin,xmax,nyp,ymin,ymax,z,'tile',0)
      call mshplt_text_ndc(0.1,-0.2,'mshplt_top (tile)')

      call mshplt_zone(3,2,5,'S')

c      call mshplt_frame3d(xmin-dx/2.,xmax+dx/2.,ymin-dy/2,ymax+dy/2,zmin,zmax,'x','y','z','')
      call mshplt_frame3d(xmin,xmax,ymin,ymax,zmin,zmax,'x','y','z','')
      call mshplt_surf(nxp,xmin,xmax,nyp,ymin,ymax,z,'mark',0)
      call mshplt_text_ndc(0.1,1.05,'mshplt_surf (mark)')

      call mshplt_zone(3,1,3,'S')
      call mshplt_set_theta_phi(90.,0.)

c      call mshplt_frame3d(xmin-dx/2.,xmax+dx/2.,ymin-dy/2,ymax+dy/2,zmin,zmax,'x','y','z','')
      call mshplt_frame3d(xmin,xmax,ymin,ymax,zmin,zmax,'x','y','z','')
      call mshplt_set_nxspline_nyspline(128,128)
      call mshplt_surf(nxp,xmin,xmax,nyp,ymin,ymax,z,'tilespline',0)
      call mshplt_text_ndc(0.,1.02,'mshplt_surf (tilespline)')
      call mshplt_text_ndc(0.,-0.15,'theta=90, phi=0')

      call mplend

      end
