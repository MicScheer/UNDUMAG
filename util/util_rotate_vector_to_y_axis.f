*CMZ :  2.02/00 26/02/2021  14.51.21  by  Michael Scheer
*CMZ :  1.25/00 15/03/2018  16.03.32  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine util_rotate_vector_to_y_axis(vin,rotmat,istat)

      implicit none

      double precision cosphi,sinphi,costhe,sinthe,vin(3),
     &  vx,vy,vz,vn,rotmat(3,3),rotphi(3,3),rotthe(3,3),wx,wy,wz

      double precision :: eps=1.0d-9

      integer istat

      istat=0
      vn=sqrt(vin(1)**2+vin(2)**2+vin(3)**2)

      rotmat=0.0d0

      if (vn.eq.0.0d0) then
        istat=-1
        return
      endif

      vx=vin(1)/vn
      vy=vin(2)/vn
      vz=vin(3)/vn

      ! special cases vin is perpendicular to x-axis
      if (abs(vx).lt.eps) then
        rotmat(1,1)=1.0d0
        vn = sqrt(vy**2+vz**2)
        vy = vy/vn
        vz = vz/vn
        rotmat(2,2)=vy
        rotmat(2,3)=vz
        rotmat(3,2)=-vz
        rotmat(3,3)=vy
        istat=0
        return
      endif

      ! special cases vin is perpendicular to z-axis
      if (abs(vz).lt.eps) then
        rotmat(3,3)=1.0d0
        vn = sqrt(vy**2+vx**2)
        vy = vy/vn
        vx = vx/vn
        rotmat(1,1)=vy
        rotmat(1,2)=-vx
        rotmat(2,1)=vx
        rotmat(2,2)=vy
        istat=0
        return
      endif

      if (vx**2+vz**2.gt.eps) then
        cosphi=vx/sqrt(vx**2+vz**2)
        sinphi=vz/sqrt(vx**2+vz**2)
      else
        cosphi=1.0d0
        sinphi=0.0d0
      endif

      wx=cosphi*vx+sinphi*vz
      wy=vy
      wz=-sinphi*vx+cosphi*vz

      if (wx**2+wy**2.gt.1.0d-9) then
        costhe=wy/sqrt(wx**2+wy**2)
        sinthe=wx/sqrt(wx**2+wy**2)
      else
        costhe=1.0d0
        sinthe=0.0d0
      endif

      rotphi(1,1)=cosphi
      rotphi(1,2)=0.0d0
      rotphi(1,3)=sinphi

      rotphi(2,1)=0.0d0
      rotphi(2,2)=1.0d0
      rotphi(2,3)=0.0d0

      rotphi(3,1)=-sinphi
      rotphi(3,2)=0.0d0
      rotphi(3,3)=cosphi

      rotthe(1,1)=costhe
      rotthe(1,2)=-sinthe
      rotthe(1,3)=0.0d0

      rotthe(2,1)=sinthe
      rotthe(2,2)=costhe
      rotthe(2,3)=0.0d0

      rotthe(3,1)=0.0d0
      rotthe(3,2)=0.0d0
      rotthe(3,3)=1.0d0

      rotmat(1,1)=
     &  rotthe(1,1)*rotphi(1,1)+rotthe(1,2)*rotphi(2,1)+rotthe(1,3)*rotphi(3,1)
      rotmat(1,2)=
     &  rotthe(1,1)*rotphi(1,2)+rotthe(1,2)*rotphi(2,2)+rotthe(1,3)*rotphi(3,2)
      rotmat(1,3)=
     &  rotthe(1,1)*rotphi(1,3)+rotthe(1,2)*rotphi(2,3)+rotthe(1,3)*rotphi(3,3)

      rotmat(2,1)=
     &  rotthe(2,1)*rotphi(1,1)+rotthe(2,2)*rotphi(2,1)+rotthe(2,3)*rotphi(3,1)
      rotmat(2,2)=
     &  rotthe(2,1)*rotphi(1,2)+rotthe(2,2)*rotphi(2,2)+rotthe(2,3)*rotphi(3,2)
      rotmat(2,3)=
     &  rotthe(2,1)*rotphi(1,3)+rotthe(2,2)*rotphi(2,3)+rotthe(2,3)*rotphi(3,3)

      rotmat(3,1)=
     &  rotthe(3,1)*rotphi(1,1)+rotthe(3,2)*rotphi(2,1)+rotthe(3,3)*rotphi(3,1)
      rotmat(3,2)=
     &  rotthe(3,1)*rotphi(1,2)+rotthe(3,2)*rotphi(2,2)+rotthe(3,3)*rotphi(3,2)
      rotmat(3,3)=
     &  rotthe(3,1)*rotphi(1,3)+rotthe(3,2)*rotphi(2,3)+rotthe(3,3)*rotphi(3,3)

      return
      end
