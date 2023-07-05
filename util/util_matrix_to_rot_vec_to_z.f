*CMZ :  2.02/00 16/09/2020  07.31.43  by  Michael Scheer
*CMZ :  1.23/04 28/09/2017  19.50.58  by  Michael Scheer
*-- Author :    Michael Scheer   13/09/2017
      subroutine util_matrix_to_rot_vec_to_z(v,rotmat,ifail)


c +PATCH,//UNDUMAG/UTIL
c +DECK,util_matrix_to_rot_vec_to_z.

      ! calculates matrix rotmat such that rotmat * v = (0,0,|v|)

      implicit none

      double precision v(3),rotmat(3,3),cosphi,sinphi,costhe,sinthe,vn,vxy
      integer ifail

      ifail=0

      vn=sqrt(v(1)**2+v(2)**2+v(3)**2)

      if (vn.eq.0.0d0) then
        ifail=1
        return
      endif

      vxy=sqrt(v(1)**2+v(2)**2)
      if (vxy.ne.0.0d0) then
        cosphi=v(1)/vxy
        sinphi=v(2)/vxy
      else
        cosphi=1.0d0
        sinphi=0.0d0
      endif
      costhe=v(3)/vn
      sinthe=sqrt(1.0d0-costhe**2)

      rotmat(1,1)=cosphi*costhe
      rotmat(1,2)=costhe*sinphi
      rotmat(1,3)=-sinthe
      rotmat(2,1)=-sinphi
      rotmat(2,2)=cosphi
      rotmat(2,3)=0.0d0
      rotmat(3,1)=cosphi*sinthe
      rotmat(3,2)=sinphi*sinthe
      rotmat(3,3)=costhe

      return
      end
