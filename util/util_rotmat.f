*CMZ :  2.02/01 27/12/2021  17.39.16  by  Michael Scheer
*-- Author :    Michael Scheer   06/04/2018
      subroutine util_rotmat(vrot,phi,rm,istat)

! +PATCH,//UNDUMAG/UTIL
! +DECK,util_rotmat.

      implicit none

      double precision phi,vrot(3),vlen,o(3),s,c,c1,rm(3,3)

      integer istat

      istat=0

      vlen=norm2(vrot)

      if (vlen.eq.0.0d0) then
        rm=0.0d0
        rm(1,1)=1.0d0
        rm(2,2)=1.0d0
        rm(3,3)=1.0d0
        istat=1
        return
      endif

      o=vrot/vlen
      s=sin(phi)
      c=cos(phi)
      c1=1.0d0-c

      rm(1,1)=o(1)*o(1)*c1+c
      rm(2,2)=o(2)*o(2)*c1+c
      rm(3,3)=o(3)*o(3)*c1+c

      rm(1,2)=o(1)*o(2)*c1-o(3)*s
      rm(1,3)=o(1)*o(3)*c1+o(2)*s

      rm(2,1)=o(1)*o(2)*c1+o(3)*s
      rm(2,3)=o(2)*o(3)*c1-o(1)*s

      rm(3,1)=o(1)*o(3)*c1-o(2)*s
      rm(3,2)=o(2)*o(3)*c1+o(1)*s

      return
      end
