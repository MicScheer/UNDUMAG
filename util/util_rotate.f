*CMZ :  2.02/01 14/10/2021  05.12.31  by  Michael Scheer
*-- Author :    Michael Scheer   06/04/2018
      subroutine util_rotate(cen,vrot,phi,vin,vout,istat)

c +PATCH,//UTIL/FOR
c +DECK,util_rotate.
      implicit none

      double precision cen(3),phi,vin(3),vout(3),vrot(3),vlen,o(3),
     &  r(3),s,c,c1,rm(3,3)

      integer istat

      istat=0

      vlen=norm2(vrot)
      if (vlen.eq.0.0d0) then
        vout=vin
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

      r=vin-cen

      vout(1)=rm(1,1)*r(1)+rm(1,2)*r(2)+rm(1,3)*r(3)+cen(1)
      vout(2)=rm(2,1)*r(1)+rm(2,2)*r(2)+rm(2,3)*r(3)+cen(2)
      vout(3)=rm(3,1)*r(1)+rm(3,2)*r(2)+rm(3,3)*r(3)+cen(3)

      return
      end
