*CMZ :  2.02/00 16/09/2020  07.31.43  by  Michael Scheer
*CMZ : 00.00/02 26/01/2004  16.28.03  by  Michael Scheer
*-- Author :    Michael Scheer   26/01/2004
      subroutine util_vnorm_of_plane(p1,p2,p3,vnorm,istat)

c +PATCH,//UNDUMAG/UTIL
c +DECK,util_vnorm_of_plane.

      implicit none

      double precision p1(3),p2(3),p3(3),vnorm(3),p12(3),p23(3),vn
      integer istat

      p12=p2-p1
      p23=p3-p2

      vnorm(1)=p12(2)*p23(3)-p12(3)*p23(2)
      vnorm(2)=p12(3)*p23(1)-p12(1)*p23(3)
      vnorm(3)=p12(1)*p23(2)-p12(2)*p23(1)

      vn=vnorm(1)**2+vnorm(2)**2+vnorm(3)**2

      if (vn.ne.0.0d0) then
        istat=0
        vnorm=vnorm/sqrt(vn)
      else
        istat=-1
      endif

      return
      end
