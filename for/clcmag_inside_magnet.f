*CMZ :  2.04/13 04/09/2023  10.39.10  by  Michael Scheer
*-- Author :    Michael Scheer   30/08/2023
      subroutine clcmag_inside_magnet(p,tmag,inside,tiny)

      use magnets_structure

      implicit none

      Type(T_Magnet) tmag

      double precision p(3),v(3),dot,tiny,gcen(3)
      integer inside,ifac,isurf

      gcen=tmag%gcen

      ! return value is 0, if point p is outside
      ! return value is 1, if point p is on surface
      ! return value is 2, if point p is inside

      inside=2
      isurf=0

      do ifac=1,tmag%nface
        v=p-tmag%fcen(1:3,ifac)-gcen(1:3)
        v=v/norm2(v)
        dot=dot_product(v,tmag%fnorm(1:3,ifac))
        if (dot.gt.tiny) then
          inside=0
          exit
        else if (abs(dot).le.tiny) then
          isurf=1
          exit
        endif
      enddo

      if (inside.eq.2.and.isurf.ne.0) inside=1

      return
      end
