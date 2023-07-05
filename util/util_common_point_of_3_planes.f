*CMZ :  2.02/00 14/09/2020  07.50.56  by  Michael Scheer
*-- Author :    Michael Scheer   12/09/2020
      subroutine util_common_point_of_3_planes(p1,p2,p3,q1,q2,q3,r1,r2,r3,
     &  x,y,z,ifail)

      implicit none

      double precision p1(3),p2(3),p3(3),q1(3),q2(3),q3(3),r1(3),r2(3),r3(3),
     &  x,y,z,p(3),v(3),hit(3),dist,distn,vnorm(3)

      integer ifail,isilent

      if (ifail.ne.0) then
        isilent=1
      else
        isilent=0
      endif

      call util_common_line_of_2_planes(p1,p2,p3,q1,q2,q3,p,v,ifail)

      if (ifail.ne.0) then
        if (isilent.eq.0) then
          print*,"*** Error in util_common_point_of_3_planes: Bad return from util_common_line_of_2_planes ***"
        endif
        return
      endif

      call util_plane_hit(r1,r2,r3,p,v,hit,dist,distn,vnorm,ifail)

      if (ifail.ne.0) then
        if (isilent.eq.0) then
          print*,"*** Error in util_common_point_of_3_planes: Bad return from util_common_line_of_2_planes ***"
        endif
        return
      endif

      x=hit(1)
      y=hit(2)
      z=hit(3)

      return
      end
