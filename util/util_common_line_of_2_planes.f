*CMZ :  2.02/00 14/09/2020  07.48.02  by  Michael Scheer
*-- Author :    Michael Scheer   12/09/2020
      subroutine util_common_line_of_2_planes(p1,p2,p3,q1,q2,q3,pl,vln,ifail)

      implicit none

      double precision p1(3),p2(3),p3(3),q1(3),q2(3),q3(3),pl(3),
     &  vln(3),vn(3),wn(3),dist,distn

      integer ifail

      ifail=-1

      call util_vnorm_of_plane(p1,p2,p3,vn,ifail)
      if (ifail.ne.0) return

      call util_vnorm_of_plane(q1,q2,q3,wn,ifail)
      if (ifail.ne.0) return

      call util_vcross(vn,wn,vln)
      call util_vcross(vn,vln,wn)

      call util_plane_hit(q1,q2,q3,p1,wn,pl,dist,distn,vn,ifail)

      return
      end
