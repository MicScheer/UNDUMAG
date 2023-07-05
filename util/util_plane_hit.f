*CMZ :  2.02/00 07/01/2019  13.02.07  by  Michael Scheer
*CMZ :  3.05/26 07/12/2018  12.21.13  by  Michael Scheer
*CMZ :  3.05/20 01/11/2018  15.08.03  by  Michael Scheer
*CMZ : 00.00/19 07/06/2016  12.17.28  by  Michael Scheer
*-- Author :    Michael Scheer   07/06/2016
      subroutine util_plane_hit(p1,p2,p3,p,v,hit,dist,distn,vnorm,istat)

      implicit none

      double precision p1(3),p2(3),p3(3),p(3),v(3),p21(3),p31(3),pp1(3),
     &  a(3,3),x(3),hit(3),dist,distn,vnorm(3)

c Calculates point hit in plane (p1,p2,p3), where r=p+dist*v hits the plane,
c i.e. dist is distance from p to then plane in the direction of v, while
c distn is the distance to the plane

      integer istat

      istat=0

      p21=p2-p1
      p31=p3-p1
      pp1=p-p1

      a(1:3,1)=v(1:3)
      a(1:3,2)=p21(1:3)
      a(1:3,3)=p31(1:3)

      call util_solve_3x3(a,pp1,istat)
      if (istat.ne.0) return

      hit=p-pp1(1)*v
      x=hit-p
      dist=sqrt(x(1)**2+x(2)**2+x(3)**2)

      if (x(1)*v(1)+x(2)*v(2)+x(3)*v(3).lt.0.0d0) then
        dist=-dist
      endif

      call util_vnorm_of_plane(p1,p2,p3,vnorm,istat)
      if (istat.ne.0) return

      distn=vnorm(1)*x(1)+vnorm(2)*x(2)+vnorm(3)*x(3)

      return
      end
