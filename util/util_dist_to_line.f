*CMZ :  2.05/02 26/10/2023  07.40.12  by  Michael Scheer
*CMZ :  3.05/26 07/12/2018  12.21.13  by  Michael Scheer
*CMZ :  3.05/20 01/11/2018  15.08.03  by  Michael Scheer
*CMZ : 00.00/19 07/06/2016  12.17.28  by  Michael Scheer
*-- Author :    Michael Scheer   07/06/2016
      subroutine util_dist_to_line(p1,p2,p,hit,dist,istat)

      implicit none

      double precision p1(3),p2(3),p(3),p21(3),pp1(3),hit(3),dist,pn

c Calculates point hit on line (p1,p2) nearest to p

      integer istat

      istat=0

      p21=p2-p1
      pn=norm2(p21)

      if (pn.eq.0.0d0) then
        istat=-1
        return
      endif

      p21=p21/pn
      pp1=p-p1

      dist=(pp1(1)*p21(1)+pp1(2)*p21(2)+pp1(3)*p21(3))

      hit=p1+dist*p21

      p21=hit-p
      dist=sqrt(p21(1)**2+p21(2)**2+p21(3)**2)

      return
      end
