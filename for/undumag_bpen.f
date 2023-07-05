*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  0.00/13 10/08/2016  14.00.54  by  Michael Scheer
*CMZ :  0.00/11 20/07/2016  14.40.07  by  Michael Scheer
*CMZ :  0.00/00 20/04/2016  12.40.40  by  Michael Scheer
*CMZ :  1.17/15 19/04/2016  09.20.55  by  Michael Scheer
*CMZ :  1.17/13 07/04/2016  13.28.54  by  Michael Scheer
*CMZ :  0.00/06 08/01/2004  10.22.18  by  Michael Scheer
*CMZ :  0.00/05 23/12/2003  11.36.18  by  Michael Scheer
*CMZ :  0.00/04 19/12/2003  18.17.16  by  Michael Scheer
*CMZ :  0.00/02 12/12/2003  10.40.20  by  Michael Scheer
*CMZ :  0.00/01 08/12/2003  10.56.22  by  Michael Scheer
*-- Author :    Michael Scheer   04/12/2003
      subroutine undumag_bpen(imag,iplan,p1,p2,p3,vn,ifail)

      use commandlinef90m

      implicit none

      double precision p1(3),p2(3),p3(3),vn(3),d2(3),d3(3),vnn
      integer imag,iplan,ifail

c vmag is a point inside magnet
c for the time being (23.12.2003), normal vector is calculated from direction
c of rotation

      ifail=-1

      d2(1)=p2(1)-p1(1)
      d2(2)=p2(2)-p1(2)
      d2(3)=p2(3)-p1(3)

      d3(1)=p3(1)-p1(1)
      d3(2)=p3(2)-p1(2)
      d3(3)=p3(3)-p1(3)

c vn= d2 x d3

      vn(1)=d2(2)*d3(3)-d2(3)*d3(2)
      vn(2)=d2(3)*d3(1)-d2(1)*d3(3)
      vn(3)=d2(1)*d3(2)-d2(2)*d3(1)

      vnn=sqrt(vn(1)*vn(1)+vn(2)*vn(2)+vn(3)*vn(3))

      if (vnn.ne.0.d0) then
        vn(1)=vn(1)/vnn
        vn(2)=vn(2)/vnn
        vn(3)=vn(3)/vnn
      else
        write(lun6,*)'***Error in undumag_bpen: Zero normal vector'
        write(lun6,*)'magnet, plane:', imag,iplan
        return
      endif

      ifail=0

      return
      end
