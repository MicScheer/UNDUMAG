*CMZ :  2.04/16 07/09/2023  15.50.32  by  Michael Scheer
*CMZ :  2.04/04 06/03/2023  10.33.27  by  Michael Scheer
*CMZ :  2.04/03 03/03/2023  11.15.21  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  18.37.56  by  Michael Scheer
*-- Author :    Michael Scheer   25/02/2023
      subroutine util_area_triangle(p1,p2,p3,a)
      implicit none

      double precision :: p1(3),p2(3),p3(3),a,p21(3),p31(3),h,pn

      a=0.0d0

      p21=p2-p1
      p31=p3-p1
      pn=norm2(p31)

      h=sqrt((dot_product(p21,p31)/pn)**2+p21(1)**2+p21(2)**2+p21(3)**2)
      a=h/2.0d0*pn

      return
      end
