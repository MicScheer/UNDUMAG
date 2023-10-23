*CMZ :  2.05/01 06/10/2023  12.12.36  by  Michael Scheer
*-- Author :    Michael Scheer   15/08/2018
      subroutine util_solve_matrix_3x3(a,b,x,ifail)

c +PATCH,//UTIL/UTIL
c +DECK,util_solve_matrix_3x3.

      ! Calculate matrix x, such that b=x*a

      implicit none

      double precision x(3,3),a(3,3),b(3,3),det,v(3)
      integer :: ifail,i

      ifail=0
      call util_determinante_3(a,det)
      if (abs(det).lt.1.0d-30) then
        ifail=-1
        return
      endif

      do i=1,3
        v=b(1:3,i)
        call util_solve_3x3(a,v,ifail)
        if (ifail.ne.0) return
        x(1:3,i)=v
      enddo

      return
      end
