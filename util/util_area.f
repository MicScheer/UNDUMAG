*CMZ :  2.04/04 06/03/2023  10.33.27  by  Michael Scheer
*CMZ :  2.04/03 03/03/2023  11.15.21  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  18.37.56  by  Michael Scheer
*-- Author :    Michael Scheer   25/02/2023
      subroutine util_area(n,x,y,tiny,a,kfail)
      implicit none

      double precision :: x(*),y(*),a,tiny
      integer n,i,kfail,nh,k,i1

      integer, dimension (:), allocatable :: ihull

      a=0.0d0

      allocate(ihull(n+1))

      call util_convex_hull_2d(n,x,y,nh,ihull,tiny,kfail)
      if (kfail.ne.0) then
        deallocate(ihull)
        return
      endif

      do k=1,nh-1
        i=ihull(k)
        i1=ihull(k+1)
        a=a+(y(i)+y(i1))/2.0d0*(x(i1)-x(i))
      enddo

      a=abs(a)

      deallocate(ihull)

      return
      end
