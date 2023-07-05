*CMZ : 00.00/02 21/08/2006  11.07.41  by  Michael Scheer
*-- Author :    Michael Scheer   21/08/2006
      subroutine util_simpson_integral(n,x,f,sum)

      implicit none

      double precision x(n),f(n),sum
      integer n,i

      sum=0.0d0

      do i=1,n-1
        sum=sum+(f(i+1)+f(i))*(x(i+1)-x(i))/2.d0
      enddo

      return
      end
