*CMZ :  0.00/13 27/07/2016  16.25.56  by  Michael Scheer
*CMZ : 00.00/02 21/08/2006  11.07.41  by  Michael Scheer
*-- Author :    Michael Scheer   21/08/2006
      subroutine util_simpson_running_integral(n,x,f,sum)

      implicit none

      double precision x(n),f(n),sum(n)
      integer n,i

      sum(1)=0.0d0

      do i=1,n-1
        sum(i+1)=sum(i)+(f(i+1)+f(i))*(x(i+1)-x(i))/2.d0
      enddo

      return
      end
