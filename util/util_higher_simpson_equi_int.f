*CMZ : 00.00/16 18/12/2014  11.31.39  by  Michael Scheer
*CMZ : 00.00/07 13/08/2009  15.38.27  by  Michael Scheer
*-- Author :    Michael Scheer   13/08/2009
      subroutine util_higher_simpson_equidist_integral(n,x,f,sum,istat)

      implicit none

      double precision x(*),f(*),sum,sumeve,sumodd,dx
      integer n,i,istat

      istat=-1

      if (n.lt.2) return

      sum=0.0d0
      sumeve=0.0d0
      sumodd=0.0d0

      dx=(x(n)-x(1))/(n-1)
      if (dx.eq.0.0d0) then
        istat=-2
        return
      endif

      do i=1,n-1
        if (abs((x(i+1)-x(i))/dx)-1.0d0.ge.1.0d-10) then
          istat=-3
          return
        endif
      enddo

      if (n.ge.8) then
        do i=5,n-4
          sumeve=sumeve+f(i)
        enddo
        sum=(
     &    (17.0d0*f(1)+59.0d0*f(2)+43.0d0*f(3)+49.0d0*f(4))/48.0d0
     &   +sumeve
     &    +(17.0d0*f(n)+59.0d0*f(n-1)+43.0d0*f(n-2)+49.0d0*f(n-3))/48.0d0
     &    )*dx
      else if (n.eq.7) then
        sum=(f(1)+4.0d0*(f(2)+f(4)+f(6))+2.0d0*(f(3)+f(5))+f(7))*dx/3.0d0
      else if (n.eq.6) then
        sum=(f(3)+f(4)+(5.0d0*f(1)+13.0d0*f(2)
     &    +13.0d0*f(5)+5.0d0*f(6))/12.0d0)*dx
      else if (n.eq.5) then
        sum=(f(1)+4.0d0*(f(2)+f(4))+2.0d0*f(3)+f(5))*dx/3.0d0
      else if (n.eq.4) then
        sum=(5.0d0*f(1)+13.0d0*f(2)
     &    +13.0d0*f(3)+5.0d0*f(4))*dx/12.0d0
      else if (n.eq.3) then
        sum=(f(1)+4.0d0*f(2)+f(3))*dx/3.0d0
      else if (n.eq.2) then
        sum=(f(1)+f(2))*dx/2.0d0
      endif

      istat=0

      return
      end
