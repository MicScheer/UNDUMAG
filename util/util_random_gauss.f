*CMZ :  0.00/09 18/01/2016  13.02.27  by  Michael Scheer
*CMZ :  3.02/03 30/10/2014  17.17.28  by  Michael Scheer
*-- Author :    Michael Scheer   05/09/2014
      subroutine util_random_gauss(n,g)

      implicit none

c Based on textbook "Numerical Reciepes"
c The subroutine util_random is called, which can be initialized
c and controlled by util_random_init, util_random_set_seed, and
c util_random_get_seed.

      real g(n),r,v1,v2,fac,rr(2)
      integer n,nold,i
      data nold/-1/
      save

      if (n.eq.1.and.nold.eq.1) then
        nold=-1
        g(1)=v2*fac
        return
      endif

      if (n.eq.1) then
1       call util_random(2,rr)
        v1=2.*rr(1)-1.
        v2=2.*rr(2)-1.
        r=v1*v1+v2*v2
        if (r.ge.1..or.r.eq.0.0) goto 1
        fac=sqrt(-2.*log(r)/r)
        g(1)=v1*fac
        nold=1
      else
        do i=1,(n/2*2),2
11      call util_random(2,rr)
        v1=2.*rr(1)-1.
        v2=2.*rr(2)-1.
        r=v1*v1+v2*v2
        if (r.ge.1..or.r.eq.0.0) goto 11
        fac=sqrt(-2.*log(r)/r)
        g(i)=v1*fac
        g(i+1)=v2*fac
        enddo
        nold=-1
      endif

      if (mod(n,2).ne.0) then
12      call util_random(2,rr)
        v1=2.*rr(1)-1.
        v2=2.*rr(2)-1.
        r=v1*v1+v2*v2
        if (r.ge.1..or.r.eq.0.0) goto 12
        fac=sqrt(-2.*log(r)/r)
        g(n)=v1*fac
      endif

      return
      end
