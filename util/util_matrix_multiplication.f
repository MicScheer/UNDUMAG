*CMZ : 00.00/02 21/07/2004  15.43.47  by  Michael Scheer
*-- Author :    Michael Scheer   21/07/2004
      subroutine util_matrix_multiplication(l,m,n,a,b,c,d)

      implicit none

      integer l,m,n,i,ll,nn

      double precision a(l,m),b(m,n),c(l,n),d(l,n)

      do nn=1,n
        do ll=1,l
          d(ll,nn)=0.0d0
          do i=1,n
            d(ll,nn)=d(ll,nn)+a(ll,i)*b(i,nn)
          enddo
        enddo
      enddo

      c=d

      return
      end
