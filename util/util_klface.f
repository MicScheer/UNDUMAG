*CMZ :  2.05/02 31/10/2023  13.36.47  by  Michael Scheer
*-- Author :    Michael Scheer   31/10/2023
      subroutine util_klface(kface,l,n,lface)

      implicit none

      integer i,l,k,n,kface(*),lface(*)

      k=1
      do i=1,n
        lface(i)=k
        k=k+kface(k)+1
      enddo

      end
