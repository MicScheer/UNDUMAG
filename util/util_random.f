*CMZ :  0.00/09 29/02/2016  16.24.34  by  Michael Scheer
*CMZ :  3.02/03 05/09/2014  12.29.51  by  Michael Scheer
*-- Author :    Michael Scheer   05/09/2014
      subroutine util_random(n,r)
*KEEP,random.
      include 'random.cmn'
*KEND.

      real r(n)

      call random_number(r)

      irancalls=irancalls+n

      return
      end
