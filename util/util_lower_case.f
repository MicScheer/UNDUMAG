*CMZ :  2.02/00 08/03/2018  14.14.52  by  Michael Scheer
*CMZ : 00.00/16 13/10/2014  09.07.28  by  Michael Scheer
*-- Author :    Michael Scheer   13/10/2014
      subroutine util_lower_case(cline)

      implicit none

      character(*) cline

      integer i,ic1
      character c1
      equivalence (ic1,c1)

      ic1=0

      do i=1,len_trim(cline)
        c1=cline(i:i)
        if (ic1.ge.65.and.ic1.le.90) ic1=ic1+32
        cline(i:i)=c1
      enddo

      return
      end
