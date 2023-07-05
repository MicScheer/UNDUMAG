*CMZ :  1.03/01 07/11/2014  19.48.39  by  Michael Scheer
*CMZ :  0.00/06 20/08/2014  09.15.17  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.25.49  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mzitoc(ival,cval)

      implicit none

      integer ival,k,i
      character*(*) cval
      character(12) c12

      write(c12,*)ival
      k=0
      cval=''
      do i=1,min(12,len(cval))
        if (c12(i:i).ne.' ') then
          k=k+1
          cval(k:k)=c12(i:i)
        endif
      enddo

      return
      end
