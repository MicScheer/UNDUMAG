*CMZ :  1.11/00 03/01/2017  10.23.19  by  Michael Scheer
*-- Author :    Michael Scheer   03/01/2017
      subroutine undumag_double_to_radia(c32)

      implicit none

      integer i
      character(32) c32
      character(64) c64

      c64=''
      c64(1:32)=c32(1:32)
      do i=1,32
        if (
     &    c32(i:i).eq.'E'.or.
     &    c32(i:i).eq.'e'.or.
     &    c32(i:i).eq.'D'.or.
     &      c32(i:i).eq.'d') then
          c64(i:i+3)='*10^'
          c64(i+4:35)=c32(i+1:32)
          c32(1:32)=c64(1:32)
          return
        endif
      enddo

      end
