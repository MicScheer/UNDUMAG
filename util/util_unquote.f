*CMZ :  2.02/02 21/02/2022  10.29.18  by  Michael Scheer
*CMZ :  1.25/00 08/03/2018  14.13.11  by  Michael Scheer
*CMZ :  1.11/05 13/02/2017  14.53.41  by  Michael Scheer
*CMZ :  1.10/00 01/11/2016  16.50.54  by  Michael Scheer
*CMZ : 00.00/19 21/01/2016  13.55.01  by  Michael Scheer
*CMZ : 00.00/06 07/01/2008  14.32.30  by  Michael Scheer
*CMZ :  1.19/07 22/08/2002  15.44.21  by  Michael Scheer
*-- Author :    Michael Scheer   09/11/2001
      subroutine util_unquote(cword)

      implicit none

      integer i,k

      character(*) cword
      character(2048) cw
      character c1

      k=0
      do i=1,len(cword)
        c1=cword(i:i)
        if (c1.ne.'"'.and.c1.ne."'") then
          k=k+1
          cw(k:k)=c1
        endif
      enddo

      cword=cw(1:k)

      return
      end
