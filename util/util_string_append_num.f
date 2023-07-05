*CMZ :  1.11/04 18/01/2017  13.24.39  by  Michael Scheer
*CMZ : 00.00/19 21/01/2016  13.55.01  by  Michael Scheer
*CMZ : 00.00/06 07/01/2008  14.32.30  by  Michael Scheer
*CMZ :  1.19/07 22/08/2002  15.44.21  by  Michael Scheer
*-- Author :    Michael Scheer   09/11/2001
      subroutine util_string_append_num(cline,num,nfirst,nlast)

      implicit none

      integer num,nfirst,nlast
      character(*) cline
      character(32) c32

      write(c32,*)num
      call util_string_append(cline,c32,nfirst,nlast)

      return
      end
