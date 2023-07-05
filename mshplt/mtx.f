*CMZ :  0.00/06 19/08/2014  19.48.10  by  Michael Scheer
*CMZ :  1.17/00 22/04/2014  15.41.26  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.16.07  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mtx(x,y,chars)

      implicit none

      real x,y
      character*(*) chars


      call mshplt_text(x,y,chars)

      return
      end
