*CMZ :  0.01/00 22/08/2014  16.39.59  by  Michael Scheer
*CMZ :  0.00/06 20/08/2014  09.12.05  by  Michael Scheer
*CMZ :  1.17/00 06/05/2014  11.03.18  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.41.44  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mtitle(tit)

      implicit none

      character*(*) tit
      character tdum

      tdum=tit(1:1)



      call mshplt_title(tit)
      return
      end
