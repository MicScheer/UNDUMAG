*CMZ :  0.01/00 23/08/2014  11.19.02  by  Michael Scheer
*CMZ :  0.00/06 20/08/2014  09.19.28  by  Michael Scheer
*CMZ :  1.17/00 06/05/2014  16.11.26  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.09.15  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mplax(xtit,ytit)

      implicit none

      character*(*) xtit,ytit
      character xtitd,ytitd

      xtitd=xtit(1:1)
      ytitd=ytit(1:1)


      call mshplt_draw_axis_titles(xtit,ytit)

      return
      end
