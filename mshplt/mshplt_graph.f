*CMZ :          06/08/2018  08.40.38  by  Michael Scheer
*-- Author :    Michael Scheer   06/08/2018
      subroutine mshplt_graph(n,x,y,xtit,ytit,chopt)

      implicit none

      integer n
      real x(n),y(n)

      character(*) xtit,ytit,chopt

      call mshplt_frame_auto(n,x,y,xtit,ytit,chopt)
      call mshplt_pline(n,x,y)

      return
      end
