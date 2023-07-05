*CMZ :  1.00/01 23/09/2014  14.28.02  by  Michael Scheer
*CMZ :  0.01/02 04/09/2014  14.47.39  by  Michael Scheer
*-- Author :    Michael Scheer   04/09/2014
      subroutine mplbox(xleft,xright,ybottom,ytop,chopt)
      real xleft,xright,ybottom,ytop
      character chopt,choptd

      choptd=chopt
      call mshplt_box(xleft,xright,ybottom,ytop)

      return
      end
