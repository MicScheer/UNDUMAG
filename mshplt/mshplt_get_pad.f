*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  08.45.57  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_get_pad(xleft,xright,ybottom,ytop)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xleft,xright,ybottom,ytop

      xleft=xleft_ps
      xright=xright_ps
      ybottom=ybottom_ps
      ytop=ytop_ps

      return
      end
