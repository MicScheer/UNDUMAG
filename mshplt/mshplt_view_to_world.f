*CMZ :  0.01/02 11/09/2014  11.44.51  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.32.19  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_view_to_world(xv,yv,xw,yw)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xv,yv,xw,yw,x,y

      x=xv
      y=yv

      xw=xleft_ps+scalex_ps*(x-wxmin_ps)
      yw=ybottom_ps+scaley_ps*(y-wymin_ps)

      return
      end
