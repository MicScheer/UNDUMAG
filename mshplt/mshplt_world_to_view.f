*CMZ :  0.01/02 11/09/2014  11.53.53  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.32.19  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_world_to_view(xw,yw,xv,yv)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xv,yv,xw,yw,x,y

      x=xw
      y=yw

      xv=wxmin_ps+(x-xleft_ps)/scalex_ps
      yv=wymin_ps+(y-ybottom_ps)/scaley_ps

      return
      end
