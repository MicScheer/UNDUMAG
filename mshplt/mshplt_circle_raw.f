*CMZ :  0.01/03 23/09/2014  09.13.55  by  Michael Scheer
*CMZ :  0.01/02 15/09/2014  15.09.38  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  18.23.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_circle_raw(x,y,r)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x,y,r,x1,y1

      x1=xleft_ps+scalex_ps*(x-wxmin_ps)
      y1=ybottom_ps+scaley_ps*(y-wymin_ps)

      write(cline_ps,*)'newpath ',x1,y1,r*2.*chhe_ps,' 0 360 arc stroke'
      call mshplt_fill_buff(cline_ps)

      return
      end
