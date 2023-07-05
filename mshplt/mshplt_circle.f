*CMZ :  0.01/03 23/09/2014  09.22.45  by  Michael Scheer
*CMZ :  0.01/02 15/09/2014  15.09.38  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  18.23.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_circle(x,y,r)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x,y,r,x1,y1

        if (log10x_ps.eq.0) then
          x1=xleft_ps+scalex_ps*(x-wxmin_ps)
        else
          x1=xleft_ps+scalex_ps*(alog10(x)-wxmin_ps)
        endif
        if (log10y_ps.eq.0) then
          y1=ybottom_ps+scaley_ps*(y-wymin_ps)
        else
          y1=ybottom_ps+scaley_ps*(alog10(y)-wymin_ps)
        endif

      write(cline_ps,*)'newpath ',x1,y1,r*2.*chhe_ps,' 0 360 arc stroke'
      call mshplt_fill_buff(cline_ps)

      return
      end
