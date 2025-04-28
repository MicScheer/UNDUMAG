*CMZ :          28/04/2025  10.12.33  by  Michael Scheer
*CMZ :  1.03/03 02/02/2025  10.28.41  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  09.38.57  by  Michael Scheer
*CMZ :  0.01/00 24/08/2014  13.17.26  by  Michael Scheer
*CMZ :  0.00/06 20/08/2014  12.49.55  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  16.17.24  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_text(x,y,text)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x1,y1,x,y
      integer ic,ir,ig,ib
      character(*) text
      character(2048) cline

      itouched_ps=1

c      if (icolor_ps.ne.kTextColor_ps) then
c        call mshplt_get_text_color(ic,ir,ig,ib)
      call mshplt_set_text_color(kTextColor_ps,ir,ig,ib)
c      endif

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

      write(cline,*)
     &  x1,y1,' moveto ',
     &  tang_ps,' rotate ',
     &  '(',text(1:len_trim(text)),') show',
     &  -tang_ps,' rotate'
      call mshplt_fill_buff(cline)

      return
      end
