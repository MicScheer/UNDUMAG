*CMZ :  1.01/02 26/09/2014  13.59.31  by  Michael Scheer
*CMZ :  0.01/03 22/09/2014  21.04.09  by  Michael Scheer
*CMZ :  0.01/02 11/09/2014  12.03.40  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.32.19  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_pline(n,x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(*),y(*),x1,y1,x2,y2
      integer n,i,ic,ir,ig,ib,kc,kr,kg,kb
      character(2048) cline

      if (n.le.1) return

      call mshplt_get_line_color(ic,ir,ig,ib)
      call mshplt_get_color(kc,kr,kg,kb)
      if (ic.ne.kc.or.ir.ne.kr.or.kg.ne.ig.or.ib.ne.kb) then
        call mshplt_set_line_color(ic,ir,ig,ib)
      endif

      if (log10x_ps.eq.0.or.mode3d_ps.ne.0) then
        x1=xleft_ps+scalex_ps*(x(1)-wxmin_ps)
      else
        x1=xleft_ps+scalex_ps*(alog10(x(1))-wxmin_ps)
      endif

      if (log10y_ps.eq.0.or.mode3d_ps.ne.0) then
        y1=ybottom_ps+scaley_ps*(y(1)-wymin_ps)
      else
        y1=ybottom_ps+scaley_ps*(alog10(y(1))-wymin_ps)
      endif

      write(cline,*)x1,y1,' moveto'
      call mshplt_fill_buff(cline)

      do i=2,n
        if (log10x_ps.eq.0.or.mode3d_ps.ne.0) then
          x2=xleft_ps+scalex_ps*(x(i)-wxmin_ps)
        else
          x2=xleft_ps+scalex_ps*(alog10(x(i))-wxmin_ps)
        endif
        if (log10y_ps.eq.0.or.mode3d_ps.ne.0) then
          y2=ybottom_ps+scaley_ps*(y(i)-wymin_ps)
        else
          y2=ybottom_ps+scaley_ps*(alog10(y(i))-wymin_ps)
        endif
        write(cline,*)x2,y2,' lineto'
        call mshplt_fill_buff(cline)
      enddo

      itouched_ps=1

      write(cline,*)'stroke'
      call mshplt_fill_buff(cline)

      return
      end
