*CMZ :  0.01/03 22/09/2014  20.55.58  by  Michael Scheer
*CMZ :  0.01/02 11/09/2014  12.03.40  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.32.19  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_pline_raw(n,x,y)

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

      x1=xleft_ps+scalex_ps*(x(1)-wxmin_ps)
      y1=ybottom_ps+scaley_ps*(y(1)-wymin_ps)

      write(cline,*)x1,y1,' moveto'
      call mshplt_fill_buff(cline)

      do i=2,n
        x2=xleft_ps+scalex_ps*(x(i)-wxmin_ps)
        y2=ybottom_ps+scaley_ps*(y(i)-wymin_ps)
        write(cline,*)x2,y2,' lineto'
        call mshplt_fill_buff(cline)
      enddo

      itouched_ps=1

      write(cline,*)'stroke'
      call mshplt_fill_buff(cline)

      return
      end
