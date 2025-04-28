*CMZ :  1.04/00 07/02/2025  11.49.37  by  Michael Scheer
*CMZ :  0.01/03 22/09/2014  20.55.58  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  13.25.01  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_line_raw(x1,y1,x2,y2)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(2),y(2),x1,y1,x2,y2
      integer kc,kr,kg,kb,ic,ir,ig,ib

      call mshplt_get_line_color(ic,ir,ig,ib)
      call mshplt_get_color(kc,kr,kg,kb)
      if (ic.ne.kc.or.ir.ne.kr.or.kg.ne.ig.or.ib.ne.kb) then
        call mshplt_set_line_color(ic,ir,ig,ib)
      endif

      x(1)=x1
      y(1)=y1
      x(2)=x2
      y(2)=y2

      call mshplt_pline_raw(2,x,y)

      return
      end
