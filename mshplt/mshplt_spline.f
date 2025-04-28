*CMZ :  1.04/00 11/02/2025  20.33.10  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  09.36.37  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  15.30.29  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_spline(n,x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(n),y(n),x1,y1,x2,y2
      integer n,i
      integer kc,kr,kg,kb,ic,ir,ig,ib
      character(128) cline

      double precision sd(n),xd(n),yd(n),x2d(n),y2d(n)
      double precision xsd,ysd,dsd,ssd

      integer :: nsegments=1000

      if (n.le.1) return

      call mshplt_get_line_color(ic,ir,ig,ib)
      call mshplt_get_color(kc,kr,kg,kb)
      if (ic.ne.kc.or.ir.ne.kr.or.kg.ne.ig.or.ib.ne.kb) then
        call mshplt_set_line_color(ic,ir,ig,ib)
      endif

      if (n.eq.2) then

        if (log10x_ps.eq.0) then
          x1=xleft_ps+scalex_ps*(x(1)-wxmin_ps)
        else
          x1=xleft_ps+scalex_ps*(alog10(x(1))-wxmin_ps)
        endif

        if (log10y_ps.eq.0) then
          y1=ybottom_ps+scaley_ps*(y(1)-wymin_ps)
        else
          y1=ybottom_ps+scaley_ps*(alog10(y(1))-wymin_ps)
        endif

        if (log10x_ps.eq.0) then
          x2=xleft_ps+scalex_ps*(x(2)-wxmin_ps)
        else
          x2=xleft_ps+scalex_ps*(alog10(x(2))-wxmin_ps)
        endif

        if (log10y_ps.eq.0) then
          y2=ybottom_ps+scaley_ps*(y(2)-wymin_ps)
        else
          y2=ybottom_ps+scaley_ps*(alog10(y(2))-wymin_ps)
        endif

        write(cline,*)x1,y1,x2,y2,' lineto'
        call mshplt_fill_buff(cline)
        return
      endif

      do i=1,n
        sd(i)=i
        if (log10x_ps.eq.0) then
          xd(i)=xleft_ps+scalex_ps*(x(i)-wxmin_ps)
        else
          xd(i)=xleft_ps+scalex_ps*(alog10(x(i))-wxmin_ps)
        endif
        if (log10y_ps.eq.0) then
          yd(i)=ybottom_ps+scaley_ps*(y(i)-wymin_ps)
        else
          yd(i)=ybottom_ps+scaley_ps*(alog10(y(i))-wymin_ps)
        endif
      enddo

      call util_spline_interpolation_f90(n,sd,xd,sd(1),xd(1),x2d,-1) !init spline
      call util_spline_interpolation_f90(n,sd,yd,sd(1),yd(1),y2d,-1) !init spline

      dsd=dble(n-1)/dble(nsegments-1)
      ssd=sd(1)
      x1=sngl(xd(1))
      y1=sngl(yd(1))
      write(cline,*)x1,y1,' moveto'
      call mshplt_fill_buff(cline)
      do i=1,nsegments
        call util_spline_interpolation_f90(n,sd,xd,ssd,xsd,x2d,0)
        x2=sngl(xsd)
        call util_spline_interpolation_f90(n,sd,yd,ssd,ysd,y2d,0)
        y2=sngl(ysd)
        write(cline,*)x1,y1,x2,y2,' lineto'
        call mshplt_fill_buff(cline)
        x1=x2
        y1=y2
        ssd=min(ssd+dsd,sd(n))
      enddo

      write(cline,*)'stroke'
      call mshplt_fill_buff(cline)

      itouched_ps=1

      return
      end
