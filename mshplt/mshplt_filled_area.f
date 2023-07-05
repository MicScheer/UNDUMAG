*CMZ :  1.03/00 06/10/2014  18.03.21  by  Michael Scheer
*CMZ :  1.02/00 29/09/2014  10.18.31  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_filled_area(n,x,y,ioutlined)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(*),y(*),dx,dy
      integer ioutlined,i,n,icolo,iro,igo,ibo

      if (n.lt.3) return

      call mshplt_get_color(icolo,iro,igo,ibo)

      call mshplt_set_color(
     &  klinecolor_ps,klinered_ps,klinegreen_ps,klineblue_ps)

      dx=x(1)
      dy=y(1)
      call mshplt_view_to_world(dx,dy,dx,dy)
      write(cline_ps,*)'newpath ',dx,dy,' moveto'
      call mshplt_fill_buff(cline_ps)

      do i=1,n-1
        dx=(x(i+1)-x(i))*scalex_ps
        dy=(y(i+1)-y(i))*scaley_ps
        write(cline_ps,*)dx,dy,' rlineto'
        call mshplt_fill_buff(cline_ps)
      enddo

      call mshplt_fill_buff('closepath')

      if (ioutlined.lt.0) then
        call mshplt_fill_buff('stroke')
        goto 9999
      endif

      if (ioutlined.eq.0) then
        call mshplt_set_color(
     &    kFillColor_ps,kFillRed_ps,kFillGreen_ps,kFillBlue_ps)
        call mshplt_fill_buff('fill')
      else
        call mshplt_fill_buff('gsave')
        call mshplt_set_color(
     &    kFillColor_ps,kFillRed_ps,kFillGreen_ps,kFillBlue_ps)
        call mshplt_fill_buff('fill grestore')
        call mshplt_set_color(
     &    kLineColor_ps,kLineRed_ps,kLineGreen_ps,kLineBlue_ps)
        call mshplt_fill_buff('stroke')
      endif

9999  call mshplt_set_color(icolo,iro,igo,ibo)

      return
      end
