*CMZ :  1.04/00 11/02/2025  21.14.27  by  Michael Scheer
*CMZ :  1.03/00 06/10/2014  18.03.21  by  Michael Scheer
*CMZ :  1.02/00 29/09/2014  10.18.31  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_filled_area_spline(n,x,y,ioutlined)

      use cmapmod
      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer, parameter :: nspline=1000
      real x(n),y(n),dx,dy,xspline(nspline),yspline(nspline)

      integer ioutlined,i,n,icolo,iro,igo,ibo,ifilcol,ifr,ifg,ifb

      if (n.lt.3) return

      call util_spline_real4(n,x,y,nspline,xspline,yspline)

      call mshplt_get_fill_color(ifilcol,ifr,ifg,ifb)
      call mshplt_get_color(icolo,iro,igo,ibo)

      call mshplt_set_color(klinecolor_ps,klinered_ps,klinegreen_ps,klineblue_ps)

      dx=x(1)
      dy=y(1)
      call mshplt_view_to_world(dx,dy,dx,dy)
      write(cline_ps,*)'newpath ',dx,dy,' moveto'
      call mshplt_fill_buff(cline_ps)

      do i=1,nspline-1
        dx=(xspline(i+1)-xspline(i))*scalex_ps
        dy=(yspline(i+1)-yspline(i))*scaley_ps
        write(cline_ps,*)dx,dy,' rlineto'
        call mshplt_fill_buff(cline_ps)
      enddo

      call mshplt_fill_buff('closepath')

      if (ioutlined.lt.0) then
        call mshplt_fill_buff('stroke')
        goto 9999
      endif

      if (ioutlined.eq.0) then
        call mshplt_set_fill_color(ifilcol,ifr,ifg,ifb)
        call mshplt_fill_buff('fill')
      else
        call mshplt_fill_buff('gsave')
        call mshplt_set_fill_color(ifilcol,ifr,ifg,ifb)
        call mshplt_fill_buff('fill grestore')
        call mshplt_set_color(
     &    kLineColor_ps,kLineRed_ps,kLineGreen_ps,kLineBlue_ps)
        call mshplt_fill_buff('stroke')
      endif

9999  call mshplt_set_color(icolo,iro,igo,ibo)

      return
      end
