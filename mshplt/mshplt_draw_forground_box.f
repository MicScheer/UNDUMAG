*CMZ :  1.02/01 05/10/2014  15.10.20  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  12.39.05  by  Michael Scheer
*-- Author :    Michael Scheer   30/09/2014
      subroutine mshplt_draw_forground_box

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer ix,iy,kco,kro,kgo,kbo

      call mshplt_get_line_color(kco,kro,kgo,kbo)
      call mshplt_set_line_color(
     &  kframecolor_ps,kframered_ps,kframegreen_ps,kframeblue_ps)

      if (ifbox_ps.eq.0) return

      if (theta_ps.le.90.) then

        if (phi_ps.le.90.) then

          call mshplt_line_raw(
     &      xpcorn_ps(1),ypcorn_ps(1),xpcorn_ps(5),ypcorn_ps(5))
          call mshplt_line_raw(
     &      xpcorn_ps(5),ypcorn_ps(5),xpcorn_ps(8),ypcorn_ps(8))
          call mshplt_line_raw(
     &      xpcorn_ps(5),ypcorn_ps(5),xpcorn_ps(6),ypcorn_ps(6))

        else if (phi_ps.le.180.) then

          call mshplt_line_raw(
     &      xpcorn_ps(7),ypcorn_ps(7),xpcorn_ps(8),ypcorn_ps(8))
          call mshplt_line_raw(
     &      xpcorn_ps(5),ypcorn_ps(5),xpcorn_ps(8),ypcorn_ps(8))
          call mshplt_line_raw(
     &      xpcorn_ps(4),ypcorn_ps(4),xpcorn_ps(8),ypcorn_ps(8))

        else if (phi_ps.le.270.) then

          call mshplt_line_raw(
     &      xpcorn_ps(7),ypcorn_ps(7),xpcorn_ps(8),ypcorn_ps(8))
          call mshplt_line_raw(
     &      xpcorn_ps(3),ypcorn_ps(3),xpcorn_ps(7),ypcorn_ps(7))
          call mshplt_line_raw(
     &      xpcorn_ps(6),ypcorn_ps(6),xpcorn_ps(7),ypcorn_ps(7))

        else if (phi_ps.le.360.) then

          call mshplt_line_raw(
     &      xpcorn_ps(5),ypcorn_ps(5),xpcorn_ps(6),ypcorn_ps(6))
          call mshplt_line_raw(
     &      xpcorn_ps(6),ypcorn_ps(6),xpcorn_ps(7),ypcorn_ps(7))
          call mshplt_line_raw(
     &      xpcorn_ps(2),ypcorn_ps(2),xpcorn_ps(6),ypcorn_ps(6))

        endif !phi.le.90.

      else if (theta_ps.le.180.) then

        if (phi_ps.le.90.) then

          call mshplt_pline_raw(3,xpcorn_ps(2),ypcorn_ps(2))
          call mshplt_line_raw(
     &      xpcorn_ps(3),ypcorn_ps(3),xpcorn_ps(7),ypcorn_ps(7))

        else if (phi_ps.le.180.) then

          ix=1
          iy=3
          call mshplt_pline_raw(3,xpcorn_ps(ix),ypcorn_ps(ix))
          call mshplt_line_raw(
     &      xpcorn_ps(ix+1),ypcorn_ps(ix+1),xpcorn_ps(ix+5),ypcorn_ps(ix+5))

        else if (phi_ps.le.270.) then

          ix=0
          iy=ix+2
          call mshplt_pline_raw_closed(4,xpcorn_ps(1),ypcorn_ps(1))
          call mshplt_line_raw(
     &      xpcorn_ps(ix+1),ypcorn_ps(ix+1),xpcorn_ps(ix+5),ypcorn_ps(ix+5))

        else if (phi_ps.le.360.) then

          ix=3
          iy=3
          call mshplt_pline_raw_closed(4,xpcorn_ps(1),ypcorn_ps(1))
          call mshplt_line_raw(
     &      xpcorn_ps(ix+1),ypcorn_ps(ix+1),xpcorn_ps(ix+5),ypcorn_ps(ix+5))

        endif !phi.le.90.

      endif !theta

      call mshplt_set_line_color(kco,kro,kgo,kbo)

      return
      end
