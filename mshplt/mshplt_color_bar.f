*CMZ :  1.04/00 12/02/2025  13.43.13  by  Michael Scheer
*CMZ :  1.03/03 06/02/2025  10.18.32  by  Michael Scheer
*CMZ :  1.02/00 29/09/2014  11.01.39  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_color_bar(n,zmin,zmax)

      use cmapmod
      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.
c+seq,cmap.

      real :: xp(4),yp(4),zmin,zmax,dy,ticheight,chhe,xpos,ymin,ymax,xaxbar
      integer n,k,i,kspline

      kspline=ksplinecmap
      ksplinecmap=0

      ! These values refer to the view system
      xpos=xcolorbar
      xaxbar=xcolorbar+offaxiscolorbar
      ymin=ymincolorbar
      ymax=ymaxcolorbar

      xp(1)=xpos-0.05
      xp(2)=xpos+0.05
      xp(3)=xp(2)
      xp(4)=xp(1)

      dy=(ymax-ymin)/n

      do i=1,n
        k=mod(i,256+1)
        call mshplt_set_fill_color(k,0,0,0)
c        print*,i,k,cmap(1:3,k)
        yp(1)=ymin+(i-0.5)*dy
        yp(2)=yp(1)
        yp(3)=yp(1)+dy
        yp(4)=yp(3)
        call mshplt_filled_area(4,xp,yp,0)
      enddo

      if (log10y_ps.eq.0) then
c      subroutine mshplt_view_axis(vxmin,vxmax,vymin,vymax,smin,smax,title,
c     &  iticside,anglab,titang,offtit,offlab)
        call mshplt_view_axis(xaxbar,xaxbar,ymin,ymax,zmin,zmax,trim(chmapvar),1,0.0,0.0,
     &    xaxbar+offtitcolorbar,offlabcolorbar)
      else
        ticheight=ticsiz_ps
        chhe=chhe_ps
c      subroutine mshplt_view_log_axis(
c     &  xorig, yorig, xylength, smin, smax, ang,
c     &  chhe, ticheight, title,iticside,ilabside,reltitang,offtit,
c     &  rellabang,offlab)
        call  mshplt_view_log_axis(
     &    xaxbar,ymincolorbar,ymax-ymincolorbar, zmin, zmax,90.0,
     &    chhe, ticheight, trim(chmapvar),1,-1,0.0,xaxbar+offtitcolorbar,
     &    -90.0,0.0)
      endif

      ksplinecmap=kspline

      return
      end
