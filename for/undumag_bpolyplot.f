*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 04/03/2023  17.04.30  by  Michael Scheer
*CMZ :  2.02/02 29/06/2022  10.35.26  by  Michael Scheer
*CMZ :  2.02/01 30/01/2022  08.37.21  by  Michael Scheer
*-- Author :    Michael Scheer   22/10/2021
      subroutine undumag_bpolyplot

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      real xplmin,xplmax,yplmin,yplmax,zplmin,zplmax,theta,pphi

      if (iunduplot.ne.0.and.nmagtot_t+ncwires.ne.0) then
        theta=unduplot_theta !plotting angle
        pphi=unduplot_phi   !plotting angle
        xplmin=xminpl
        xplmax=xmaxpl
        yplmin=yminpl
        yplmax=ymaxpl
        zplmin=zminpl
        zplmax=zmaxpl
        if (xminpl.eq.9999.0d0) xplmin=xmin_t-(xmax_t-xmin_t)*0.1
        if (xmaxpl.eq.9999.0d0) xplmax=xmax_t+(xmax_t-xmin_t)*0.1
        if (yminpl.eq.9999.0d0) yplmin=ymin_t-(ymax_t-ymin_t)*0.1
        if (ymaxpl.eq.9999.0d0) yplmax=ymax_t+(ymax_t-ymin_t)*0.1
        if (zminpl.eq.9999.0d0) zplmin=zmin_t-(zmax_t-zmin_t)*0.1
        if (zmaxpl.eq.9999.0d0) zplmax=zmax_t+(zmax_t-zmin_t)*0.1
        call clcmag_bpolyplot(iunduplot,xplmin,xplmax,yplmin,yplmax,zplmin,zplmax,
     &    theta,pphi,nwitems,ncwires,wire)
      endif !iplot

      zminprof=zplmin
      zmaxprof=zplmax

      return
      end
