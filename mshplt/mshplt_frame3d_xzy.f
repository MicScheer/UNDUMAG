*CMZ :          25/09/2016  11.32.32  by  Michael Scheer
*CMZ :  1.03/01 08/10/2014  14.16.19  by  Michael Scheer
*CMZ :  1.03/00 06/10/2014  16.06.01  by  Michael Scheer
*CMZ :  1.02/01 05/10/2014  10.00.08  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  11.12.46  by  Michael Scheer
*CMZ :  1.01/02 26/09/2014  13.31.57  by  Michael Scheer
*CMZ :  1.01/00 25/09/2014  08.49.12  by  Michael Scheer
*CMZ :  1.00/01 24/09/2014  13.15.20  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  13.56.57  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 21/08/2014  16.57.22  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  11.22.28  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  16.03.13  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_frame3d_xzy(xminin,xmaxin,zminin,zmaxin,yminin,ymaxin,
     &  xtit,ztit,ytit,chopt)

c ONYL FOR SPECIAL PURPOSES. TO GET REAL RESULT,
C YOU MUST PLOT YMAX-(Y-YMIN) INSTEAD OF Y DATA INTO THIS FRAME

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.


      real xmin,xmax,ymin,ymax,zmin,zmax,
     &  xminin,xmaxin,yminin,ymaxin,zminin,zmaxin,
     &  xcorn(8),ycorn(8),zcorn(8),ang,xyplen,
     &  ticheight,chhe,titang,rellabang

      integer ilinestyleo,ilinecoloro,iaxis,ibox,ic,iticside,ilabside
      integer kred,kgreen,kblue,kcolor,ifirst
      integer kredo,kgreeno,kblueo

      character(*) xtit,ytit,ztit,chopt
      character(2048) xtitd,ytitd,ztitd,choptd

      data xcorn/-0.5,0.5,0.5,-0.5,-0.5,0.5,0.5,-0.5/
      data ycorn/-0.5,-0.5,0.5,0.5,-0.5,-0.5,0.5,0.5/
      data zcorn/-0.5,-0.5,-0.5,-0.5,0.5,0.5,0.5,0.5/

      xmax=xmaxin
      xmin=xminin
      ymax=ymaxin
      ymin=yminin
      zmax=zmaxin
      zmin=zminin

      if (log10x_ps.ne.0) then

        if (xminin.le.0.0) then
          xmin=-30.
        else
          xmin=float(int(alog10(xminin)))
          if (xmin.le.0.0.and.
     &      (10.**xmin-xminin)/10.**xmin.gt.1.0e-5) xmin=xmin-1.
        endif

        if (xmaxin.le.xminin) then
          xmax=xmin+1
        else
          xmax=float(int(alog10(xmax)))
          if (xmax.ge.0.0.and.
     &      (xmaxin-10.**xmax)/10.**xmax.gt.1.0e-5)
     &      xmax=xmax+1.
        endif
      endif

      if (log10y_ps.ne.0) then

        if (yminin.le.0.0) then
          ymin=-30.
        else
          ymin=float(int(alog10(yminin)))
          if (ymin.le.0.0.and.
     &      (10.**ymin-yminin)/10.**ymin.gt.1.0e-5) ymin=ymin-1.
        endif

        if (ymaxin.le.yminin) then
          ymax=ymin+1
        else
          ymax=float(int(alog10(ymax)))
          if (ymax.ge.0.0.and.
     &      (ymaxin-10.**ymax)/10.**ymax.gt.1.0e-5)
     &      ymax=ymax+1.
        endif
      endif

      if (log10z_ps.ne.0) then

        if (zminin.le.0.0) then
          zmin=-30.
        else
          zmin=float(int(alog10(zminin)))
          if (zmin.le.0.0.and.
     &      (10.**zmin-zminin)/10.**zmin.gt.1.0e-5) zmin=zmin-1.
        endif

        if (zmaxin.le.zminin) then
          zmax=zmin+1
        else
          zmax=float(int(alog10(zmax)))
          if (zmax.ge.0.0.and.
     &      (zmaxin-10.**zmax)/10.**zmax.gt.1.0e-5)
     &      zmax=zmax+1.
        endif
      endif

      ticheight=ticsiz_ps
      chhe=chhe_ps

      write(lun_ps,'(a)')'% begin of mshplt_frame3d'

      if (ihigzmode_ps.ne.0.and.isameframe_ps.eq.0.and.kzone_ps.gt.0) then
        if (kzone_ps.lt.nzone_ps) then
          ifirst=kzone_ps+1
          call mshplt_zone(nxzone_ps,nyzone_ps,ifirst,'s')
          else
            if (itouched_ps.ne.0) call mshplt_newpage
          call mshplt_zone(nxzone_ps,nyzone_ps,1,' ')
        endif
      endif

      kcolor=kFramecolor_ps
      kgreen=kFramegreen_ps
      kred=kFramered_ps
      kblue=kFrameblue_ps

      call mshplt_set_frame_color(kcolor,kgreen,kred,kblue)

      if (nzone_ps.le.0) then
        xcorn(1)=-0.5
        xcorn(2)=0.5
        xcorn(3)=0.5
        xcorn(4)=-0.5
        xcorn(5)=-0.5
        xcorn(6)=0.5
        xcorn(7)=0.5
        xcorn(8)=-0.5
        ycorn(1)=-0.5
        ycorn(2)=-0.5
        ycorn(3)=0.5
        ycorn(4)=0.5
        ycorn(5)=-0.5
        ycorn(6)=-0.5
        ycorn(7)=0.5
        ycorn(8)=0.5
        zcorn(1)=-0.5
        zcorn(2)=-0.5
        zcorn(3)=-0.5
        zcorn(4)=-0.5
        zcorn(5)=0.5
        zcorn(6)=0.5
        zcorn(7)=0.5
        zcorn(8)=0.5
      else
        xcorn(1)=-0.625
        xcorn(2)=0.625
        xcorn(3)=0.625
        xcorn(4)=-0.625
        xcorn(5)=-0.625
        xcorn(6)=0.625
        xcorn(7)=0.625
        xcorn(8)=-0.625
        ycorn(1)=-0.625
        ycorn(2)=-0.625
        ycorn(3)=0.625
        ycorn(4)=0.625
        ycorn(5)=-0.625
        ycorn(6)=-0.625
        ycorn(7)=0.625
        ycorn(8)=0.625
        zcorn(1)=-0.625
        zcorn(2)=-0.625
        zcorn(3)=-0.625
        zcorn(4)=-0.625
        zcorn(5)=0.625
        zcorn(6)=0.625
        zcorn(7)=0.625
        zcorn(8)=0.625
      endif

      xtitd=xtit
      ytitd=ytit
      ztitd=ztit
      choptd=chopt

      if(chopt.eq.'') choptd='AB'

      iaxis=0
      ibox=0
      ifbox_ps=0
      do ic=1,len_trim(choptd)
        if (choptd(ic:ic).eq.'A'.or.choptd(ic:ic).eq.'a') iaxis=1
        if (choptd(ic:ic).eq.'B'.or.choptd(ic:ic).eq.'b') then
          ibox=1
          ifbox_ps=1
        endif
      enddo

      ilinestyleo=ilinestyle_ps
      call mshplt_get_line_color(ilinecoloro,kredo,kblueo,kgreeno)

      call mshplt_3dto2d(8,xcorn,ycorn,zcorn,xpcorn_ps,ypcorn_ps)

      wxmin_ps=min(xpcorn_ps(1),xpcorn_ps(2),xpcorn_ps(3),xpcorn_ps(4),
     &  xpcorn_ps(5),xpcorn_ps(6),xpcorn_ps(7),xpcorn_ps(8))
      wxmax_ps=max(xpcorn_ps(1),xpcorn_ps(2),xpcorn_ps(3),xpcorn_ps(4),
     &  xpcorn_ps(5),xpcorn_ps(6),xpcorn_ps(7),xpcorn_ps(8))

      wymin_ps=min(ypcorn_ps(1),ypcorn_ps(2),ypcorn_ps(3),ypcorn_ps(4),
     &  ypcorn_ps(5),ypcorn_ps(6),ypcorn_ps(7),ypcorn_ps(8))
      wymax_ps=max(ypcorn_ps(1),ypcorn_ps(2),ypcorn_ps(3),ypcorn_ps(4),
     &  ypcorn_ps(5),ypcorn_ps(6),ypcorn_ps(7),ypcorn_ps(8))

      scalex_ps=xsiz_ps/(wxmax_ps-wxmin_ps)
      scaley_ps=ysiz_ps/(wymax_ps-wymin_ps)

      if (log10x_ps.eq.0) then
        xmin3d_ps=xmin
        xmax3d_ps=xmax
      else
        if (xmin.le.0.0) xmin=1.0e-30
        if (xmax.le.xmin) xmax=1.0e30
        xmin3d_ps=alog10(xmin)
        xmax3d_ps=alog10(xmax)
      endif

      if (log10y_ps.eq.0) then
        ymin3d_ps=ymin
        ymax3d_ps=ymax
      else
        if (ymin.le.0.0) ymin=1.0e-30
        if (ymax.le.ymin) ymax=1.0e30
        ymin3d_ps=alog10(ymin)
        ymax3d_ps=alog10(ymax)
      endif

      if (log10z_ps.eq.0) then
        zmin3d_ps=zmin
        zmax3d_ps=zmax
      else
        if (zmin.le.0.0) zmin=1.0e-30
        if (zmax.le.zmin) zmax=1.0e30
        zmin3d_ps=alog10(zmin)
        zmax3d_ps=alog10(zmax)
      endif

      call mshplt_set_line_style(1)

      if (iaxis.ne.1.or.theta_ps.gt.90.or.phi_ps.gt.90.) then
        print*,"*** Warning in mshplt_frame3d_xzy: This routine is not ready"
        print*,"for iaxis=0 or theta>90 or phi>90, sorry!"
      endif

      if (iaxis.eq.1) then

        if (theta_ps.le.90.0) then
          if (phi_ps.le.90.0) then

            if (log10x_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(1),xpcorn_ps(2),ypcorn_ps(1),ypcorn_ps(2),
     &          xmin,xmax,xtitd(1:len_trim(xtitd)),-1,0.,0.,
     &          xtitoff_ps,
     &          xlaboff_ps) !x-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(2)-xpcorn_ps(1))**2+
     &          (ypcorn_ps(2)-ypcorn_ps(1))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(2)-ypcorn_ps(1)),
     &          (xpcorn_ps(2)-xpcorn_ps(1))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(1),ypcorn_ps(1),xyplen,
     &          xminin,xmaxin,ang,
     &          chhe,ticheight,xtit,
     &          iticside,ilabside,
     &          titang,ytitoff_ps,
     &          rellabang,xlaboff_ps) !x-axis
            endif

            if (log10y_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(4),xpcorn_ps(1),ypcorn_ps(4),ypcorn_ps(1),
     &          ymin,ymax,ytitd(1:len_trim(ytitd)),-1,0.,0.,
     &          ytitoff_ps,
     &          ylaboff_ps) !y-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(4)-xpcorn_ps(1))**2+
     &          (ypcorn_ps(4)-ypcorn_ps(1))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(1)-ypcorn_ps(4)),
     &          (xpcorn_ps(1)-xpcorn_ps(4))
     &          )*radtodeg_ps
              iticside=+1
              ilabside=1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(4),ypcorn_ps(4),xyplen,
     &          yminin,ymaxin,ang,
     &          chhe,ticheight,ytit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,2.*zlaboff_ps) !y-axis
            endif

            if (log10z_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(4),xpcorn_ps(8),ypcorn_ps(4),ypcorn_ps(8),
     &          zmin,zmax,ztitd(1:len_trim(ztitd)),1,-90.,0.,
     &          -ztitoff_ps,-zlaboff_ps) !z-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(8)-xpcorn_ps(4))**2+
     &          (ypcorn_ps(8)-ypcorn_ps(4))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(8)-ypcorn_ps(4)),
     &          (xpcorn_ps(8)-xpcorn_ps(4))
     &          )*radtodeg_ps
              iticside=1
              ilabside=-1
              titang=0.
              rellabang=-90.
              call mshplt_view_log_axis(
     &          xpcorn_ps(4),ypcorn_ps(4),xyplen,
     &          zminin,zmaxin,ang,
     &          chhe,ticheight,ztit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-zlaboff_ps*1.75) !z-axis
            endif

          else if (phi_ps.le.180.0) then

            if (log10x_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(4),xpcorn_ps(3),ypcorn_ps(4),ypcorn_ps(3),
     &          xmin,xmax,xtitd(1:len_trim(xtitd)),1,180.,180.,
     &          xtitoff_ps,
     &          -xlaboff_ps) !x-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(3)-xpcorn_ps(4))**2+
     &          (ypcorn_ps(3)-ypcorn_ps(4))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(3)-ypcorn_ps(4)),
     &          (xpcorn_ps(3)-xpcorn_ps(4))
     &          )*radtodeg_ps
              iticside=1
              ilabside=-1
              titang=180.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(4),ypcorn_ps(4),xyplen,
     &          xminin,xmaxin,ang,
     &          chhe,ticheight,xtit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-2.*xlaboff_ps) !x-axis
            endif

            if (log10y_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(1),xpcorn_ps(4),ypcorn_ps(1),ypcorn_ps(4),
     &          ymin,ymax,ytitd(1:len_trim(ytitd)),1,180.,180.,
     &          ytitoff_ps,
     &          -ylaboff_ps) !y-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(4)-xpcorn_ps(1))**2+
     &          (ypcorn_ps(4)-ypcorn_ps(1))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(4)-ypcorn_ps(1)),
     &          (xpcorn_ps(4)-xpcorn_ps(1))
     &          )*radtodeg_ps
              iticside=1
              ilabside=1
              titang=180.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(1),ypcorn_ps(1),xyplen,
     &          yminin,ymaxin,ang,
     &          chhe,ticheight,ytit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-1.5*zlaboff_ps) !y-axis
            endif

            if (log10z_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(3),xpcorn_ps(7),ypcorn_ps(3),ypcorn_ps(7),
     &          zmin,zmax,ztitd(1:len_trim(ztitd)),1,-90.,0.,
     &          -ztitoff_ps,-zlaboff_ps) !z-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(7)-xpcorn_ps(3))**2+
     &          (ypcorn_ps(7)-ypcorn_ps(3))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(7)-ypcorn_ps(3)),
     &          (xpcorn_ps(7)-xpcorn_ps(3))
     &          )*radtodeg_ps
              iticside=1
              ilabside=-1
              titang=0.
              rellabang=-90.
              call mshplt_view_log_axis(
     &          xpcorn_ps(3),ypcorn_ps(3),xyplen,
     &          zminin,zmaxin,ang,
     &          chhe,ticheight,ztit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,zlaboff_ps*0.6) !z-axis
            endif

          else if (phi_ps.le.270.0) then

            if (log10x_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(4),xpcorn_ps(3),ypcorn_ps(4),ypcorn_ps(3),
     &          xmin,xmax,xtitd(1:len_trim(xtitd)),1,180.,180.,
     &          xtitoff_ps,
     &          -xlaboff_ps) !x-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(3)-xpcorn_ps(4))**2+
     &          (ypcorn_ps(3)-ypcorn_ps(4))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(3)-ypcorn_ps(4)),
     &          (xpcorn_ps(3)-xpcorn_ps(4))
     &          )*radtodeg_ps
              iticside=1
              ilabside=-1
              titang=180.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(4),ypcorn_ps(4),xyplen,
     &          xminin,xmaxin,ang,
     &          chhe,ticheight,xtit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps*0.7,
     &          rellabang,-2.*xlaboff_ps) !x-axis
            endif

            if (log10y_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(2),xpcorn_ps(3),ypcorn_ps(2),ypcorn_ps(3),
     &          ymin,ymax,ytitd(1:len_trim(ytitd)),-1,0.,0.,
     &          1.1*ytitoff_ps,
     &          1.25*ylaboff_ps) !y-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(3)-xpcorn_ps(2))**2+
     &          (ypcorn_ps(3)-ypcorn_ps(2))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(3)-ypcorn_ps(2)),
     &          (xpcorn_ps(3)-xpcorn_ps(2))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=-1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(2),ypcorn_ps(2),xyplen,
     &          yminin,ymaxin,ang,
     &          chhe,ticheight,ytit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,1.25*zlaboff_ps) !y-axis
            endif

            if (log10z_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(2),xpcorn_ps(6),ypcorn_ps(2),ypcorn_ps(6),
     &          zmin,zmax,ztitd(1:len_trim(ztitd)),1,-90.,0.,
     &          -ztitoff_ps,-zlaboff_ps) !z-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(6)-xpcorn_ps(2))**2+
     &          (ypcorn_ps(6)-ypcorn_ps(2))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(6)-ypcorn_ps(2)),
     &          (xpcorn_ps(6)-xpcorn_ps(2))
     &          )*radtodeg_ps
              iticside=1
              ilabside=-1
              titang=0.
              rellabang=-90.
              call mshplt_view_log_axis(
     &          xpcorn_ps(2),ypcorn_ps(2),xyplen,
     &          zminin,zmaxin,ang,
     &          chhe,ticheight,ztit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-3.*zlaboff_ps*0.6) !z-axis
            endif

          else if (phi_ps.le.360.0) then

            if (log10x_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(1),xpcorn_ps(2),ypcorn_ps(1),ypcorn_ps(2),
     &          xmin,xmax,xtitd(1:len_trim(xtitd)),-1,0.,0.,
     &          1.1*xtitoff_ps,
     &          1.2*xlaboff_ps) !x-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(1)-xpcorn_ps(2))**2+
     &          (ypcorn_ps(1)-ypcorn_ps(2))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(2)-ypcorn_ps(1)),
     &          (xpcorn_ps(2)-xpcorn_ps(1))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=-1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(1),ypcorn_ps(1),xyplen,
     &          xminin,xmaxin,ang,
     &          chhe,ticheight,xtit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps*0.7,
     &          rellabang,xlaboff_ps) !x-axis
            endif

            if (log10y_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(2),xpcorn_ps(3),ypcorn_ps(2),ypcorn_ps(3),
     &          ymin,ymax,ytitd(1:len_trim(ytitd)),-1,0.,0.,
     &          1.2*ytitoff_ps,
     &          1.1*ylaboff_ps) !y-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(3)-xpcorn_ps(2))**2+
     &          (ypcorn_ps(3)-ypcorn_ps(2))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(3)-ypcorn_ps(2)),
     &          (xpcorn_ps(3)-xpcorn_ps(2))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(2),ypcorn_ps(2),xyplen,
     &          yminin,ymaxin,ang,
     &          chhe,ticheight,ytit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps*0.8,
     &          rellabang,zlaboff_ps) !y-axis
            endif

            if (log10z_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(1),xpcorn_ps(5),ypcorn_ps(1),ypcorn_ps(5),
     &          zmin,zmax,ztitd(1:len_trim(ztitd)),1,-90.,0.,
     &          -ztitoff_ps,-zlaboff_ps) !z-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(5)-xpcorn_ps(1))**2+
     &          (ypcorn_ps(5)-ypcorn_ps(1))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(5)-ypcorn_ps(1)),
     &          (xpcorn_ps(5)-xpcorn_ps(1))
     &          )*radtodeg_ps
              iticside=1
              ilabside=-1
              titang=0.
              rellabang=-90.
              call mshplt_view_log_axis(
     &          xpcorn_ps(1),ypcorn_ps(1),xyplen,
     &          zminin,zmaxin,ang,
     &          chhe,ticheight,ztit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-3.*zlaboff_ps*0.6) !z-axis
            endif

          endif !if (phi_ps.le.90.0) then

        else if (theta_ps.le.180.0) then

          if (phi_ps.le.90.0) then

            if (log10x_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(8),xpcorn_ps(7),ypcorn_ps(8),ypcorn_ps(7),
     &          xmin,xmax,xtitd(1:len_trim(xtitd)),-1,0.,0.,
     &          xtitoff_ps,
     &          xlaboff_ps) !x-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(7)-xpcorn_ps(8))**2+
     &          (ypcorn_ps(7)-ypcorn_ps(8))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(7)-ypcorn_ps(8)),
     &          (xpcorn_ps(7)-xpcorn_ps(8))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(8),ypcorn_ps(8),xyplen,
     &          xminin,xmaxin,ang,
     &          chhe,ticheight,xtit,
     &          iticside,ilabside,
     &          titang,ytitoff_ps,
     &          rellabang,xlaboff_ps) !x-axis
            endif

            if (log10y_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(6),xpcorn_ps(7),ypcorn_ps(6),ypcorn_ps(7),
     &          ymin,ymax,ytitd(1:len_trim(ytitd)),1,180.,180.,
     &          ytitoff_ps,
     &          -ylaboff_ps) !y-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(7)-xpcorn_ps(6))**2+
     &          (ypcorn_ps(7)-ypcorn_ps(6))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(7)-ypcorn_ps(6)),
     &          (xpcorn_ps(7)-xpcorn_ps(6))
     &          )*radtodeg_ps
              iticside=1
              ilabside=1
              titang=180.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(6),ypcorn_ps(6),xyplen,
     &          yminin,ymaxin,ang,
     &          chhe,ticheight,ytit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-1.5*zlaboff_ps) !y-axis
            endif

            if (log10z_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(4),xpcorn_ps(8),ypcorn_ps(4),ypcorn_ps(8),
     &          zmin,zmax,ztitd(1:len_trim(ztitd)),1,+90.,0.,
     &          ztitoff_ps,zlaboff_ps) !z-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(8)-xpcorn_ps(4))**2+
     &          (ypcorn_ps(8)-ypcorn_ps(4))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(8)-ypcorn_ps(4)),
     &          (xpcorn_ps(8)-xpcorn_ps(4))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=-1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(4),ypcorn_ps(4),xyplen,
     &          zminin,zmaxin,ang,
     &          chhe,ticheight,ztit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-1.75*zlaboff_ps) !z-axis
            endif

          else if (phi_ps.le.180.0) then

            if (log10x_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(5),xpcorn_ps(6),ypcorn_ps(5),ypcorn_ps(6),
     &          xmin,xmax,xtitd(1:len_trim(xtitd)),1,180.,180.,
     &          1.1*xtitoff_ps,
     &          -xlaboff_ps) !x-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(6)-xpcorn_ps(5))**2+
     &          (ypcorn_ps(6)-ypcorn_ps(5))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(6)-ypcorn_ps(5)),
     &          (xpcorn_ps(6)-xpcorn_ps(5))
     &          )*radtodeg_ps
              iticside=1
              ilabside=1
              titang=180.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(5),ypcorn_ps(5),xyplen,
     &          xminin,xmaxin,ang,
     &          chhe,ticheight,xtit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-1.5*xlaboff_ps) !x-axis
            endif

            if (log10y_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(6),xpcorn_ps(7),ypcorn_ps(6),ypcorn_ps(7),
     &          ymin,ymax,ytitd(1:len_trim(ytitd)),1,180.,180.,
     &          ytitoff_ps,
     &          -ylaboff_ps) !y-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(7)-xpcorn_ps(6))**2+
     &          (ypcorn_ps(7)-ypcorn_ps(6))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(7)-ypcorn_ps(6)),
     &          (xpcorn_ps(7)-xpcorn_ps(6))
     &          )*radtodeg_ps
              iticside=1
              ilabside=1
              titang=180.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(6),ypcorn_ps(6),xyplen,
     &          yminin,ymaxin,ang,
     &          chhe,ticheight,ytit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-2.*zlaboff_ps) !y-axis
            endif

            if (log10z_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(3),xpcorn_ps(7),ypcorn_ps(3),ypcorn_ps(7),
     &          zmin,zmax,ztitd(1:len_trim(ztitd)),-1,+90.,180.,
     &          -ztitoff_ps,zlaboff_ps) !z-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(7)-xpcorn_ps(3))**2+
     &          (ypcorn_ps(7)-ypcorn_ps(3))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(7)-ypcorn_ps(3)),
     &          (xpcorn_ps(7)-xpcorn_ps(3))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=-1
              titang=0.
              rellabang=+90.
              call mshplt_view_log_axis(
     &          xpcorn_ps(3),ypcorn_ps(3),xyplen,
     &          zminin,zmaxin,ang,
     &          chhe,ticheight,ztit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-1.75*zlaboff_ps) !z-axis
            endif

          else if (phi_ps.le.270.0) then

            if (log10x_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(5),xpcorn_ps(6),ypcorn_ps(5),ypcorn_ps(6),
     &          xmin,xmax,xtitd(1:len_trim(xtitd)),1,180.,180.,
     &          1.2*xtitoff_ps,
     &          -1.25*xlaboff_ps) !x-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(6)-xpcorn_ps(5))**2+
     &          (ypcorn_ps(6)-ypcorn_ps(5))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(6)-ypcorn_ps(5)),
     &          (xpcorn_ps(6)-xpcorn_ps(5))
     &          )*radtodeg_ps
              iticside=1
              ilabside=1
              titang=180.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(5),ypcorn_ps(5),xyplen,
     &          xminin,xmaxin,ang,
     &          chhe,ticheight,xtit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps*0.7,
     &          rellabang,-2.*xlaboff_ps) !x-axis
            endif

            if (log10y_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(5),xpcorn_ps(8),ypcorn_ps(5),ypcorn_ps(8),
     &          ymin,ymax,ytitd(1:len_trim(ytitd)),-1,0.,0.,
     &          1.1*ytitoff_ps,
     &          1.25*ylaboff_ps) !y-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(8)-xpcorn_ps(5))**2+
     &          (ypcorn_ps(8)-ypcorn_ps(5))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(8)-ypcorn_ps(5)),
     &          (xpcorn_ps(8)-xpcorn_ps(5))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(5),ypcorn_ps(5),xyplen,
     &          yminin,ymaxin,ang,
     &          chhe,ticheight,ytit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,zlaboff_ps*0.5) !y-axis
            endif

            if (log10z_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(2),xpcorn_ps(6),ypcorn_ps(2),ypcorn_ps(6),
     &          zmin,zmax,ztitd(1:len_trim(ztitd)),-1,+90.,0.,
     &          ztitoff_ps,zlaboff_ps) !z-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(6)-xpcorn_ps(2))**2+
     &          (ypcorn_ps(6)-ypcorn_ps(2))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(6)-ypcorn_ps(2)),
     &          (xpcorn_ps(6)-xpcorn_ps(2))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=-1
              titang=0.
              rellabang=90.
              call mshplt_view_log_axis(
     &          xpcorn_ps(2),ypcorn_ps(2),xyplen,
     &          zminin,zmaxin,ang,
     &          chhe,ticheight,ztit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-1.75*zlaboff_ps) !z-axis
            endif

          else if (phi_ps.le.360.0) then

            if (log10x_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(8),xpcorn_ps(7),ypcorn_ps(8),ypcorn_ps(7),
     &          xmin,xmax,xtitd(1:len_trim(xtitd)),-1,0.,0.,
     &          1.1*xtitoff_ps,
     &          1.2*xlaboff_ps) !x-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(7)-xpcorn_ps(8))**2+
     &          (ypcorn_ps(7)-ypcorn_ps(8))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(7)-ypcorn_ps(8)),
     &          (xpcorn_ps(7)-xpcorn_ps(8))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(8),ypcorn_ps(8),xyplen,
     &          xminin,xmaxin,ang,
     &          chhe,ticheight,xtit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps*0.7,
     &          rellabang,xlaboff_ps*0.5) !x-axis
            endif

            if (log10y_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(5),xpcorn_ps(8),ypcorn_ps(5),ypcorn_ps(8),
     &          ymin,ymax,ytitd(1:len_trim(ytitd)),-1,0.,0.,
     &          1.2*ytitoff_ps,
     &          1.1*ylaboff_ps) !y-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(8)-xpcorn_ps(5))**2+
     &          (ypcorn_ps(8)-ypcorn_ps(5))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(8)-ypcorn_ps(5)),
     &          (xpcorn_ps(8)-xpcorn_ps(5))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=1
              titang=0.
              rellabang=-ang
              call mshplt_view_log_axis(
     &          xpcorn_ps(5),ypcorn_ps(5),xyplen,
     &          yminin,ymaxin,ang,
     &          chhe,ticheight,ytit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,zlaboff_ps) !y-axis
            endif

            if (log10z_ps.eq.0) then
              call mshplt_view_axis(
     &          xpcorn_ps(1),xpcorn_ps(5),ypcorn_ps(1),ypcorn_ps(5),
     &          zmin,zmax,ztitd(1:len_trim(ztitd)),-1,90.,0.,
     &          ztitoff_ps,zlaboff_ps) !z-axis
            else
              xyplen=sqrt(
     &          (xpcorn_ps(5)-xpcorn_ps(1))**2+
     &          (ypcorn_ps(5)-ypcorn_ps(1))**2
     &          )
              ang=atan2(
     &          (ypcorn_ps(5)-ypcorn_ps(1)),
     &          (xpcorn_ps(5)-xpcorn_ps(1))
     &          )*radtodeg_ps
              iticside=-1
              ilabside=-1
              titang=0.
              rellabang=90.
              call mshplt_view_log_axis(
     &          xpcorn_ps(1),ypcorn_ps(1),xyplen,
     &          zminin,zmaxin,ang,
     &          chhe,ticheight,ztit,
     &          iticside,ilabside,
     &          titang,ztitoff_ps,
     &          rellabang,-1.75*zlaboff_ps) !z-axis
            endif

          endif !if (phi_ps.le.90.0) then
        endif !if (theta_ps.le.90.0) then

      endif !iaxis.eq.1

      if (ibox.ne.0) then
        call mshplt_line_raw(xpcorn_ps(1),ypcorn_ps(1),xpcorn_ps(2),ypcorn_ps(2))
        call mshplt_line_raw(xpcorn_ps(1),ypcorn_ps(1),xpcorn_ps(4),ypcorn_ps(4))
        call mshplt_line_raw(xpcorn_ps(4),ypcorn_ps(4),xpcorn_ps(8),ypcorn_ps(8))
        call mshplt_line_raw(xpcorn_ps(4),ypcorn_ps(4),xpcorn_ps(3),ypcorn_ps(3))
        call mshplt_line_raw(xpcorn_ps(2),ypcorn_ps(2),xpcorn_ps(3),ypcorn_ps(3))
        call mshplt_line_raw(xpcorn_ps(1),ypcorn_ps(1),xpcorn_ps(5),ypcorn_ps(5))
        call mshplt_line_raw(xpcorn_ps(2),ypcorn_ps(2),xpcorn_ps(6),ypcorn_ps(6))
        call mshplt_line_raw(xpcorn_ps(3),ypcorn_ps(3),xpcorn_ps(7),ypcorn_ps(7))
        call mshplt_pline_raw_closed(4,xpcorn_ps(5),ypcorn_ps(5))
      endif

      call mshplt_get_line_color(ilinecoloro,kredo,kblueo,kgreeno)
      call mshplt_set_line_style(ilinestyleo)

      if (kzone_ps.lt.0) then
        kzone_ps=ifirst_ps
      endif

      write(lun_ps,'(a)')'% end of mshplt_frame3d'

      return
      end
