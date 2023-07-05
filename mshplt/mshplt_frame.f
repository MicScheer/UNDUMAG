*CMZ :          04/03/2023  17.35.33  by  Michael Scheer
*CMZ :  1.03/02 25/04/2016  12.19.28  by  Michael Scheer
*CMZ :  1.03/01 10/10/2014  13.29.05  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  11.06.23  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  16.33.15  by  Michael Scheer
*CMZ :  0.01/02 22/09/2014  20.47.10  by  Michael Scheer
*CMZ :  0.01/01 27/08/2014  08.54.49  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.47.40  by  Michael Scheer
*CMZ :  0.00/05 18/08/2014  13.15.26  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 05/08/2014  15.57.52  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.42.14  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_frame(xmini,xmaxi,ymini,ymaxi,xtit,ytit,chopt)

c Draw frame of axis, ticksize is ticsiz_ps
c chopt controls with axis are to be drawn:
c        L: Left axis with labels
c        l: Left axis without labels
c        R: Right axis with labels
c        r: Right axis without labels
c        B: Bottom axis with labels
c        b: Bottom axis without labels
c        T: Top axis with labels
c        t: Top axis without labels
c
c Option chopt:
c       L: Left axis with labels
c       l: Left axis without labels
c       B: Bottom axis with labels
c       b: Bottom axis without labels
c       R: Right axis with labels
c       r: right axis without labels
c       T: Top axis with labels
c       t: top axis without labels
c       c: Clipping mode is on

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xmin,xmax,ymin,ymax,wxmn,wxmx,wymn,wymx,x(4),y(4),
     &  xmini,xmaxi,ymini,ymaxi,
     &  xminin,xmaxin,yminin,ymaxin,
     &  theo,phio
      real sizlab(100),xlabrel(100),ylaboffset(100),anglabrel(100),ticlen(100),
     &  ticposrel(100),titoff,titangrel,ticangrel(100),titsiz,titposrel

      integer il,ir,ib,it,i,iclip,ix,iy,ifirst
      integer kred,kgreen,kblue,kcolor,nlab,ntic
      integer kredo,kgreeno,kblueo,kcoloro

      character(*) xtit,ytit,chopt
      character(2048) copt
      character c1
      character(12) chlab(100)
      call mshplt_flush_buff

      write(lun_ps,'(a)')'% begin of mshplt_frame'

      if (idrawgtit_ps.eq.1) call mshplt_draw_title

      theo=theta_ps
      phio=phi_ps

      if (len_trim(chopt).eq.0) then
        copt='LBrtc'
      else
        copt=chopt
      endif

      isameframe_ps=0
      iclip=0
      il=0
      ir=0
      ib=0
      it=0
      inolabs_ps=1

      kcolor=kFramecolor_ps
      kgreen=kFramegreen_ps
      kred=kFramered_ps
      kblue=kFrameblue_ps

      call mshplt_set_frame_color(kcolor,kgreen,kred,kblue)
      call mshplt_get_line_color(kcoloro,kgreeno,kredo,kblueo)
      call mshplt_set_line_color(kcolor,kgreen,kred,kblue)

      do i=1,len_trim(copt)

        c1=copt(i:i)

        !Left axis
        if (c1.eq.'L') then
          il=1
          if (chhe_ps.eq.0.0) il=-1
        else if (c1.eq.'l') then
          il=-1

        !Right axis
        else if (c1.eq.'R') then
          ir=1
          if (chhe_ps.eq.0.0) ir=-1
        else if (c1.eq.'r') then
          ir=-1

        !Bottom axis
        else if (c1.eq.'B') then
          ib=1
          if (chhe_ps.eq.0.0) ib=-1
        else if (c1.eq.'b') then
          ib=-1

        !Top axis
        else if (c1.eq.'T') then
          it=1
          if (chhe_ps.eq.0.0) it=-1
        else if (c1.eq.'t') then
          it=-1
        else if (c1.eq.'s'.or.c1.eq.'S') then
          isameframe_ps=1
        else if (c1.eq.'c') then
          iclip=1
        endif
      enddo

      if (ihigzmode_ps.ne.0.and.isameframe_ps.eq.0.and.kzone_ps.gt.0) then
        ix=nxzone_ps
        iy=nyzone_ps
        if (kzone_ps.lt.nzone_ps) then
          ifirst=kzone_ps+1
          call mshplt_zone(ix,iy,ifirst,'s')
          else
            if (itouched_ps.ne.0) call mshplt_newpage
          call mshplt_zone(ix,iy,1,' ')
        endif
      endif

      !convert to pad system
      wxmn=xleft_ps
      wxmx=xright_ps
      wymn=ybottom_ps
      wymx=ytop_ps

      xminin=xmini
      xmaxin=xmaxi
      yminin=ymini
      ymaxin=ymaxi

      if (xminin.ne.xminin) then
          xminin=-1.0e30
      endif

      if (xminin.ne.0.0) then
        if (1.0/xminin.eq.0.0) then
          if (xminin.lt.0.0) then
            xminin=-1.0e30
          else
            xminin=1.0e30
          endif
        endif
      endif

      if (xmaxin.ne.xmaxin) then
          xmaxin=1.0e30
      endif

      if (xmaxin.ne.0.0) then
        if (1.0/xmaxin.eq.0.0) then
          if (xmaxin.lt.0.0) then
            xmaxin=-1.0e30
          else
            xmaxin=1.0e30
          endif
        endif
      endif

      if (xminin.eq.xmaxin) then
        xmin=xminin-0.5
        xmax=xmaxin+0.5
      endif

      if (yminin.ne.yminin) then
          yminin=-1.0e30
      endif
      if (yminin.ne.0.0) then
        if (1.0/yminin.eq.0.0) then
          if (yminin.lt.0.0) then
            yminin=-1.0e30
          else
            yminin=1.0e30
          endif
        endif
      endif

      if (ymaxin.ne.ymaxin) then
          ymaxin=1.0e30
      endif
      if (ymaxin.ne.0.0) then
        if (1.0/ymaxin.eq.0.0) then
          if (ymaxin.lt.0.0) then
            ymaxin=-1.0e30
          else
            ymaxin=1.0e30
          endif
        endif
      endif

      xmin=xminin
      xmax=xmaxin

      ymin=yminin
      ymax=ymaxin

      if (yminin.eq.ymaxin) then
        ymin=yminin-0.5
        ymax=ymaxin+0.5
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

      wymin_ps=ymin
      wymax_ps=ymax
      scaley_ps=ysiz_ps/(wymax_ps-wymin_ps)

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

      wxmin_ps=xmin
      wxmax_ps=xmax
      scalex_ps=xsiz_ps/(wxmax_ps-wxmin_ps)

      write(lun_ps,'(a)')'% frame: xleft_ps xright_ps ybottom_ps ytop_ps:'
      write(lun_ps,*)'%',xleft_ps,xright_ps,ybottom_ps,ytop_ps
      write(lun_ps,'(a)')cline_ps(2:len_trim(cline_ps))
      write(lun_ps,'(a)')'% size: xsiz_ps ysiz_ps:'
      write(lun_ps,*)'%',xsiz_ps,ysiz_ps
      write(lun_ps,'(a)')cline_ps(2:len_trim(cline_ps))

      write(lun_ps,*)'% coord.-system: wxmin_ps wxmax_ps:',
     &  wxmin_ps,wxmax_ps
      write(lun_ps,*)'% coord.-system: wymin_ps wymax_ps:',
     &  wymin_ps,wymax_ps
      write(lun_ps,'(a)')cline_ps(2:len_trim(cline_ps))

      if (il.ne.0) then

        if (il.lt.0) then
          inolabs_ps=1
        else
          inolabs_ps=0
        endif

        if (log10y_ps.eq.0) then
          nlab=-100
          sizlab=aLabHeight_ps*(1-inolabs_ps)
          ylaboffset=-ylaboff_ps
          anglabrel=-90.
          ticlen=-ticsiz_ps
          ticangrel=90.
          titsiz=tsiz_ps*(1-inolabs_ps)
          titoff=-ytitoff_ps
          titangrel=0.
          titposrel=0.5
          call mshplt_axis_taylor(
     &      xmin,xmin,ymin,ymax,ymin,ymax,
     &      nlab,chlab,sizlab,xlabrel,ylaboffset,
     &      xoffexp_ps,yoffexp_ps,
     &      anglabrel,
     &      ntic,ticlen,ticposrel,ticangrel,
     &      titsiz,titposrel,titoff,titangrel,ytit
     &      )
c          call mshplt_world_axis(wxmn,wxmn,wymn,wymx,ymin,ymax,ytit,
c     &      -1,0,0.,ytitoff_ps*scaletxt_ps,ylaboff_ps*scaletxt_ps)
        else
          call mshplt_world_log_axis(wxmn,wxmn,wymn,wymx,
     &      10.**ymin,10.**ymax,ytit,
     &      -1,-(1-iNoLabs_ps),0.,ytitoff_ps*scaletxt_ps,
     &      ylaboff_ps*scaletxt_ps)
        endif

      endif !il

      if (ir.ne.0) then

        if (ir.lt.0) then
          inolabs_ps=1
        else
          inolabs_ps=0
        endif

        if (log10y_ps.eq.0) then
          nlab=-100
          sizlab=aLabHeight_ps*(1-inolabs_ps)
          ylaboffset=ylaboff_ps
          anglabrel=-90.
          ticlen=ticsiz_ps
          ticangrel=90.
          titsiz=tsiz_ps*(1-inolabs_ps)
          titoff=ytitoff_ps
          titangrel=0.
          titposrel=0.5
          call mshplt_axis_taylor(
     &      xmax,xmax,ymin,ymax,yminin,ymaxin,
     &      nlab,chlab,sizlab,xlabrel,ylaboffset,
     &      xoffexp_ps,yoffexp_ps,
     &      anglabrel,
     &      ntic,ticlen,ticposrel,ticangrel,
     &      titsiz,titposrel,titoff,titangrel,ytit
     &      )
c          call mshplt_world_axis(wxmx,wxmx,wymn,wymx,ymin,ymax,ytit,
c     &      1,0,0.,ytitoff_ps*scaletxt_ps,ylaboff_ps*scaletxt_ps)
        else
          call mshplt_world_log_axis(
     &      wxmx,wxmx,wymn,wymx,10.**ymin,10.**ymax,ytit,
     &      1,(1-iNoLabs_ps),0.,
     &      ytitoff_ps*scaletxt_ps,ylaboff_ps*scaletxt_ps)
        endif

      endif !ir

      if (ib.ne.0) then

        if (ib.lt.0) then
          inolabs_ps=1
        else
          inolabs_ps=0
        endif

        if (log10x_ps.eq.0) then
          nlab=-100
          sizlab=aLabHeight_ps*(1-inolabs_ps)
          ylaboffset=xlaboff_ps
          anglabrel=0.
          ticlen=ticsiz_ps
          ticangrel=90.
          titsiz=tsiz_ps*(1-inolabs_ps)
          titoff=ytitoff_ps
          titangrel=0.
          titposrel=0.5
          call mshplt_axis_taylor(
     &      xmin,xmax,ymin,ymin,xminin,xmaxin,
     &      nlab,chlab,sizlab,xlabrel,ylaboffset,
     &      xoffexp_ps,yoffexp_ps,
     &      anglabrel,
     &      ntic,ticlen,ticposrel,ticangrel,
     &      titsiz,titposrel,titoff,titangrel,xtit
     &      )
c          call mshplt_world_axis(wxmn,wxmx,wymn,wymn,xmin,xmax,xtit,
c     &      1,1,0.,-xtitoff_ps*scaletxt_ps,-xlaboff_ps*scaletxt_ps)
        else
          call mshplt_world_log_axis(
     &      wxmn,wxmx,wymn,wymn,10.**xmin,10.**xmax,xtit,
     &      -1,-(1-iNoLabs_ps),
     &      0.,xtitoff_ps*scaletxt_ps,xlaboff_ps*scaletxt_ps)
        endif
      endif !ib

      if (it.ne.0) then
        if (it.lt.0) then
          inolabs_ps=1
        else
          inolabs_ps=0
        endif

        if (log10x_ps.eq.0) then
          nlab=-100
          sizlab=aLabHeight_ps*(1-inolabs_ps)
          ylaboffset=-xlaboff_ps
          anglabrel=0.
          ticlen=-ticsiz_ps
          ticangrel=90.
          titsiz=tsiz_ps*(1-inolabs_ps)
          titoff=-xtitoff_ps
          titangrel=0.
          titposrel=0.5
          call mshplt_axis_taylor(
     &      xmin,xmax,ymax,ymax,xminin,xmaxin,
     &      nlab,chlab,sizlab,xlabrel,ylaboffset,
     &      xoffexp_ps,yoffexp_ps,
     &      anglabrel,
     &      ntic,ticlen,ticposrel,ticangrel,
     &      titsiz,titposrel,titoff,titangrel,xtit
     &      )
c          call mshplt_world_axis(wxmn,wxmx,wymx,wymx,xmin,xmax,xtit,
c     &      -1,0,0.,-xtitoff_ps*scaletxt_ps,xlaboff_ps*scaletxt_ps)
        else
          call mshplt_world_log_axis(
     &      wxmn,wxmx,wymx,wymx,10.**xmin,10.**xmax,xtit,
     &      1,(1-iNoLabs_ps),0.,
     &      (xtitoff_ps-chhe_ps)*scaletxt_ps,xlaboff_ps*scaletxt_ps)
        endif
      endif

      x(1)=xmin
      y(1)=ymin
      x(2)=xmax
      y(2)=y(1)
      x(3)=x(2)
      y(3)=ymax
      x(4)=x(1)
      y(4)=y(3)
      if (iclip.eq.1) then
        call mshplt_clip(4,x,y)
      else
        call mshplt_reset_clipping
      endif

      if (kzone_ps.lt.0) then
        kzone_ps=ifirst_ps
      endif

c      iColor_ps=kcolor
c      ired_ps=kred
c      igreen_ps=kgreen
c      iblue_ps=kblue

      call mshplt_set_line_color(kcoloro,kgreeno,kredo,kblueo)
      call mshplt_set_theta_phi(theo,phio) ! due to axis routines

      write(lun_ps,'(a)')'% end of mshplt_frame'

      return
      end
