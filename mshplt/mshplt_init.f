*CMZ :          03/08/2018  14.48.39  by  Michael Scheer
*CMZ :  1.03/02 22/09/2016  17.00.47  by  Michael Scheer
*CMZ :  1.03/01 10/10/2014  08.51.19  by  Michael Scheer
*CMZ :  1.02/00 01/10/2014  11.55.05  by  Michael Scheer
*CMZ :  1.01/02 27/09/2014  16.22.46  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  14.15.23  by  Michael Scheer
*CMZ :  1.00/01 23/09/2014  22.42.58  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  13.59.29  by  Michael Scheer
*CMZ :  0.01/02 22/09/2014  17.08.09  by  Michael Scheer
*CMZ :  0.01/01 26/08/2014  14.55.55  by  Michael Scheer
*CMZ :  0.01/00 25/08/2014  15.21.06  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.08.05  by  Michael Scheer
*CMZ :  0.00/05 18/08/2014  14.47.51  by  Michael Scheer
*CMZ :  0.00/04 18/08/2014  10.08.48  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.01.11  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  16.15.45  by  Michael Scheer
*CMZ :  0.00/01 07/07/2014  09.10.33  by  Michael Scheer
*CMZ : 00.00/02 04/07/2014  21.01.32  by  Michael Scheer
*-- Author :    Michael Scheer   27/06/2014
      subroutine mshplt_init(idev,xpapsiz,ypapsiz,ibxl,ibyb,ibxr,ibyt,
     &  file,viewer,viewerkill,rescale)

      implicit none

      real xpapsiz,ypapsiz,rescale
      integer lun,i,ierr,ibxl,ibxr,ibyb,ibyt,idev
      character(*) file,viewer,viewerkill

*KEEP,MSHPLTINCL.
      include 'mshplt.cmn'
*KEEP,MPLOT.

      real rescale_mshplt

      character(2048)
     &  fileeps_mshplt,
     &  viewer_mshplt,viewer_kill_mshplt

      common/mplotc/
     &  rescale_mshplt,
     &  fileeps_mshplt,
     &  viewer_mshplt,viewer_kill_mshplt
*KEND.

      logical lisopen

      lunbase_ps=100000
1     inquire(unit=lunbase_ps,opened=lisopen)
      if (lisopen) then
        lunbase_ps=lunbase_ps+1000
        if (lunbase_ps.gt.10000000) then
          write(6,*)'*** Error in mshplt_init: No free LUN base found ***'
          stop
        endif
        goto 1
      endif

      lun=lunbase_ps+1

      if (xpapsiz.lt.0..or.ypapsiz.lt.0) then
        ihigzmode_ps=1
      else
        ihigzmode_ps=0
      endif

      viewer_ps=viewer(1:len_trim(viewer))
      viewer_kill_mshplt=viewerkill(1:len_trim(viewerkill))

      if (rescale.le.0) then
        rescale_mshplt=1.
      else
        rescale_mshplt=rescale
      endif


      irunviewer_ps=1
      if (len_trim(viewer).eq.0.or.idev.eq.0) irunviewer_ps=0
      if (irunviewer_ps.ne.0.and.idev.ne.0) then
        iviewinter_ps=1
      else
        iviewinter_ps=1
      endif

      istat_ps=0
      nfile_ps=0
      inolabs_ps=0

      radtodeg_ps=180./(4.*atan(1.))

      xsizorig_ps=abs(xpapsiz) !cm
      ysizorig_ps=abs(ypapsiz) !cm
      if (xsizorig_ps.le.0) xsizorig_ps=15.
      if (ysizorig_ps.le.0) ysizorig_ps=15.
      xleftorig_ps=4.
      ybottomorig_ps=4.
      xrightorig_ps=xleftorig_ps+xsizorig_ps !cm
      ytoporig_ps=ybottomorig_ps+ysizorig_ps !cm

      kbbxl_ps=ibxl
      kbbxr_ps=ibxr
      kbbyb_ps=ibyb
      kbbyt_ps=ibyt

      if (
     &    kbbxl_ps.lt.0.or.kbbxr_ps.lt.0.or.
     &    kbbyb_ps.lt.0.or.kbbyt_ps.lt.0) then
        kbbxl_ps=int(50. - 50.*(xsizorig_ps-10.)/10.)
        kbbxr_ps=int(450. + 350.*(xsizorig_ps-10.)/10.)
        kbbyb_ps=kbbxl_ps
        kbbyt_ps=kbbxr_ps
      endif

      if (
     &    kbbxl_ps.lt.0.or.kbbxr_ps.lt.0.or.
     &    kbbyb_ps.lt.0.or.kbbyt_ps.lt.0) then
        kbbxl_ps=0
        kbbxr_ps=400
        kbbyb_ps=0
        kbbyt_ps=400
      endif

      open(unit=99,file='.mshplt.bb',status='unknown')
      write(99,*) kbbxl_ps,kbbxr_ps,kbbyb_ps,kbbyt_ps
      close(99)

      mode3d_ps=0

      idrawgtit_ps=0 !to control drawing of global title

      xsiz_ps=xsizorig_ps !cm
      ysiz_ps=ysizorig_ps !cm

      if (isscale_ps.ne.1) scale_ps=pttocm_ps

      xleft_ps=xleftorig_ps !cm
      xright_ps=xleft_ps+xsizorig_ps !cm
      ybottom_ps=ybottomorig_ps !cm
      ytop_ps=ybottom_ps+ysizorig_ps !cm

      x_ps=xleft_ps
      y_ps=ybottom_ps

      ang_ps=0. !degree
      tang_ps=0. !degree
      pi_ps=4.*atan(1.)

      wxmin_ps=0.
      wxmax_ps=xsiz_ps
      wymin_ps=0.
      wymax_ps=ysiz_ps

      scalex_ps=xsiz_ps/(wxmax_ps-wxmin_ps)
      scaley_ps=ysiz_ps/(wymax_ps-wymin_ps)

      ilabmod_ps=0

      scaletxt_ps=ysizorig_ps/20.*1.5
      if (ihigzmode_ps.ne.0) then
        scaletxt_ps=ysizorig_ps/20.
      endif
      call mshplt_set_character_height(0.3) !cm
      call mshplt_set_index_height(0.3*0.8) !cm
      ticsiz_ps=0.2*scaletxt_ps !cm

      offgtitx_ps=-2. !cm
      offgtity_ps=4.*chhe_ps

      if (isoffdate_ps.ne.1) then
        offdatex_ps=-10*chhe_ps
        offdatey_ps=1.5*chhe_ps
      endif

      rlinewidth_ps=0.03*scaletxt_ps !cm

      xlaboff_ps=2.*chhe_ps !offset of x-axis label cm
      ylaboff_ps=2.*chhe_ps !offset of y-axis label cm
      zlaboff_ps=2.5*chhe_ps !offset of z-axis label cm

      xoffexp_ps=-2.0*chhe_ps! long. offset of power term
      yoffexp_ps=ylaboff_ps+1.5*chhe_ps ! transv. offset of power

      xtitoff_ps=4.0*chhe_ps !offset of x-axis title cm
      ytitoff_ps=4.0*chhe_ps !offset of y-axis title cm
      ztitoff_ps=5.*chhe_ps !offset of z-axis title cm

      ygti_ps=1.5*chhe_ps !cm
      gsiz_ps=2.*chhe_ps !cm

      inewpage_ps=2

      ibuffpos_ps=0

      do i=1,9
          write(chch_ps(i),'(a,i1,a)')'\',i,'  '
      enddo
      do i=10,99
        write(chch_ps(i),'(a,i2,a)')'\',i,' '
      enddo
      do i=100,999
        write(chch_ps(i),'(a,i3)')'\',i
      enddo

      ! Font Symbol
      rmtyp_ps(1,0)=456.!dot
      rmtyp_ps(2,0)=-0.04
      rmtyp_ps(1,1)=267. !bullet
      rmtyp_ps(2,1)=-0.33
      rmtyp_ps(1,2)=250. !volle Raute
      rmtyp_ps(2,2)=-0.25
      rmtyp_ps(1,3)=264. !diag. cross
      rmtyp_ps(2,3)=-0.265
      rmtyp_ps(1,4)=340. !hohle Raute
      rmtyp_ps(2,4)=-0.37
      rmtyp_ps(1,5)=304. !Circle with diag. cross
      rmtyp_ps(2,5)=-0.35
      rmtyp_ps(1,6)=305. !Circle with cross
      rmtyp_ps(2,6)=-0.35
      rmtyp_ps(1,6)=452. !Asterix
      rmtyp_ps(2,6)=-0.33
      rmtyp_ps(1,7)=453. !Plus
      rmtyp_ps(2,7)=-0.27
      rmtyp_ps(1,8)=43. !Hash
      rmtyp_ps(2,8)=-0.27
      rmtyp_ps(1,9)=-1 !circle, drawn by mshplt_circle
      rmtyp_ps(2,9)=0.

      !variables for hplot zones

      if (ihigzmode_ps.ne.0) then
        nxzone_ps=1
        nyzone_ps=1
        nzone_ps=nxzone_ps*nyzone_ps
      endif

      xmgl_ps=0.5/scaletxt_ps !cm
      xmgr_ps=0.5/scaletxt_ps !cm
      ymgl_ps=2.0/scaletxt_ps !cm
      ymgu_ps=2.0/scaletxt_ps !cm
      xwin_ps=2.5/scaletxt_ps !cm
      ywin_ps=2.5/scaletxt_ps !cm

      xtit_ps=''
      ytit_ps=''
      ztit_ps=''
      gtit_ps=''

      tsiz_ps=chhe_ps

      if (isDate_ps.ne.1) kDate_ps=1
      if (isBox_ps.ne.1) kBox_ps=0

      if (nfile_ps.eq.0) then
        call mshplt_file_open(lun,file,'',ierr)
        if (ierr.ne.0) goto 9999
      endif

      call mshplt_set_marker_type(1)
      call mshplt_set_marker_size(0.4) !cm

      call mshplt_set_theta_phi(60.,30.)

      call mshplt_set_color(1,0,0,0)
      call mshplt_set_fill_color(3,0,0,0)
      call mshplt_set_line_color(1,0,0,0)
      call mshplt_set_text_color(1,0,0,0)
      call mshplt_set_frame_color(1,0,0,0)
      call mshplt_set_marker_color(1,0,0,0)

      call mshplt_set_label_size(0.3)

      write(lun_ps,'(a)')'% end of mshplt_init'

      return

 9999 istat_ps=-1

      return
      end
