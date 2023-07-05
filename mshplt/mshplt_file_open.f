*CMZ :  1.03/01 07/10/2014  15.16.02  by  Michael Scheer
*CMZ :  1.02/00 02/10/2014  21.58.12  by  Michael Scheer
*CMZ :  1.01/02 27/09/2014  16.14.01  by  Michael Scheer
*CMZ :  0.01/02 12/09/2014  09.10.17  by  Michael Scheer
*CMZ :  0.01/00 23/08/2014  09.58.55  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.02.41  by  Michael Scheer
*CMZ :  0.00/05 18/08/2014  15.01.02  by  Michael Scheer
*CMZ :  0.00/04 16/08/2014  15.29.25  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_file_open(luni,file,chopt,ierr)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(10) dtday,dttime,dtzone
      integer idatetime(8),lun,luni,ierr,ifound,lunmax,i
      integer lchopt,lfile,l1num,l2num,nlun
      character(*) file,chopt
      character(2048) cline,cnum

      character(8192) fileeps

      if (nfile_ps.ge.nmaxfile_ps) then
        ierr=-1000
        return
      endif

      lun=luni

      write(cnum,*)nfile_ps

      lunmax=0
      do i=1,nfile_ps
        if (lunall_ps(nfile_ps).gt.lunmax) lunmax=lunall_ps(nfile_ps)
      enddo

      ifound=0
      do i=1,nfile_ps
        if (lunall_ps(nfile_ps).eq.luni) ifound=1
      enddo

      if (ifound.eq.1) then
        lun=lunmax+1
        print*,'*** Warning in mshplt_file_open: LUN',luni,'is in use!'
        print*,'*** Changed to',lun
      endif

      nfile_ps=nfile_ps+1

      l2num=len_trim(cnum)
      do l1num=1,l2num
        if (cnum(l1num:l1num).ne.' ') goto 9
      enddo

9     continue

      lfile=len_trim(file)

      if (lun.gt.0) then
        nlun=lun
        if (file(lfile-2:lfile).ne.'eps') then
          filebase_ps=file(1:lfile)
          fileeps=file(1:lfile)//'_'//cnum(l1num:l2num)//'.eps'
        else
          filebase_ps=file(1:lfile-4)
          fileeps=file(1:lfile-4)//'_'//cnum(l1num:l2num)//'.eps'
        endif
      else
        nlun=lun_ps+1
        fileeps=filebase_ps(1:len_trim(filebase_ps))//'_'//cnum(l1num:l2num)//'.eps'
      endif

      if (nfile_ps.eq.1) fileeps=file

      lfile=len_trim(fileeps)
      lchopt=len_trim(chopt)

      open(unit=nlun,file=fileeps(1:lfile),status='unknown',iostat=ierr)
      file_ps=fileeps(1:lfile)

      CALL date_and_time(dtday,dttime,dtzone,idatetime)

      lun_ps=nlun

      write(lun_ps,'(a)')'%!PS-Adobe-2.0'
      write(cline,*)'%%BoundingBox: ',kbbxl_ps,kbbyb_ps,kbbxr_ps,kbbyt_ps

      write(lun_ps,'(a)')cline(2:len_trim(cline))
      write(lun_ps,'(a)')'%%Title: '//file_ps(1:len_trim(file_ps))
      write(lun_ps,'(a)')'%%Pages: (atend)'
c      write(lun_ps,'(a)')'%%Creator: mshplt'
*KEEP,MSHPLTVERSION.
      write(lun_ps,'(a)') '%%Creator mshplt 1.03/02'
*KEND.
      write(cline,*)'%%CreationDate: ',
     &  dtday(1:4),'.',dtday(5:6),'.',dtday(7:8),'   ',
     &  dttime(1:2),':',dttime(3:4),':',dttime(5:6)
      write(lun_ps,'(a)')cline(2:len_trim(cline))
      write(lun_ps,'(a)')'%%EndComments'

      write(lun_ps,*)scale_ps,scale_ps,' scale'
      write(lun_ps,*)rlinewidth_ps,' setlinewidth'
      write(lun_ps,*)xleftorig_ps,ybottomorig_ps,' moveto %goto origin'

      call mshplt_draw_title

      if (ihigzmode_ps.ne.0) then
        write(lun_ps,*)'/Helvetica findfont ',chhe_ps*scaletxt_ps*1.5,
     &    ' scalefont setfont'
      else
        write(lun_ps,*)'/Helvetica findfont ',chhe_ps*scaletxt_ps,
     &    ' scalefont setfont'
      endif

      if (inewpage_ps.ne.0) then

        if (kBox_ps.gt.0) then
          write(lun_ps,*)xsizorig_ps,0.0,' rlineto'
          write(lun_ps,*)0.0,ysizorig_ps,' rlineto'
          write(lun_ps,*)-xsizorig_ps,0.0,' rlineto'
          write(lun_ps,*)0.0,-ysizorig_ps,' rlineto'
          write(lun_ps,*)'stroke'
        endif

        if (ihigzmode_ps.ne.0) then

          if (kDate_ps.gt.0) then
            write(lun_ps,*)xrightorig_ps+offdatex_ps*scaletxt_ps*1.5,
     &        ytoporig_ps+offdatey_ps*scaletxt_ps-1.25,
     &        'moveto ( ',
     &        dtday(1:4),'.',dtday(5:6),'.',dtday(7:8),'   ',
     &        dttime(1:2),':',dttime(3:4),
     &        ') show'
          endif

        else

          if (kDate_ps.gt.0) then
            write(lun_ps,*)xrightorig_ps+offdatex_ps,
     &        ytoporig_ps+offdatey_ps,
     &        'moveto ( ',
     &        dtday(1:4),'.',dtday(5:6),'.',dtday(7:8),'   ',
     &        dttime(1:2),':',dttime(3:4),
     &        ') show'
          endif

        endif

        inewpage_ps=0
      endif

      write(lun_ps,*)xleftorig_ps,ybottomorig_ps,' moveto %goto origin'
      lunall_ps(nfile_ps)=nlun
      fileall_ps(nfile_ps)=fileeps(1:lfile)

      if (irunviewer_ps.ne.0) then
        call system(
     &    viewer_ps(1:len_trim(viewer_ps))//' '//
     &    file_ps(1:len_trim(file_ps)))
      endif

      write(lun_ps,'(a)')'% end of mshplt_file_open'
      return
      end
