*CMZ :          31/07/2018  11.59.03  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  14.19.07  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.42.16  by  Michael Scheer
*CMZ :  0.01/01 27/08/2014  15.12.25  by  Michael Scheer
*CMZ :  0.01/00 25/08/2014  14.37.06  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  11.44.36  by  Michael Scheer
*CMZ :  0.00/04 18/08/2014  08.59.57  by  Michael Scheer
*CMZ :  1.17/00 06/05/2014  19.24.52  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  16.30.12  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mplint(jdev)

      implicit none

      integer idev,jdev

*KEEP,MPLOTINCL.
      include 'mplot.cmn'
*KEND.
      integer lunbase
      logical lexist
      real xsiz,ysiz
      integer ibbxl,ibbyb,ibbxr,ibbyt,lenfile,lenview,lenkview,i

      idev=jdev



      lunbase=100000
      viewer_mshplt=''
      fileeps_mshplt='mshplt.eps'
      xsiz=-20.
      ysiz=-20.
      rescale_mshplt=1.
      inquire(file='.mshplt.cnf',exist=lexist)
      if (lexist) then
c        call util_get_free_lun(lunbase)
        open(newunit=lunbase,file='.mshplt.cnf',status='old')
        read(lunbase,*)idev
        read(lunbase,*)xsiz,ysiz
        read(lunbase,*)rescale_mshplt
        read(lunbase,*)ibbxl, ibbyb,ibbxr,ibbyt
        read(lunbase,'(a)')fileeps_mshplt
        if(idev.ne.0) then
          read(lunbase,'(a)')viewer_mshplt
          read(lunbase,'(a)')viewer_kill_mshplt
        endif
        close(lunbase)
      endif

      lenfile=0
      do i=1,len_trim(fileeps_mshplt)
        if (fileeps_mshplt(i:i).eq.'!') goto 99
        lenfile=i
      enddo

99    continue

      lenview=0
      do i=1,len_trim(viewer_mshplt)
        if (viewer_mshplt(i:i).eq.'!') goto 99
        lenview=i
      enddo

      lenkview=0
      do i=1,len_trim(viewer_kill_mshplt)
        if (viewer_kill_mshplt(i:i).eq.'!') goto 99
        lenkview=i
      enddo

      if (idev.ne.0) idev=1

      ! negative size values indicate HIGZ mode for mshplt
      call mshplt_init(idev,
     &  xsiz,ysiz,
     &  ibbxl,ibbyb,ibbxr,ibbyt,
     &  fileeps_mshplt(1:lenfile)
     &  ,viewer_mshplt(1:lenview)
     &  ,viewer_kill_mshplt(1:lenkview),rescale_mshplt
     &  )
      call mplzon(1,1,1,'')
      return
      end
