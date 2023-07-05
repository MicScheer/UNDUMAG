*CMZ :          27/10/2017  08.39.09  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 23/08/2014  09.16.36  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.16.33  by  Michael Scheer
*CMZ :  0.00/05 18/08/2014  14.45.59  by  Michael Scheer
*CMZ :  0.00/04 18/08/2014  10.16.50  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_zone(nxzon,nyzon,ifirst,chopt)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer nxzon,nyzon,ifirst,ix,iy,i
      real wid,hig,xl,xr,yl,yt
      character(*) chopt
      character choptd
      integer ic
      equivalence (ic,choptd)

      call mshplt_flush_buff

      choptd=chopt
      if (len_trim(chopt).ne.0.and.ic.ne.0) then
        write(cline_ps,*)'% begin of mshplt_zone',nxzon,nyzon,ifirst,chopt
      else
        write(cline_ps,*)'% begin of mshplt_zone',nxzon,nyzon,ifirst
      endif
      write(lun_ps,'(a)')cline_ps(2:len_trim(cline_ps))

      nxzone_ps=nxzon
      nyzone_ps=nyzon
      nzone_ps=nxzone_ps*nyzone_ps

      if (chopt.eq.'S'.or.chopt.eq.'s') then
        isamecanvas_ps=1
      else
        isamecanvas_ps=0
        isameframe_ps=0
      endif

      if (nzone_ps.gt.0) then
        if (ifirst.gt.nzone_ps.or.ifirst.le.0) then
          print*,'*** WARNING IN MSHPLT_ZONE: IFIRST > NXZONE*NYZONE OR IFIRST <= 0'
          print*,'*** WARNING IN MSHPLT_ZONE: IFRST SET TO ONE'
          ifirst_ps=1
        else
          ifirst_ps=ifirst
        endif
      endif

      wid=(xsizorig_ps-xmgl_ps-xmgr_ps-xwin_ps*(nxzon-1))/nxzon
      hig=(ysizorig_ps-ymgl_ps-ymgu_ps-ywin_ps*(nyzon-1))/nyzon

      ix=0
      iy=1
      do i=1,ifirst
        ix=ix+1
        if (ix.gt.nxzon) then
          ix=1
          iy=iy+1
        endif
      enddo

      xl=xleftorig_ps+xmgl_ps+(ix-1)*(wid+xwin_ps)
      xr=xl+wid
      yl=ybottomorig_ps+ymgl_ps+(nyzon-iy)*(hig+ywin_ps)
      yt=yl+hig
      call mshplt_set_pad(xl,xr,yl,yt)

      if (isamecanvas_ps.eq.0.and.abs(kzone_ps).ge.nzone_ps
     &    .and.itouched_ps.ne.0) then
        call mshplt_newpage
      endif

      kzone_ps=ifirst
      kzone_ps=min(-1,-abs(kzone_ps)) !to indicate call to mshplt_zone

      write(cline_ps,*)'% end of mshplt_zone'
      write(lun_ps,'(a)')cline_ps(2:len_trim(cline_ps))

      return
      end
