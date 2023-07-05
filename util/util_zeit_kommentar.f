*CMZ :  2.03/00 26/07/2022  07.55.50  by  Michael Scheer
*CMZ :  2.02/02 01/07/2022  17.30.28  by  Michael Scheer
*CMZ :  2.02/00 29/03/2021  09.26.44  by  Michael Scheer
*CMZ :  1.00/00 19/08/2016  18.24.11  by  Michael Scheer
*CMZ : 00.00/15 04/01/2013  12.22.07  by  Michael Scheer
*CMZ : 00.00/05 27/02/2007  16.32.04  by  Michael Scheer
*CMZ : 00.00/02 04/08/2006  14.56.41  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.25.58  by  Michael Scheer
*-- Author : Michael Scheer
      subroutine util_zeit_kommentar(lun,comment)

c to determine date and time and write it to logical unit lun

      implicit none

      integer lun,ilast

      character(*) comment

      character spacer(30)
      character(10) dtday,dttime,dtzone
      character(2048) :: cblank=''
      character(2048) cline

      integer idatetime(8),iyear,imonth,iday,ihour,iminute,isec
      integer iyearo,imontho,idayo,ihouro,iminuteo,iseco

      integer :: ical=0,linlen

      data spacer/30*' '/

      save

      call date_and_time(dtday,dttime,dtzone,idatetime)

      iyear=idatetime(1)
      imonth=idatetime(2)
      iday=idatetime(3)
      ihour=idatetime(5)
      iminute=idatetime(6)
      isec=idatetime(7)

      ilast=len_trim(comment)

      write(lun,*)
      if (ilast.gt.0) then
        write(cline,*)comment(1:ilast),spacer,dttime(1:2),':',dttime(3:4),':',
     &    dttime(5:6),' ',dtday(7:8),'.',dtday(5:6),'.',dtday(3:4)
      else
        write(cline,*)spacer,dttime(1:2),':',dttime(3:4),':',dttime(5:6),' '
     &    ,dtday(7:8),'.',dtday(5:6),'.',dtday(3:4)
      endif

      ilast=len_trim(cline)

      if (ilast.lt.64) then
        write(lun,'(a)') cblank(1:64-ilast) // cline(1:ilast)
      else
        write(lun,*) trim(cline)
      endif

      if (ical.gt.0) then
        if (iyearo.lt.iyear) then
          write(lun,*)'delta time:',
     &      iyear-iyearo,' years',
     &      imonth-imontho,' months',
     &      iday-idayo,' days',
     &      ihour-ihouro,' hours',
     &      iminute-iminuteo,' minutes',
     &      isec-iseco,' seconds'
        else if (imontho.lt.imonth) then
          write(lun,*)'delta time:',
     &      imonth-imontho,' months',
     &      iday-idayo,' days',
     &      ihour-ihouro,' hours',
     &      iminute-iminuteo,' minutes',
     &      isec-iseco,' seconds'
        else if (idayo.lt.iday) then
          write(lun,*)'delta time:',
     &      iday-idayo,' days',
     &      ihour-ihouro,' hours',
     &      iminute-iminuteo,' minutes',
     &      isec-iseco,' seconds'
        else if (ihouro.lt.ihour) then
          write(lun,*)'delta time:',
     &      ihour-ihouro,' hours',
     &      iminute-iminuteo,' minutes',
     &      isec-iseco,' seconds'
        else if (iminuteo.lt.iminute) then
          if (iseco.lt.isec) then
            write(lun,*)'delta time:',
     &        iminute-iminuteo,' minutes',
     &        isec-iseco,' seconds'
          else
            write(lun,*)'delta time:',
     &        iminute-iminuteo-1,' minutes',
     &        isec-iseco+60,' seconds'
          endif
        else if (iseco.lt.isec) then
          write(lun,*)'delta time:',
     &      isec-iseco,' seconds'
        endif
      endif

      write(lun,*)

      iyearo=iyear
      imontho=imonth
      idayo=iday
      ihouro=ihour
      iminuteo=iminute
      iseco=isec

      ical=1

      return
      end
