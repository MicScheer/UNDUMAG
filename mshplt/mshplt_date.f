*CMZ :  1.01/00 24/09/2014  14.08.29  by  Michael Scheer
*CMZ :  0.01/03 22/09/2014  21.17.47  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 21/08/2014  16.21.19  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.13.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_date(chdate)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(*) chdate
      character(10) dtday,dttime,dtzone
      integer idatetime(8)

      integer lendate

      lendate=len_trim(chdate)
      if (lendate.eq.0) return

      if (chdate.eq.'today') then
        call date_and_time(dtday,dttime,dtzone,idatetime)
        write(cline_ps,*)dtday(1:4),'.',dtday(5:6),'.',dtday(7:8),'  ',
     &    dttime(1:2),':',dttime(3:4)
      else
        cline_ps=chdate(1:lendate)
      endif

      lendate=len_trim(cline_ps)

      call mshplt_text_raw(
     &  xrightorig_ps-chhe_ps*scaletxt_ps*lendate+offdatex_ps*scaletxt_ps,
     &  ytoporig_ps-offdatey_ps*scaletxt_ps,
     &  cline_ps(1:lendate)
     &  )

      return
      end
