*CMZ :  2.02/01 10/10/2021  19.06.12  by  Michael Scheer
*CMZ :  0.00/02 28/04/2016  11.55.58  by  Michael Scheer
*CMZ : 00.00/16 19/03/2014  12.14.18  by  Michael Scheer
*CMZ : 00.00/15 03/09/2012  09.26.58  by  Michael Scheer
*CMZ : 00.00/07 05/03/2008  15.43.44  by  Michael Scheer
*CMZ : 00.00/02 14/08/2006  13.22.55  by  Michael Scheer
*-- Author :    Michael Scheer   23/01/2004
      subroutine util_skip_comment_empty_end(lun,ieof)

      implicit none

      integer lun,ieof,icom
      character(2048) cline

      ieof=0
      icom=0

1     read(lun,'(a)',end=99) cline

      if (cline(1:1).eq.'{') then
        icom=1
      endif

      if (cline(1:1).eq.'}') then
        icom=0
        cline(1:1)='*'
      endif

      if (icom.eq.1) goto 1

      if (cline(1:1).ne.'!'.and.cline(1:1).ne.'*'.and.cline(1:1).ne.'#'
     &    .and.cline(1:1).ne.'%'.and.cline(1:1).ne.'@'.and.cline.ne.'') then
        backspace(lun)
      else
        goto 1
      endif

      return

99    ieof=1

      return
      end
