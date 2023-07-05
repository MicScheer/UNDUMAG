*CMZ :  0.00/02 28/04/2016  11.55.58  by  Michael Scheer
*CMZ : 00.00/15 02/07/2012  12.26.49  by  Michael Scheer
*CMZ : 00.00/02 26/01/2004  09.27.26  by  Michael Scheer
*-- Author :    Michael Scheer   23/01/2004
      subroutine util_skip_comment(lun)

      implicit none

      integer lun,icom
      character com

      icom=0

1     read(lun,'(a)') com

      if (com.eq.'{') then
        icom=1
      endif

      if (com.eq.'}') then
        icom=0
        com='*'
      endif

      if (icom.eq.1) goto 1

      if (com.ne.'!'.and.com.ne.'*'.and.com.ne.'#'
     &    .and.com.ne.'%'.and.com.ne.'@') then
        backspace(lun)
      else
        goto 1
      endif

      return
      end
