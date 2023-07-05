*CMZ :  1.11/00 03/01/2017  16.11.46  by  Michael Scheer
*CMZ :  0.00/02 28/04/2016  11.55.58  by  Michael Scheer
*CMZ : 00.00/15 02/07/2012  12.26.49  by  Michael Scheer
*CMZ : 00.00/02 26/01/2004  09.27.26  by  Michael Scheer
*-- Author :    Michael Scheer   23/01/2004
      subroutine util_skip_comment_radia(lun,lunrad)

      implicit none

      integer lun,icom,lunrad,l
      character com
      character(6) c6
      character(2048) cline

      icom=0

1     read(lun,'(a)') c6
      com=c6(1:1)

      if (c6.eq.'{Radia') then
        write(lunrad,*)' '
        do while (.true.)
          read(lun,'(a)')cline
          if (cline(1:6).eq.'}Radia') then
            write(lunrad,*)' '
            exit
          endif
          write(lunrad,*)trim(cline)
        enddo
        goto 1
      else if (com.eq.'{') then
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
