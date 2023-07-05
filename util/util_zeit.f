*CMZ : 00.00/15 04/01/2013  12.19.21  by  Michael Scheer
*CMZ : 00.00/02 26/01/2004  09.37.25  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.25.58  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_ZEIT(LUN)

C To determine date and time and write it to logical unit LUN

      IMPLICIT NONE
      INTEGER LUN

      CHARACTER SPACER(50)
      character(10) dtday,dttime,dtzone
      integer idatetime(8)

      DATA SPACER/50*' '/

      CALL date_and_time(dtday,dttime,dtzone,idatetime)

      WRITE(LUN,*)
      WRITE(LUN,*)SPACER,dttime(1:2),':',dttime(3:4),':',dttime(5:6),' '
     &,dtday(7:8),'.',dtday(5:6),'.',dtday(3:4)
      WRITE(LUN,*)

      RETURN
      END
