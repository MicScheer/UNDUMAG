*CMZ :  2.03/00 11/08/2022  07.49.32  by  Michael Scheer
*CMZ :  2.02/02 01/07/2022  17.30.28  by  Michael Scheer
*CMZ :  2.02/00 29/03/2021  09.26.44  by  Michael Scheer
*CMZ :  1.00/00 19/08/2016  18.24.11  by  Michael Scheer
*CMZ : 00.00/15 04/01/2013  12.22.07  by  Michael Scheer
*CMZ : 00.00/05 27/02/2007  16.32.04  by  Michael Scheer
*CMZ : 00.00/02 04/08/2006  14.56.41  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.25.58  by  Michael Scheer
*-- Author : Michael Scheer
      subroutine util_time_and_date(chtime)

      implicit none

      integer idatetime(8)
      character(*) chtime
      character(10) dtday,dttime,dtzone

      call date_and_time(dtday,dttime,dtzone,idatetime)

      chtime=dttime(1:2) // ':' // dttime(3:4) // ':' //
     &  dttime(5:6) // ' ' // dtday(7:8) // '.' // dtday(5:6) // '.'//
     &  dtday(3:4)

      return
      end
