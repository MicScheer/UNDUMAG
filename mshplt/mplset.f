*CMZ :  0.00/06 19/08/2014  13.33.59  by  Michael Scheer
*CMZ :  1.17/00 17/04/2014  17.15.31  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.51.04  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mplset(case,arg)

      implicit none

      real arg
      character(4) case



      call mshplt_hplset(case,arg)

      return
      end
