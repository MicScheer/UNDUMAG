*CMZ :  1.02/00 02/10/2014  16.21.50  by  Michael Scheer
*CMZ :  0.01/02 17/09/2014  13.06.35  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  13.52.14  by  Michael Scheer
*CMZ :  1.17/00 17/04/2014  17.28.58  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.50.46  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mplopt(choptn,n)

      implicit none

      integer n
c      character(4) chopt(*)
      character(*) choptn



      call mshplt_hplopt(choptn,n)

      return
      end
