*CMZ :          07/08/2018  14.43.55  by  Michael Scheer
*CMZ :  0.00/04 13/08/2014  16.44.46  by  Michael Scheer
*CMZ :  1.17/00 26/05/2014  09.35.25  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.36.34  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mplzon(ix,iy,iwi,ckic)

      implicit none
      integer ix,iy,iwi
      character(*) ckic


      call mshplt_zone(ix,iy,iwi,ckic)
      return
      end
