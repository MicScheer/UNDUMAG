*CMZ :  1.24/01 16/10/2017  16.04.30  by  Michael Scheer
*CMZ :  1.23/03 19/09/2017  19.32.15  by  Michael Scheer
*CMZ :  1.23/02 18/09/2017  14.10.42  by  Michael Scheer
*CMZ :  1.15/11 19/04/2017  14.47.22  by  Michael Scheer
*CMZ :  1.15/02 01/04/2017  15.43.17  by  Michael Scheer
*CMZ :  1.15/01 28/03/2017  14.43.16  by  Michael Scheer
*CMZ :  1.10/01 18/11/2016  15.02.58  by  Michael Scheer
*CMZ :  1.02/01 09/09/2016  13.43.20  by  Michael Scheer
*CMZ :  0.00/13 16/08/2016  12.20.28  by  Michael Scheer
*CMZ :  0.00/09 04/07/2016  15.33.12  by  Michael Scheer
*CMZ :  0.00/01 26/04/2016  16.02.08  by  Michael Scheer
*CMZ :  1.17/14 13/04/2016  09.46.04  by  Michael Scheer
*CMZ :  1.17/13 07/04/2016  17.38.21  by  Michael Scheer
*CMZ :  1.17/12 06/04/2016  14.53.04  by  Michael Scheer
*CMZ :  1.17/09 04/04/2016  09.28.51  by  Michael Scheer
*CMZ :  1.17/07 04/04/2016  08.49.46  by  Michael Scheer
*-- Author :    Michael Scheer   03/04/2016
      subroutine undumag_fbfield(iplan,x,y,z,bxout,byout,bzout,ifail)

c This subroutine should ony be called from undumag_force for a rectangular
c force box

      use bpolyederf90m
      use undumagf90m

      implicit none

*KEEP,seqdebug.
      include 'seqdebug.cmn'
*KEND.

      double precision x,y,z,bxout,byout,bzout,bc(3)

      integer ifail,kino,iplan

      integer :: ical=0

      kino=kinside
      kinside=0

      call undumag_field(x,y,z,bxout,byout,bzout,ifail)

      if (kinside.gt.0) then
        bc=bpebc(4:6,kinside)
        if (iplan.eq.1.or.iplan.eq.3) then
          bxout=bxout+bc(1)
        else if (iplan.eq.2.or.iplan.eq.4) then
          byout=byout+bc(2)
        else if (iplan.eq.5.or.iplan.eq.6) then
          bzout=bzout+bc(3)
        endif
      endif

      kinside=kino

      return
      end
