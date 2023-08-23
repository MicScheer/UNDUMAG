*CMZ :  1.25/03 22/08/2023  09.03.52  by  Michael Scheer
*-- Author :    Michael Scheer   26/03/2018
      subroutine undumag_bcoilsinf(yin,zin,byint,bzint,istat)

      use undumagf90m

      implicit none

      double precision yin,zin,byint,bzint,byi,bzi,wire7(7)

      integer istat,iw

      istat=0

      byint=0.0d0
      bzint=0.0d0

      do iw=1,ncwires
        wire7(1)=wire(2,iw)
        wire7(2:7)=wire(3:8,iw)/1000.0d0
        call undumag_bwireinf(wire7,yin,zin,byi,bzi,istat)
        byint=byint+byi
        bzint=bzint+bzi
      enddo

      if (abs(byint).lt.1.0d-15) byint=0.0d0
      if (abs(bzint).lt.1.0d-15) bzint=0.0d0

      return
      end
