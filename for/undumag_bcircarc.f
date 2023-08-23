*CMZ :  2.02/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  1.25/02 20/03/2018  17.15.30  by  Michael Scheer
*CMZ :  1.25/01 16/03/2018  16.48.51  by  Michael Scheer
*CMZ :  1.25/00 16/03/2018  10.05.19  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_bcircarc(krace,xin,yin,zin,bxout,byout,bzout)

      use undumagf90m

      use commandlinef90m

      implicit none

      double precision xin,yin,zin,bxout,byout,bzout,wire7(7),bx,by,bz

      integer istat,krace,iw

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0
      istat=0

      do iw=ncfila+1,ncwires
        if (wire(2,iw).eq.0.0d0.or.
     &    abs(wire(3,iw)-wire(6,iw))+abs(wire(4,iw)-wire(7,iw))+
     &    abs(wire(5,iw)-wire(8,iw)).lt.1.0d-9) cycle
        wire7(1)=wire(2,iw)
        wire7(2:7)=wire(3:8,iw)/1000.0d0
        call undumag_bwireana(wire7,xin,yin,zin,bx,by,bz)
        if (by.ne.by) then
          write(lun6,*)iw,wire7
          write(lun6,*)"Ende"
          stop
        endif
        bxout=bxout+bx
        byout=byout+by
        bzout=bzout+bz
      enddo

      return
      end
