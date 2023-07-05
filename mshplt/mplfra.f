*CMZ :          23/02/2022  10.34.24  by  Michael Scheer
*CMZ :  0.00/06 20/08/2014  12.45.42  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  11.23.35  by  Michael Scheer
*CMZ :  1.17/00 22/04/2014  15.58.15  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.22.38  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mplfra(xmin,xmax,ymin,ymax,chopt)

      implicit none

      real xmin,xmax,ymin,ymax
      integer i
      character*(*) chopt
      character(6) copt



      copt='LrBtc'

      do i=1,len_trim(chopt)
        if (chopt(i:i).eq.'S'.or.chopt(i:i).eq.'s') then
          copt(6:6)='S'
        endif
      enddo

      do i=1,len_trim(chopt)
        if (chopt(i:i).eq.'A'.or.chopt(i:i).eq.'a') then
c          copt(1:1)='l'
c          copt(3:3)='b'
        endif
      enddo

      do i=1,len_trim(chopt)
        if (chopt(i:i).eq.'B'.or.chopt(i:i).eq.'b') then
          copt(1:5)='xxxxx'
        endif
      enddo

      call mshplt_frame(xmin,xmax,ymin,ymax,'','',copt)

      return
      end
