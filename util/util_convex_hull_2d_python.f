*CMZ :          09/07/2025  08.17.36  by  Michael Scheer
*CMZ : 00.00/21 13/03/2017  11.18.51  by  Michael Scheer
*CMZ : 00.00/20 20/02/2017  22.03.44  by  Michael Scheer
*CMZ : 00.00/19 12/04/2016  09.18.40  by  Michael Scheer
*-- Author : Michael Scheer
      subroutine util_convex_hull_2d_python(n,x,y,nh,ihull,ifail)

      implicit none

      integer lun

      double precision x(n),y(n),z(n),xx,yy

      integer n,ifail,k,nh,nedge,ihull(*)

      open(newunit=lun,file='hull2d.in')
      do k=1,n
        write(lun,*)x(k),y(k)
      enddo
      close(lun)

      call execute_command_line('python3 ../python/hull2d_main.py',.true.,ifail)

      if (ifail.ne.0) then
        return
      endif

      open(newunit=lun,file='hull2d.out')

      read(lun,*) nh
      read(lun,*) nedge

      do k=1,nh
        read(lun,*) ihull(k)
        ihull(k)=ihull(k)+1
      enddo

      nh=nh+1
      ihull(nh)=ihull(1)

      close(lun)

      return
      end
