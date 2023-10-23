*CMZ :  2.05/01 06/10/2023  11.46.25  by  Michael Scheer
*CMZ : 00.00/16 20/07/2015  09.58.29  by  Michael Scheer
*-- Author :    Michael Scheer   20/07/2015
      subroutine util_solve_3x3(a,x,ifail)

      implicit none

      real*8 a(3,3),x(3),det,dws,ws(3,3),xs(3)

      integer ifail,i

      call util_determinante_3(a,det)
      if (det.eq.0.0d0) then
        ifail=-1
        return
      endif

      xs=x

      do i=1,3
        ws(1:3,1:3)=a(1:3,1:3)
        ws(1:3,i)=xs
        call util_determinante_3(ws,dws)
        x(i)=dws/det
      enddo

      ifail=0

      return
      end
