*CMZ :  2.04/00 31/10/2022  15.40.57  by  Michael Scheer
*CMZ :  2.02/01 28/05/2021  09.15.52  by  Michael Scheer
*CMZ :  2.02/00 31/10/2018  16.15.03  by  Michael Scheer
*CMZ : 00.00/16 20/07/2015  09.58.29  by  Michael Scheer
*-- Author :    Michael Scheer   20/07/2015
      subroutine util_solve_3x3(a,x,ifail)

      implicit none

      double precision a(3,3),x(3),det,dws,ws(3,3),xs(3)

      integer ifail,i

      call util_determinante_3(a,det)
      if (det.eq.0.0d0) then
        ifail=-1
        return
      endif

      xs=x

      do i=1,3
        ws=a
        ws(1:3,i)=xs
        call util_determinante_3(ws,dws)
        x(i)=dws/det
      enddo

      ifail=0

      return
      end
