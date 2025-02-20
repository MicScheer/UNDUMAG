*CMZ :  1.04/00 11/02/2025  21.09.26  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  13.42.26  by  Michael Scheer
*CMZ :  0.01/02 04/09/2014  12.59.12  by  Michael Scheer
*CMZ : 00.00/15 12/10/2013  12.19.05  by  Michael Scheer
*CMZ : 00.00/11 07/06/2011  14.38.25  by  Michael Scheer
*CMZ : 00.00/07 07/06/2011  13.46.51  by  Michael Scheer
*CMZ : 00.00/06 06/07/2007  16.55.15  by  Michael Scheer
*CMZ : 00.00/02 25/08/2006  15.16.22  by  Michael Scheer
*CMZ : 00.00/01 23/02/96  14.56.50  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.27.54  by  Michael Scheer
*-- Author : Michael Scheer
      SUBROUTINE util_spline_real4(N,X,Y,nspline,xspline,yspline)

      IMPLICIT NONE

      real*4 X(n),Y(n),xspline(nspline),yspline(nspline)
      real*8 Xd(n),Yd(n),sd(n),x2d(n),y2d(n),ssd,dsd,xsd,ysd

      integer n,nspline,i

      xd=x
      yd=y

      do i=1,n
        sd(i)=i
      enddo

      call util_spline_interpolation_f90(n,sd,xd,sd(1),xd(1),x2d,-1) !init spline
      call util_spline_interpolation_f90(n,sd,yd,sd(1),yd(1),y2d,-1) !init spline

      dsd=dble(n-1)/dble(nspline-1)
      ssd=sd(1)

      do i=1,nspline
        call util_spline_interpolation_f90(n,sd,xd,ssd,xsd,x2d,0)
        xspline(i)=sngl(xsd)
        call util_spline_interpolation_f90(n,sd,yd,ssd,ysd,y2d,0)
        yspline(i)=sngl(ysd)
        ssd=min(ssd+dsd,sd(n))
      enddo

      end
