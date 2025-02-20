*CMZ :  1.04/00 31/10/2022  17.05.45  by  Michael Scheer
*CMZ : 00.00/16 19/03/2014  12.30.26  by  Michael Scheer
*CMZ : 00.00/15 03/09/2012  09.27.13  by  Michael Scheer
*CMZ : 00.00/07 22/03/2010  15.28.00  by  Michael Scheer
*CMZ : 00.00/02 26/03/97  10.23.11  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.27.40  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_PARABEL(Xin,Yin,A,YP,XOPT,yopt,IFAIL)

C--- CALCULATES A(1),A(2),A(3), THE DERIVATIVES YP(X(1)),YP(X(2)),YP(X(3)),
C    AND THE EXTREMUM (XOPT,A(XOPT)) OF PARABOLA Y=A1+A2*X+A3*X**2
C    FROM COORDINATES OF THE THREE POINTS (X(1),Y(1)),(X(2),Y(2)),(X(3),Y(3))
C

      IMPLICIT NONE

      INTEGER IFAIL

      REAL*8 A(3),X(3),Y(3),DXM,DXP,x0,a1,a2,dxm2,dxp2,dxmax,dymax,
     &  xin(3),yin(3),DET,YP(3),XOPT,yopt,a22,fm,fp,f0

      IFAIL=0

c calculate f=a0+a1*(x-x0)+a2*(x-x0)**2
c  = a0 + a1*x - a0*x0 + a2*x**2 - 2*a2*x*x0 + a2*x0**2
c  = a0 + (a2*x0 -a0)*x0 + (a1 - 2*a2*x0 )*x+ a2*x**2

c change system: (x0,s0)->(0,0), i.e.
c calculate f=a1*dx+a2*dx**2
c  df/dx=a1+2*a2*dx_max =! 0, dx_max=-a1/2/a2

      x=xin
      y=yin

      call util_sort_func(3,x,y)

c      if (x(1).le.x(2).and.x(1).le.x(3)) then
c      endif

      x0=x(2)
      f0=y(2)

      fm=y(1)-f0
      fp=y(3)-f0

      dxm=x(1)-x0
      dxp=x(3)-x0

c fm=a1*dxm+a2*dxm**2
c fp=a1*dxp+a2*dxp**2

c (dxm dxm2) (a1) = (y(1))
c (dxp dxp2) (a2) = (y(3))

      dxm2=dxm*dxm
      dxp2=dxp*dxp

      det=dxm*dxp2-dxp*dxm2

      if (det.ne.0.0d0) then
        a1=(fm*dxp2-fp*dxm2)/det
        a2=(fp*dxm-fm*dxp)/det
      else
        ifail=1
        return
      endif

      if (a2.ne.0.0d0) then
        dxmax=-a1/(2.0d0*a2)
        dymax=(a1+a2*dxmax)*dxmax
        xopt=x0+dxmax
        yopt=f0+dymax
      endif

c calculate f=f0+a1*dx+a2*dx**2
c = a1*x - a1*x0 + a2*x**2 + a2*x0**2 - 2*a2*x*x0
c  f = f0 + (a2*x0 -a1)*x0 + (a1 - 2*a2*x0 )*x+ a2*x**2

      a22=2.0d0*a2

      a(1)=f0 + (a2*x0 -a1)*x0
      a(2)=a1 - a22*x0
      a(3)=a2

c calculate yp=a1+2*a2*dx

      yp(1)=a1+a22*dxm
      yp(2)=a1
      yp(3)=a1+a22*dxp

      RETURN
      END
