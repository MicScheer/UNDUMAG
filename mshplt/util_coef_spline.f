*CMZ :  1.04/00 09/02/2025  12.49.56  by  Michael Scheer
*CMZ : 00.00/07 07/05/2008  14.02.20  by  Michael Scheer
*CMZ : 00.00/02 14/04/2004  14.25.24  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.27.48  by  Michael Scheer
*-- Author : Michael Scheer
      SUBROUTINE util_coef_spline(n,X,Y,YP1,YPN,YP,Y2,istatus)

C--- CALCULATES SPLINE COEFFICIENTS

C--   INPUT:

C-       N: NUMBER OF X,Y-VALUES
C-       X: ARRAY OF X-VALUES
C-       Y: ARRAY OF Y-VALUES
C-       YP1:  SECOND DERIVATIVE AT FIRST X-VALUE
C-       YPN:  SECOND DERIVATIVE AT LAST X-VALUE

C--   OUPUT:

C-       YP:   DERIVATIVES AT XA
C-       Y2:   SPLINE-COEFFICIENTS
C-  ISTATUS:   EXIT-CODE


      IMPLICIT NONE

      INTEGER N,J,I,I1,istatus

      REAL*8  X(N),Y(N),YP(N),Y2(N),AA(N),BB(N),CC(N),C(N)
      REAL*8 YP1,YPN

      double precision xx(3),yy(3),a(3),yp3(3),xopt,yopt
      INTEGER ifail

      istatus=0

      IF (N.LT.3) then
        istatus=-1
        RETURN
      endif

      if (abs(yp1).eq.9999.0d0) then
        xx=x(1:3)
        yy=y(1:3)
        call UTIL_PARABEL(xx,yy,A,YP3,XOPT,yopt,IFAIL)
        if (ifail.eq.0) then
          y2(1)=2.0d0*a(3)
        else
          y2(1)=0.0d0
        endif
      else
        Y2(1)=YP1
      endif

      if (abs(ypn).eq.9999.0d0) then
        xx=x(n-2:n)
        yy=y(n-2:n)
        call UTIL_PARABEL(xx,yy,A,YP3,XOPT,yopt,IFAIL)
        if (ifail.eq.0) then
          y2(n)=2.0d0*a(3)
        else
          y2(N)=0.0d0
        endif
      else
        Y2(N)=YPN
      endif

      C(1)=Y2(1)
      C(N)=y2(n)

      BB(1)=1.D0
      CC(1)=0.D0
      CC(N)=1.D0

      DO J=2,N-1
        AA(J)=(X(J  )-X(J-1))/6.D0
        BB(J)=(X(J+1)-X(J-1))/3.D0
        CC(J)=(X(J+1)-X(J  ))/6.D0
        C(J)=(Y(J+1)-Y(J  ))/(X(J+1)-X(J  ))
     &    -(Y(J  )-Y(J-1))/(X(J  )-X(J-1))
      ENDDO !J

      DO J=2,N-1

        BB(J)=BB(J)-AA(J)*CC(J-1)
        C(J)= C(J)-AA(J)* C(J-1)

        CC(J)=CC(J)/BB(J)
        C(J)= C(J)/BB(J)
        BB(J)=1.D0

      ENDDO !J

      DO J=N-1,2,-1
        Y2(J)=C(J)-CC(J)*Y2(J+1)
      ENDDO

      DO I=1,N-1
        I1=I+1
        YP(I)=(Y(I1)-Y(I))/(X(I1)-X(I))-
     &    (Y2(I1)+2.D0*Y2(I))/6.D0*(X(I1)-X(I))
      ENDDO

      I1=N
      I=N-1

      YP(N)=(Y(I1)-Y(I))/(X(I1)-X(I))+
     &  (2.D0*Y2(I1)+Y2(I))/6.D0*(X(I1)-X(I))

      RETURN
      END
