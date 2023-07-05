*CMZ : 00.00/16 18/03/2014  17.02.27  by  Michael Scheer
*CMZ : 00.00/15 12/10/2013  12.22.24  by  Michael Scheer
*CMZ : 00.00/07 02/05/2008  13.10.35  by  Michael Scheer
*CMZ : 00.00/02 17/08/2004  09.47.26  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.25.29  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_SPLINE_INTEGRAL(X,Y,N,resultat
     &                                 ,COEF,WORK1,WORK2,WORK3,WORK4)

C---  CALCULATES INTERGRAL OF Y(X) VIA SPLINES

      IMPLICIT NONE

      INTEGER I,N
      REAL*8 X(N),Y(N),resultat
      REAL*8 COEF(N),WORK1(N),WORK2(N),WORK3(N),WORK4(N)

C---  SPLINE-COEFFICIENTS

      CALL UTIL_SPLINE_COEF(X,Y,N,9999.0d0,9999.0d0,COEF,WORK1,WORK2,WORK3,WORK4)

C--- INTEGRATION

      resultat=0.0D0
      DO I=1,N-1

      resultat=resultat
     &          +(X(I+1)-X(I))*0.5D0
     &          *(Y(I)+Y(I+1))
     &          -(X(I+1)-X(I))**3/24.D0
     &          *(COEF(I)+COEF(I+1))

      ENDDO

      RETURN
      END
