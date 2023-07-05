*CMZ :  2.02/01 28/05/2021  09.19.32  by  Michael Scheer
*CMZ : 00.00/16 19/03/2014  12.16.04  by  Michael Scheer
*CMZ : 00.00/15 12/10/2013  12.11.47  by  Michael Scheer
*CMZ : 00.00/11 11/02/2011  15.34.09  by  Michael Scheer
*-- Author :    Michael Scheer   11/02/2011
      SUBROUTINE util_spline_running_integral(X,Y,N,resultat
     &                                 ,COEF,WORK1,WORK2,WORK3,WORK4)

C---  CALCULATES RUNNING INTERGRAL OF Y(X) VIA SPLINES

      IMPLICIT NONE

      INTEGER I,N
      double precision X(N),Y(N),resultat(n)
      double precision COEF(N),WORK1(N),WORK2(N),WORK3(N),WORK4(N)

C---  SPLINE-COEFFICIENTS

      CALL UTIL_SPLINE_COEF(X,Y,N,9999.0d0,9999.0d0,COEF,
     &  WORK1,WORK2,WORK3,WORK4)

C--- INTEGRATION

      resultat(1)=0.0D0
      DO I=1,N-1

        resultat(i+1)=resultat(i)
     &          +(X(I+1)-X(I))*0.5D0
     &          *(Y(I)+Y(I+1))
     &          -(X(I+1)-X(I))**3/24.D0
     &          *(COEF(I)+COEF(I+1))

      ENDDO

      RETURN
      END
