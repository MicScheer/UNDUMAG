*CMZ :  1.11/00 04/01/2017  15.32.49  by  Michael Scheer
*CMZ : 00.00/16 18/12/2014  11.39.02  by  Michael Scheer
*CMZ : 00.00/15 12/10/2013  12.22.24  by  Michael Scheer
*CMZ : 00.00/07 02/05/2008  13.10.35  by  Michael Scheer
*CMZ : 00.00/02 17/08/2004  09.47.26  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.25.29  by  Michael Scheer
*-- Author :
      SUBROUTINE util_spline_or_simpson_integral(X,Y,N,resultat
     &                                 ,COEF,WORK1,WORK2,WORK3,WORK4)

C---  CALCULATES INTERGRAL OF Y(X) VIA SPLINES

      IMPLICIT NONE

      REAL*8 X(N),Y(N),resultat
      REAL*8 COEF(N),WORK1(N),WORK2(N),WORK3(N),WORK4(N)

      INTEGER I,N,ical,isimpson,lun,istat

      data ical/0/,isimpson/0/

      save

      if (ical.eq.0) then
        lun=99
        call util_get_free_lun(lun)
        open(unit=lun,file=".util_spline_or_simpson_integral.dat",
     &    status='old',err=9)
        read(99,*)isimpson
        close(lun)
        ical=1
      endif

C---  SPLINE-COEFFICIENTS

9     continue

      if (isimpson.eq.0) then
        CALL UTIL_SPLINE_COEF(
     &    X,Y,N,9999.0d0,9999.0d0,COEF,WORK1,WORK2,WORK3,WORK4)

C--- INTEGRATION

      resultat=0.0D0
      DO I=1,N-1

      resultat=resultat
     &          +(X(I+1)-X(I))*0.5D0
     &          *(Y(I)+Y(I+1))
     &          -(X(I+1)-X(I))**3/24.D0
     &          *(COEF(I)+COEF(I+1))

      ENDDO

      else if (isimpson.eq.1) then
        call util_simpson_integral(n,x,y,resultat)
      else
        call util_higher_simpson_equidist_integral(n,x,y,resultat,istat)
      endif

      RETURN
      END
