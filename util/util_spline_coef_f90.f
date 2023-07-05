*CMZ : 00.00/07 07/05/2008  14.02.20  by  Michael Scheer
*CMZ :  2.47/07 14/04/2003  15.17.05  by  Michael Scheer
*CMZ :  2.20/09 03/04/2001  11.03.37  by  Michael Scheer
*CMZ :  2.15/00 28/04/2000  10.32.37  by  Michael Scheer
*CMZ : 00.00/04 29/04/94  17.55.48  by  Michael Scheer
*CMZ : 00.00/00 28/04/94  16.14.48  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_SPLINE_COEF_F90(X,Y,N,YP1,YPN,Y2)

C--- CALCULATES SPLINE COEFFICIENTS

C--   INPUT:

C-       N: NUMBER OF X,Y-VALUES
C-       X: ARRAY OF X-VALUES
C-       Y: ARRAY OF Y-VALUES
C-       YP1:  SECOND DERIVATIVE AT FIRST X-VALUE
C-       YPN:  SECOND DERIVATIVE AT LAST X-VALUE

C--   OUPUT:

C-       Y2:   SPLINE-COEFFICIENTS

C--   WORKINGSPACE: AA(N),BB(N),CC(N),C(N)


      IMPLICIT NONE

      INTEGER N,J
      DOUBLE PRECISION  X(N),Y(N),Y2(N)
      DOUBLE PRECISION YP1,YPN

      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE :: AA,BB,CC,C

      double precision xx(3),yy(3),a(3),yp(3),xopt,yopt
      INTEGER ifail

      ALLOCATE(AA(N))
      ALLOCATE(BB(N))
      ALLOCATE(CC(N))
      ALLOCATE(C(N))

      IF (N.LT.3) then
        if (abs(yp1).eq.9999.0d0) then
          y2(1)=0.0d0
        else
          y2(1)=yp1
        endif
        if (abs(ypn).eq.9999.0d0) then
          y2(n)=0.0d0
        else
          y2(n)=ypn
        endif
        GOTO 999
      endif

      if (abs(yp1).eq.9999.0d0) then
        xx=x(1:3)
        yy=y(1:3)
        call UTIL_PARABEL(xx,yy,A,YP,XOPT,yopt,IFAIL)
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
        call UTIL_PARABEL(xx,yy,A,YP,XOPT,yopt,IFAIL)
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
     &          -(Y(J  )-Y(J-1))/(X(J  )-X(J-1))
      ENDDO !J

      DO J=2,N-1

          BB(J)=BB(J)-AA(J)*CC(J-1)
           C(J)= C(J)-AA(J)* C(J-1)
C030414          AA(J)=AA(J)-AA(J)*BB(J-1)

          CC(J)=CC(J)/BB(J)
           C(J)= C(J)/BB(J)
          BB(J)=1.D0

      ENDDO !J

      DO J=N-1,2,-1
         Y2(J)=C(J)-CC(J)*Y2(J+1)
      ENDDO

999   CONTINUE

      DEALLOCATE(AA)
      DEALLOCATE(BB)
      DEALLOCATE(CC)
      DEALLOCATE(C)

      RETURN
      END
