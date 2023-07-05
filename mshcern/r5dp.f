*CMZ :          28/08/2014  12.57.13  by  Michael Scheer
*-- Author :    Michael Scheer   28/08/2014

cmsh Generated with: cpp -E -DCERNLIB_DOUBLE -DCERNLIB_UNIX r5dp.F

# 1 "r5dp.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "r5dp.F"
*
* $Id: r5dp.F,v 1.1.1.1 1996/04/01 15:01:57 mclareni Exp $
*
* $Log: r5dp.F,v $
* Revision 1.1.1.1 1996/04/01 15:01:57 mclareni
* Mathlib gen
*
*
# 1 "/usr/include/gen/pilot.h" 1 3 4
# 10 "r5dp.F" 2

      FUNCTION C309R5(X,ETA,ZL,EPS,FCL,TPK1,ETANE0,LIMIT,ERR,NFP,
     1 FPMIN,FPMAX,LPR)
C
C *** Evaluate CF1 = F'(ZL,ETA,X)/F(ZL,ETA,X)  (COMPLEX)
C
      IMPLICIT COMPLEX*16(A-H,O-Z)
      LOGICAL LPR,ETANE0
      DOUBLE PRECISION EPS,ERR,FPMIN,FPMAX,ABSC,SMALL,PX





      ABSC(W)=ABS(DREAL(W))+ABS(DIMAG(W))

      FCL=1
      XI=1/X
      PK=ZL+1
      PX=PK+LIMIT
      EK=ETA/PK
      F=EK+PK*XI
      IF(ABSC(F) .LT. FPMIN) F=FPMIN
      D=0
      C=F
      SMALL=SQRT(FPMIN)
      RK2=1+EK*EK
C
C *** begin CF1 loop on PK = k = lambda + 1
C
   10 PK1=PK+1
      TPK1=PK+PK1
      IF(ETANE0) THEN
       EK=ETA/PK
       RK2=1+EK*EK
       TK=TPK1*(XI+EK/PK1)
      ELSE
       TK=TPK1*XI
      END IF
      C=TK-RK2/C
      D=TK-RK2*D
      IF(ABSC(C) .LT. FPMIN) C=FPMIN
      IF(ABSC(D) .LT. FPMIN) D=FPMIN
      D=1/D
      DF=D*C
      F=F*DF
      FCL=FCL*D*TPK1*XI
      IF(ABSC(FCL) .LT. SMALL) FCL=FCL/SMALL
      IF(ABSC(FCL) .GT. FPMAX) FCL=FCL*FPMIN
      PK=PK1
      IF(DREAL(PK) .LE. PX) THEN
       IF(ABSC(DF-1) .GE. EPS) GO TO 10
       NFP=PK-ZL-1
       ERR=EPS*SQRT(REAL(NFP))
       C309R5=F
      ELSE
       IF(LPR) WRITE (6,1000) LIMIT,ABS(X)
       ERR=2
      END IF
      RETURN
 1000 FORMAT(1X,'***** CERN C309 WCLBES ... CF1 (COMPLEX) HAS FAILED ',
     1'TO CONVERGE AFTER',I10,' ITERATIONS AS ABS(X) =',F15.0)
      END
