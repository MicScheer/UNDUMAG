*CMZ :          28/08/2014  12.25.01  by  Michael Scheer
*-- Author :    Michael Scheer   28/08/2014
# 1 "cdigam64.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "cdigam64.F"
*
* $Id: cdigam64.F,v 1.1.1.1 1996/04/01 15:01:56 mclareni Exp $
*
* $Log: cdigam64.F,v $
* Revision 1.1.1.1 1996/04/01 15:01:56 mclareni
* Mathlib gen
*
*
# 1 "/usr/include/gen/pilot.h" 1 3 4
# 10 "cdigam64.F" 2



cmsh: Generated with: cpp -E -DCERNLIB_DOUBLE -DCERNLIB_UNIX cdigam64.F


      FUNCTION WDIGAM(Z)
# 1 "/usr/include/gen/imp64.inc" 1 3 4
*
* $Id: imp64.inc,v 1.1.1.1 1996/04/01 15:02:59 mclareni Exp $
*
* $Log: imp64.inc,v $
* Revision 1.1.1.1 1996/04/01 15:02:59 mclareni
* Mathlib gen
*
*
* imp64.inc
*







      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
# 18 "cdigam64.F" 2
# 1 "/usr/include/gen/defc64.inc" 1 3 4
*
* $Id: defc64.inc,v 1.1.1.1 1996/04/01 15:02:59 mclareni Exp $
*
* $Log: defc64.inc,v $
* Revision 1.1.1.1 1996/04/01 15:02:59 mclareni
* Mathlib gen
*
*
* defc64.inc
*







      COMPLEX*16
# 19 "cdigam64.F" 2
     + WDIGAM

# 1 "/usr/include/gen/defc64.inc" 1 3 4
*
* $Id: defc64.inc,v 1.1.1.1 1996/04/01 15:02:59 mclareni Exp $
*
* $Log: defc64.inc,v $
* Revision 1.1.1.1 1996/04/01 15:02:59 mclareni
* Mathlib gen
*
*
* defc64.inc
*







      COMPLEX*16
# 22 "cdigam64.F" 2
     + Z,U,V,H,R,P
      CHARACTER NAME*(*)
      CHARACTER*80 ERRTXT




      PARAMETER (NAME = 'CDIGAM/WDIGAM')

      DIMENSION C(6)

      PARAMETER (Z1 = 1, HF = Z1/2)
      PARAMETER (PI = 3.14159 26535 89793 24D0)

# 1 "/usr/include/gen/gcmpfun.inc" 1 3 4
*
* $Id: gcmpfun.inc,v 1.1.1.1 1996/04/01 15:02:59 mclareni Exp $
*
* $Log: gcmpfun.inc,v $
* Revision 1.1.1.1 1996/04/01 15:02:59 mclareni
* Mathlib gen
*
*
*
* gcmpfun.inc
*

# 1 "/usr/include/gen/def64.inc" 1 3 4
*
* $Id: def64.inc,v 1.1.1.1 1996/04/01 15:02:59 mclareni Exp $
*
* $Log: def64.inc,v $
* Revision 1.1.1.1 1996/04/01 15:02:59 mclareni
* Mathlib gen
*
*
*
* def64.inc
*







      DOUBLE PRECISION
# 14 "/usr/include/gen/gcmpfun.inc" 2 3 4
     + GREAL,GIMAG,XARG,YARG
# 1 "/usr/include/gen/defc64.inc" 1 3 4
*
* $Id: defc64.inc,v 1.1.1.1 1996/04/01 15:02:59 mclareni Exp $
*
* $Log: defc64.inc,v $
* Revision 1.1.1.1 1996/04/01 15:02:59 mclareni
* Mathlib gen
*
*
* defc64.inc
*







      COMPLEX*16
# 16 "/usr/include/gen/gcmpfun.inc" 2 3 4
     + ZARG,GCONJG,GCMPLX
      GREAL( ZARG)=DREAL( ZARG)
      GIMAG( ZARG)=DIMAG( ZARG)
      GCONJG(ZARG)=DCONJG(ZARG)
      GCMPLX(XARG,YARG)=DCMPLX(XARG,YARG)
# 37 "cdigam64.F" 2
CSEQ,GCMPLX.

      DATA C(1) / 8.33333 33333 33333 33D-2/
      DATA C(2) /-8.33333 33333 33333 33D-3/
      DATA C(3) / 3.96825 39682 53968 25D-3/
      DATA C(4) /-4.16666 66666 66666 67D-3/
      DATA C(5) / 7.57575 75757 57575 76D-3/
      DATA C(6) /-2.10927 96092 79609 28D-2/

      U=Z
      X=U
      A=ABS(X)
      IF(GIMAG(U) .EQ. 0 .AND. -A .EQ. INT(X)) THEN
       H=0
       WRITE(ERRTXT,101) X
       CALL MTLPRT(NAME,'C307.1',ERRTXT)
      ELSE
       IF(X .LT. 0) U=-U
       V=U
       H=0
       IF(A .LT. 15) THEN
        H=1/V
        DO 1 I = 1,14-INT(A)
        V=V+1
    1 H=H+1/V
        V=V+1
       END IF
       R=1/V**2
       P=R*C(1)
       DO 2 I = 6,1,-1
    2 P=R*(C(I)+P)
       H=LOG(V)-HF/V-P-H
       IF(X .LT. 0) THEN
        V=PI*U
        X=V
        A=SIN(X)
        X=COS(X)
        Y=TANH(GIMAG(V))
        H=H+1/U+PI*GCMPLX(X,-A*Y)/GCMPLX(A,X*Y)
       END IF
      ENDIF

      WDIGAM=H




      RETURN
  101 FORMAT(1X,'ARGUMENT EQUALS NON-POSITIVE INTEGER = ',1P,E15.1)
      END
