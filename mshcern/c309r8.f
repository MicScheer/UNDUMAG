*CMZ :          28/08/2014  12.00.37  by  Michael Scheer
*-- Author :    Michael Scheer   28/08/2014
*
* $Id: r8dp.F,v 1.1.1.1 1996/04/01 15:01:57 mclareni Exp $
*
* $Log: r8dp.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:57  mclareni
* Mathlib gen
*
*

cmsh #include "gen/pilot.h"
#define CERNLIB_DOUBLE

#if defined(CERNLIB_DOUBLE)
      FUNCTION C309R8(Z,ACC)
      COMPLEX*16 C309R8,Z
      DOUBLE PRECISION ACC
      DOUBLE PRECISION X,Y,AX,AY,A

#if defined(CERNLIB_QF2C)
#include "defdr.inc"
#endif
      X=DREAL(Z)
      Y=DIMAG(Z)
      AX=ABS(X)
      AY=ABS(Y)
      A=5*ACC*(AX+AY)
      IF(AX .LT. A) X=0
      IF(AY .LT. A) Y=0
      C309R8=DCMPLX(X,Y)
      RETURN
      END
#endif
