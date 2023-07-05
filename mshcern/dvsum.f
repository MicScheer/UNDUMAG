*CMZ :          02/05/2017  14.54.26  by  Michael Scheer
*-- Author :

*KEEP,cmsh,T=F77.
!
!       Routine were taken from the CERNLIB
!       Changes by Michael Scheer are marked by "cmsh"
!
*KEND.

cmsh # 19 "dvsub.F" 2

# 1 "dvsum.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dvsum.F"
*
* $Id: dvsum.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: dvsum.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dvsum.F" 2
          DOUBLE PRECISION FUNCTION DVSUM(N,X,X2)
          DOUBLE PRECISION    X(*), X2(*), SUM
          SUM  =  0.D0
          IF(N .LE. 0)  GOTO 20

# 1 "dxj.inc" 1
*
* $Id: dxj.inc,v 1.1.1.1 1996/02/15 17:48:50 mclareni Exp $
*
* $Log: dxj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* dxj.inc
*

          JX  =  (LOCF(X2) - LOCF(X)) / 2
# 15 "dvsum.F" 2
          LXJ  =  1
          DO 10     J  =  1, N
             SUM  =  SUM + X(LXJ)
             LXJ  =  LXJ + JX
  10         CONTINUE
  20      DVSUM  =  SUM
          RETURN
          END
