*CMZ :          02/05/2017  14.55.22  by  Michael Scheer
*-- Author :

*KEEP,cmsh,T=F77.
!
!       Routine were taken from the CERNLIB
!       Changes by Michael Scheer are marked by "cmsh"
!
*KEND.

cmsh # 19 "dvadd.F" 2

cmsh # 1 "dvcpy.F"
# 1 "<built-in>"
# 1 "<command-line>"
cmsh # 1 "dvcpy.F"
*
* $Id: dvcpy.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: dvcpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

cmsh # 10 "dvcpy.F" 2
          SUBROUTINE          DVCPY(N,X,X2,Z,Z2)
          DOUBLE PRECISION    X(*), X2(*), Z(*), Z2(*), FUNCT, A
          FUNCT(A)  =  A
          IF(N .LE. 0)  RETURN

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
cmsh # 15 "dvcpy.F" 2

# 1 "dzj.inc" 1
*
* $Id: dzj.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: dzj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* dzj.inc
*

          JZ  =  (LOCF(Z2) - LOCF(Z)) / 2
cmsh # 16 "dvcpy.F" 2

# 1 "vcpy.inc" 1
*
* $Id: vcpy.inc,v 1.1.1.1 1996/02/15 17:48:50 mclareni Exp $
*
* $Log: vcpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* vcpy.inc
*
          LXJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             Z(LZJ)  =  FUNCT( X(LXJ) )
             LXJ     =  LXJ + JX
             LZJ     =  LZJ + JZ
  10         CONTINUE
          RETURN
          END
