*CMZ :          02/05/2017  14.51.45  by  Michael Scheer
*-- Author :

*KEEP,cmsh,T=F77.
!
!       Routine were taken from the CERNLIB
!       Changes by Michael Scheer are marked by "cmsh"
!
*KEND.

cmsh# 19 "dvscs.F" 2

cmsh # 1 "dvset.F"
# 1 "<built-in>"
# 1 "<command-line>"
cmsh # 1 "dvset.F"
*
* $Id: dvset.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: dvset.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

cmsh # 10 "dvset.F" 2
          SUBROUTINE          DVSET(N,S,Z,Z2)
          DOUBLE PRECISION    S, Z(*), Z2(*)
          IF(N .LE. 0)  RETURN

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
cmsh # 14 "dvset.F" 2

# 1 "vset.inc" 1
*
* $Id: vset.inc,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: vset.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vset.inc
*
          LZJ  =  1
          DO 10     J  =  1, N
             Z(LZJ)  =  S
             LZJ     =  LZJ + JZ
  10         CONTINUE
          RETURN
          END
