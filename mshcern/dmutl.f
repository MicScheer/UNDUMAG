*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 18 "dmsub.F" 2

# 1 "dmutl.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dmutl.F"
*
* $Id: dmutl.F,v 1.1.1.1 1996/02/15 17:48:59 mclareni Exp $
*
* $Log: dmutl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dmutl.F" 2
          SUBROUTINE          DMUTL(N,X,X12,X21)
          DOUBLE PRECISION    X(*), X12(*), X21(*)
          IF(N .LE. 1)  RETURN

# 1 "dxij.inc" 1
*
* $Id: dxij.inc,v 1.1.1.1 1996/02/15 17:48:55 mclareni Exp $
*
* $Log: dxij.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* dxij.inc
*

          IX  =  (LOCF(X21) - LOCF(X)) / 2
          JX  =  (LOCF(X12) - LOCF(X)) / 2
# 14 "dmutl.F" 2

# 1 "mutl.inc" 1
*
* $Id: mutl.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mutl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mutl.inc
*
          LXII  =  1
          DO 12     IP1  =  2, N
             LXIJ  =  LXII
             LXJI  =  LXII
             DO 11  J  =  IP1, N
                LXIJ  =  LXIJ + JX
                LXJI  =  LXJI + IX
                X(LXJI)  =  X(LXIJ)
  11            CONTINUE
             LXII  =  LXII + IX + JX
  12         CONTINUE
          RETURN
          END
