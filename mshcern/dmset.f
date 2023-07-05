*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 18 "dmscl.F" 2

# 1 "dmset.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dmset.F"
*
* $Id: dmset.F,v 1.1.1.1 1996/02/15 17:48:58 mclareni Exp $
*
* $Log: dmset.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dmset.F" 2
          SUBROUTINE          DMSET(M,N,S,Z,Z12,Z21)
          DOUBLE PRECISION    S, Z(*), Z12(*), Z21(*)
          IF(M .LE. 0  .OR. N .LE. 0)  RETURN

# 1 "dzij.inc" 1
*
* $Id: dzij.inc,v 1.1.1.1 1996/02/15 17:48:55 mclareni Exp $
*
* $Log: dzij.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* dzij.inc
*

          IZ  =  (LOCF(Z21) - LOCF(Z)) / 2
          JZ  =  (LOCF(Z12) - LOCF(Z)) / 2
# 14 "dmset.F" 2

# 1 "mset.inc" 1
*
* $Id: mset.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mset.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mset.inc
*
          MM  =  M
          NN  =  N
          IF(MM .GT. NN)  THEN
             MN  =  NN
             NN  =  MM
             MM  =  MN
             IJ  =  JZ
             JZ  =  IZ
             IZ  =  IJ
          ENDIF
          LZI1  =  1
          DO 12     I  =  1, MM
             LZIJ  =  LZI1
             DO 11  J  =  1, NN
                Z(LZIJ)  =  S
                LZIJ     =  LZIJ + JZ
  11            CONTINUE
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
