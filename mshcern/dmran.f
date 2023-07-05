*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 20 "dmmpy.F" 2

# 1 "dmran.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dmran.F"
*
* $Id: dmran.F,v 1.1.1.1 1996/02/15 17:48:58 mclareni Exp $
*
* $Log: dmran.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dmran.F" 2
          SUBROUTINE          DMRAN(M,N,A,B,Z,Z12,Z21)
          DOUBLE PRECISION    A, B, Z(*), Z12(*), Z21(*), C
          DOUBLE PRECISION    DRANF
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
# 15 "dmran.F" 2
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
          C     =  B - A
          LZI1  =  1
          DO 12     I  =  1, MM
             LZIJ  =  LZI1
             DO 11  J  =  1, NN
                Z(LZIJ)  =  C * DRANF() + A
                LZIJ     =  LZIJ + JZ
  11            CONTINUE
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
