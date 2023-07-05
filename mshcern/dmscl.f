*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 1 "dmscl.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dmscl.F"
*
* $Id: dmscl.F,v 1.1.1.1 1996/02/15 17:48:58 mclareni Exp $
*
* $Log: dmscl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dmscl.F" 2
          SUBROUTINE          DMSCL(M,N,S,X,X12,X21,Z,Z12,Z21)
          DOUBLE PRECISION    S, X(*),X12(*),X21(*), Z(*),Z12(*),Z21(*)
          DOUBLE PRECISION    FUNCT, A
          FUNCT(A)  =  S*A
          IF(M .LE. 0  .OR. N .LE. 0)  RETURN

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
# 16 "dmscl.F" 2

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
# 17 "dmscl.F" 2

# 1 "mcpy.inc" 1
*
* $Id: mcpy.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mcpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mcpy.inc
*
          MM  =  M
          NN  =  N
          IF(MM .GT. NN)  THEN
             MN  =  NN
             NN  =  MM
             MM  =  MN
             IJ  =  JX
             JX  =  IX
             IX  =  IJ
             IJ  =  JZ
             JZ  =  IZ
             IZ  =  IJ
          ENDIF
          LXI1  =  1
          LZI1  =  1
          DO 12     I  =  1, MM
             LXIJ  =  LXI1
             LZIJ  =  LZI1
             DO 11     J  =  1, NN
                Z(LZIJ)  =  FUNCT( X(LXIJ) )
                LXIJ  =  LXIJ + JX
                LZIJ  =  LZIJ + JZ
  11         CONTINUE
             LXI1  =  LXI1 + IX
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
