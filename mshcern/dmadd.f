*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 1 "dmadd.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dmadd.F"
*
* $Id: dmadd.F,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: dmadd.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dmadd.F" 2
          SUBROUTINE          DMADD(M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          DOUBLE PRECISION    X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          DOUBLE PRECISION    Z(*), Z12(*), Z21(*), ADD,  A,      B
          ADD(A,B)  =  A+B
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN

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
# 16 "dmadd.F" 2

# 1 "dyij.inc" 1
*
* $Id: dyij.inc,v 1.1.1.1 1996/02/15 17:48:55 mclareni Exp $
*
* $Log: dyij.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* dyij.inc
*

          IY  =  (LOCF(Y21) - LOCF(Y)) / 2
          JY  =  (LOCF(Y12) - LOCF(Y)) / 2
# 17 "dmadd.F" 2

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
# 18 "dmadd.F" 2

# 1 "madd.inc" 1
*
* $Id: madd.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: madd.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* madd.inc
*
          MM  =  M
          NN  =  N
          IF(MM .GT. NN) THEN
             MN  =  NN
             NN  =  MM
             MM  =  MN
             IJ  =  JX
             JX  =  IX
             IX  =  IJ
             IJ  =  JY
             JY  =  IY
             IY  =  IJ
             IJ  =  JZ
             JZ  =  IZ
             IZ  =  IJ
          ENDIF
          LXI1  =  1
          LYI1  =  1
          LZI1  =  1
          DO 12     I  =  1, MM
             LXIJ  =  LXI1
             LYIJ  =  LYI1
             LZIJ  =  LZI1
             DO 11  J  =  1, NN
                Z(LZIJ)  =  ADD( X(LXIJ),Y(LYIJ) )
                LXIJ     =  LXIJ + JX
                LYIJ     =  LYIJ + JY
                LZIJ     =  LZIJ + JZ
  11            CONTINUE
             LXI1  =  LXI1 + IX
             LYI1  =  LYI1 + IY
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
