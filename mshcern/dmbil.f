*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 19 "dmadd.F" 2

# 1 "dmbil.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dmbil.F"
*
* $Id: dmbil.F,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: dmbil.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dmbil.F" 2
          DOUBLE PRECISION FUNCTION DMBIL(N,X,X2,Y,Y12,Y21,Z,Z2)
          DOUBLE PRECISION X(*),X2(*),Y(*),Y12(*),Y21(*),Z(*),Z2(*)
          DOUBLE PRECISION A, B, SUM, ZERO, F, G, SXYZ, SYZ
          F(A,B,SUM)  =  A*B + SUM
          G(A,B,SUM)  =  A*B + SUM
          DATA      ZERO      /  0.D0  /
          SXYZ  =  ZERO
          IF(N .LE. 0)  GOTO 20

# 1 "dxi.inc" 1
*
* $Id: dxi.inc,v 1.1.1.1 1996/02/15 17:48:54 mclareni Exp $
*
* $Log: dxi.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:54  mclareni
* Kernlib
*
*
*
* dxi.inc
*

          IX  =  (LOCF(X2)  - LOCF(X)) / 2
# 19 "dmbil.F" 2

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
# 20 "dmbil.F" 2

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
# 21 "dmbil.F" 2

# 1 "mbil.inc" 1
*
* $Id: mbil.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mbil.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mbil.inc
*
          LXI  =  1
          LYI  =  1
          DO 12     I  =  1, N
             SYZ   =  ZERO
             LYIJ  =  LYI
             LZJ   =  1
             DO 11  J  =  1, N
                SYZ   =  F(Y(LYIJ),Z(LZJ),SYZ)
                LYIJ  =  LYIJ + JY
                LZJ   =  LZJ + JZ
  11            CONTINUE
             SXYZ  =  G(SYZ,X(LXI),SXYZ)
             LXI   =  LXI + IX
             LYI   =  LYI + IY
  12         CONTINUE
# 22 "dmbil.F" 2
  20      DMBIL  =  SXYZ
          RETURN
          END
