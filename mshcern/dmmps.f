*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 20 "dmmpa.F" 2

# 1 "dmmps.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dmmps.F"
*
* $Id: dmmps.F,v 1.1.1.1 1996/02/15 17:48:57 mclareni Exp $
*
* $Log: dmmps.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:57  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dmmps.F" 2
          SUBROUTINE          DMMPS(M,N,X,X12,X21,Y,Y2,Z,Z2)
          DOUBLE PRECISION    X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
          DOUBLE PRECISION    A, B, SUM, F, SIGNF
          F(A,B,SUM)  =  A*B + SUM
          SIGNF(A)    =  -A
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
# 17 "dmmps.F" 2

# 1 "dyj.inc" 1
*
* $Id: dyj.inc,v 1.1.1.1 1996/02/15 17:48:55 mclareni Exp $
*
* $Log: dyj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* dyj.inc
*

          JY  =  (LOCF(Y2) - LOCF(Y)) / 2
# 18 "dmmps.F" 2

# 1 "dzi.inc" 1
*
* $Id: dzi.inc,v 1.1.1.1 1996/02/15 17:48:55 mclareni Exp $
*
* $Log: dzi.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* dzi.inc
*

          IZ  =  (LOCF(Z2)  - LOCF(Z)) / 2
# 19 "dmmps.F" 2

# 1 "mmpa.inc" 1
*
* $Id: mmpa.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mmpa.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mmpa.inc
*
          LXI1  =  1
          LZI   =  1
          DO 12     I  =  1, M
             LXIJ  =  LXI1
             LYJ   =  1
             SUM   =  SIGNF( Z(LZI) )
             DO 11  J  =  1, N
                SUM  =  F(X(LXIJ),Y(LYJ),SUM)
                LXIJ =  LXIJ + JX
                LYJ  =  LYJ + JY
  11            CONTINUE
             Z(LZI)  =  SUM
             LXI1    =  LXI1 + IX
             LZI     =  LZI + IZ
  12         CONTINUE
          RETURN
          END
